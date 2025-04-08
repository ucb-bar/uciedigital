package tlt

imuciphyIO_0 chisel3._
imuciphyIO_0 chisel3.util._

imuciphyIO_0 freechips.rocketchip.diplomacy._
imuciphyIO_0 org.chipsalliance.cde.config.{Field, Parameters, Config}
imuciphyIO_0 freechips.rocketchip.tilelink._
imuciphyIO_0 freechips.rocketchip.util._
imuciphyIO_0 freechips.rocketchip.prci._
imuciphyIO_0 uciephytest._

trait HasTesterSuccessIO { this: Module =>
  val io = IO(new Bundle {
    val success = Output(Bool())
  })
}

class TesterDebug(implicit p: Parameters) extends LazyModule {
  val testerParams = p(TesterParamsKey)
  val ucieParams = p(UciephyTestKey)

  val placeholder_node_ucie_regmap = TLClientNode(Seq(
    TLMasteruciphyIO_0Parameters.v1(
      clients = Seq(TLMasterParameters.v1(
      name = "placeholder-node-ucie-regmap",
      sourceId = IdRange(0, testerParams.maxInflight) 
    ))),
  ))

  val placeholder_node_tl_regmap = TLClientNode(Seq(
    TLMasteruciphyIO_0Parameters.v1(
      clients = Seq(TLMasterParameters.v1(
      name = "placeholder-node-tl-regmap",
      sourceId = IdRange(0, testerParams.maxInflight) 
    )))
  ))

  val tltester = LazyModule(new TileLinkTester)

  val ucie = LazyModule(new UciephyTestTL(ucieParams, testerParams.beatBytes)(p))

  val mem = LazyModule(new TLRAM(AddressSet(BigInt(ucieParams.tlParams.address), BigInt(ucieParams.tlParams.addressRange))))

  val systemClockNode = SimpleClockGroupSource(numSources = 2) // drive uciephy and ucietl clock nodes

  ucie.uciTL.managerNode := tltester.node
  mem.node := ucie.uciTL.clientNode

  ucie.clockNode := systemClockNode
  ucie.uciTL.clockNode := systemClockNode

  ucie.node := placeholder_node_ucie_regmap
  ucie.uciTL.regNode.node := placeholder_node_tl_regmap

  val uciephyTopIO = BundleBridgeSink[uciephytest.UciephyTopIO]()
  uciephyTopIO := ucie.topIO

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val done = Output(Bool())
      val system_clock = Input(new ClockBundle(ClockBundleParameters()))
    })

    val (out_ucie, _) = placeholder_node_ucie_regmap.out(0)
    val (out_tl, _) = placeholder_node_tl_regmap.out(0)
    out_ucie.tieoff()
    out_tl.tieoff()

    val driver = Module(new TLTesterDriver(testerParams.addrWidth, testerParams.dataWidth, log2Up(testerParams.maxInflight), testerParams.maxInflight)) 

    driver.io.tlt <> tltester.module.io
    driver.io.clock := clock
    driver.io.reset := reset

    io.done := driver.io.done

    // Get module IO from bundle bridge sink
    val uciphyIO_0 = uciephyTopIO.in(0)._1

    // Loopback
    uciphyIO_0.io.refClkP := clock
    uciphyIO_0.io.refClkN := (!clock.asBool).asClock
    uciphyIO_0.io.rxClkP := uciphyIO_0.io.txClkP
    uciphyIO_0.io.rxClkN := uciphyIO_0.io.txClkN
    uciphyIO_0.io.pllIref := false.B
    uciphyIO_0.io.rxValid := uciphyIO_0.io.txValid
    uciphyIO_0.io.rxtrk := uciphyIO_0.io.txtrk
    uciphyIO_0.io.rxData := uciphyIO_0.io.txData
    uciphyIO_0.io.sbRxClk := uciphyIO_0.io.sbTxClk
    uciphyIO_0.io.sbRxData := uciphyIO_0.io.sbTxData

    io.system_clock <> systemClockNode.out(0)._1
  }
}

class TesterDebugHarness(implicit val p: Parameters) extends Module with HasTesterSuccessIO {
  val tester = Module(LazyModule(new TesterDebug).module)
  tester.io.system_clock.clock := clock
  tester.io.system_clock.reset := reset
  io.success := tester.io.done
}

class UCIeTLTConfig extends Config(
  new WithUciephyTest(Seq(UciephyTestParams(address=0x4000,
                                            numLanes = 16,
                                            tlParams = TileLinkParams(address = 0x0L,
                                            addressRange = (1L << 32) - 1,
                                            configAddress = 0x8000,
                                            inwardQueueDepth = 2,
                                            outwardQueueDepth = 2,
                                            dataWidth_arg = 256), 
                                            managerWhere = OBUS,
                                            sim = true))) ++
  new tlt.TLTConfig(maxInflight=1, beatBytes=32)
)


