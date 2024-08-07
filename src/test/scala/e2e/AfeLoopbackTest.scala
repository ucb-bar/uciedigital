package edu.berkeley.cs.ucie.digital
package e2e

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.{Field, Parameters, Config}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import freechips.rocketchip.diplomacy._
import chisel3.experimental.BundleLiterals._

// import freechips.rocketchip.unittest._
import org.scalatest.flatspec.AnyFlatSpec
import tilelink._
import sideband.{SidebandParams}
import logphy.{LinkTrainingParams}
import interfaces._
import protocol.{ProtocolLayerParams}
import java.util.ResourceBundle

class AfeLoopbackTester(implicit p: Parameters) extends LazyModule {
  val protoParams = ProtocolLayerParams()
  val tlParams = TileLinkParams(
    address = 0x0,
    addressRange = 0xffff,
    configAddress = 0x40000,
    inwardQueueDepth = 8,
    outwardQueueDepth = 8,
  )
  val fdiParams = FdiParams(width = 64, dllpWidth = 64, sbWidth = 32)
  val rdiParams = RdiParams(width = 64, sbWidth = 32)
  val sbParams = SidebandParams()
  val myId = 1
  val linkTrainingParams = LinkTrainingParams()
  val afeParams = AfeParams()
  val laneAsyncQueueParams = AsyncQueueParams()
  val delay = 0.0
  val txns = 100

  // Create clock source
  val clockSourceNode = ClockSourceNode(Seq(ClockSourceParameters()))

  val csrfuzz = LazyModule(new TLFuzzer(txns))
  val fuzz = LazyModule(new TLFuzzer(txns))
  val tlUcieDie1 = LazyModule(
    new UCITLFront(
      tlParams = tlParams,
      protoParams = protoParams,
      fdiParams = fdiParams,
      rdiParams = rdiParams,
      sbParams = sbParams,
      linkTrainingParams = linkTrainingParams,
      afeParams = afeParams,
      laneAsyncQueueParams = laneAsyncQueueParams,
    ),
  )
  tlUcieDie1.clockNode := clockSourceNode
  val ram = LazyModule(
    new TLRAM(
      AddressSet(tlParams.ADDRESS, tlParams.addressRange),
      beatBytes = tlParams.BEAT_BYTES,
    ),
  )

  // CSR node
  tlUcieDie1.regNode.node := csrfuzz.node
  // connect data nodes
  tlUcieDie1.managerNode := TLSourceShrinker(
    tlParams.sourceIDWidth,
  ) := fuzz.node
  ram.node := tlUcieDie1.clientNode
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val uci_clock = Input(new ClockBundle(ClockBundleParameters()))
      val finished = Output(Bool())
    })
    // connect IOs
    io.finished := fuzz.module.io.finished
    val AfeLoopback = Module(new AfeLoopback(afeParams))
    io.uci_clock <> clockSourceNode.out(0)._1
    // inputs to tlUcieDie1
    // tlUcieDie1.module.io.mbAfe <> AfeLoopback.io.mbAfe
    tlUcieDie1.module.io.mbAfe_tx.get <> AfeLoopback.io.mbAfe_tx
    tlUcieDie1.module.io.mbAfe_rx.get <> AfeLoopback.io.mbAfe_rx
    tlUcieDie1.module.io.txSbAfe <> AfeLoopback.io.sbAfe_tx
    tlUcieDie1.module.io.rxSbAfe <> AfeLoopback.io.sbAfe_rx

  }
}

class AfeTLTestHarness(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle { val success = Output(Bool()) })
  val tester = Module(LazyModule(new AfeLoopbackTester).module)
  tester.io.uci_clock.clock := clock
  tester.io.uci_clock.reset := reset
  io.success := tester.io.finished

  // Dummy plusarg to avoid breaking verilator builds with emulator.cc
  val useless_plusarg = PlusArg("useless_plusarg", width = 1)
  dontTouch(useless_plusarg)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
}

class AfeLoopbackTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AfeLoopback"
  val txns = 2
  val timeout = 100000
  implicit val p: Parameters = Parameters.empty
  it should "finish request and response before timeout" in {
    test(new AfeTLTestHarness()).withAnnotations(
      Seq(VcsBackendAnnotation, WriteFsdbAnnotation),
    ) { c => // .withAnnotations(Seq(VcsBackendAnnotation, WriteVcdAnnotation))

      println("start Afe Loopback Test")
      c.reset.poke(true.B)
      c.clock.step(3)
      c.reset.poke(false.B)
      c.clock.setTimeout(timeout + 10)
      c.clock.step(timeout)
      c.io.success.expect(true.B)
      println("Afe Loopback Test finished? " + c.io.success.peek())
    }
  }
}
