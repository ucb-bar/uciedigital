package edu.berkeley.cs.ucie.digital
package tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import e2e._
import protocol._
import interfaces._
import sideband._
import logphy._

class UcieDigitalTopIO(mbLanes: Int = 16, STANDALONE: Boolean = true)
    extends Bundle {
  //  FDI interface for testing purposes only
  // val fdi = new Fdi(fdiParams)
  // IOs for connecting to the AFE
  val mbSerializerRatio = if (STANDALONE) 16 else 32
  val afeParams =
    AfeParams(mbSerializerRatio = mbSerializerRatio, STANDALONE = STANDALONE)
  val linkTrainingParams = LinkTrainingParams()
  val mbAfe_tx = if (STANDALONE) Some(Output(new MainbandIo(mbLanes))) else None
  val mbAfe_rx = if (STANDALONE) Some(Input(new MainbandIo(mbLanes))) else None
  val phyAfe =
    if (STANDALONE) None else Some(new MainbandLaneIOWithValid(afeParams))
  val rxSbAfe = Input(new SidebandIo())
  val txSbAfe = Output(new SidebandIo())
  val train =
    if (STANDALONE) None
    else
      Some(new TrainingOperation(afeParams, linkTrainingParams.maxPatternCount))
  // val mbAfe = new MainbandAfeIo(afeParams)
  // val sbAfe = new SidebandAfeIo(afeParams)
}

// TODO: Sideband messaging
/** Main class to generate manager, client and register nodes on the tilelink
  * diplomacy. These needs to get connected to the chipyard system. The class
  * converts tilelink packets to UCIe Raw 64B flit. It also instantiates the
  * protocol layer which acts as an agnostic interface to generate FDI
  * signalling.
  */
class UCITLFront(
    val tlParams: TileLinkParams,
    val protoParams: ProtocolLayerParams,
    val fdiParams: FdiParams,
    val rdiParams: RdiParams,
    val sbParams: SidebandParams,
    val linkTrainingParams: LinkTrainingParams,
    val afeParams: AfeParams,
    val laneAsyncQueueParams: AsyncQueueParams,
)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {

      println(s"PLUM\n tlParams.dataWidth: ${tlParams.dataWidth}")

      val device = new SimpleDevice("ucie-front", Seq("ucie,ucie0"))

  val beatBytes = tlParams.BEAT_BYTES

  // MMIO registers controlled by the sideband module
  val regNode = LazyModule(
    new UCIConfigRF(
      beatBytes = tlParams.CONFIG_BEAT_BYTES,
      address = tlParams.CONFIG_ADDRESS,
    ),
  )

  // Manager node to send and acquire traffic to partner die
  val managerNode: TLManagerNode = TLManagerNode(
    Seq(
      TLSlavePortParameters.v1(
        Seq(
          TLSlaveParameters.v1(
            address = Seq(AddressSet(tlParams.ADDRESS, tlParams.ADDR_RANGE)),
            resources = device.reg,
            regionType =
              RegionType.UNCACHED, // Should be changed to CACHED eventually
            executable = true,
            supportsGet = TransferSizes(1, beatBytes),
            supportsPutFull = TransferSizes(1, beatBytes),
            supportsPutPartial = TransferSizes(1, beatBytes),
            fifoId = Some(0),
          ),
        ),
        beatBytes = beatBytes,
      ),
    ),
  )

  // Client node to reply to send and acquire traffic from partner die
  val clientNode: TLClientNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        Seq(
          TLMasterParameters.v1(
            name = "ucie-client",
            sourceId = IdRange(0, 1 << tlParams.sourceIDWidth),
            requestFifo = true,
            visibility = Seq(AddressSet(tlParams.ADDRESS, tlParams.ADDR_RANGE)),
          ),
        ),
      ),
    ),
  )

  /* val topIO = BundleBridgeSource(() => new
   * UcieDigitalTopIO(afeParams.mbLanes)) */

  override lazy val module = new UCITLFrontImp

  class UCITLFrontImp extends Impl {
    val io = IO(new UcieDigitalTopIO(afeParams.mbLanes, afeParams.STANDALONE))
    // new Bundle {
    //  FDI interface for testing purposes only
    // val fdi = new Fdi(fdiParams)
    // IOs for connecting to the AFE
    // val mbAfe = new MainbandAfeIo(afeParams)
    //  val mbAfe_tx = Output(new MainbandIo(afeParams.mbLanes))
    //  val mbAfe_rx = Input (new MainbandIo(afeParams.mbLanes))
    //  val sbTxIo = Output(new SidebandIo)
    //  val sbRxIo = Input(new SidebandIo)
    // })

    withClockAndReset(clock, reset) {

      val fault = RegInit(false.B) // if fault in ecc code

      // Instantiate the agnostic protocol layer
      // val protocol = Module(new ProtocolLayer(fdiParams))
      val ucietop = Module(
        new UCITop(
          fdiParams,
          rdiParams,
          sbParams,
          linkTrainingParams,
          afeParams,
          laneAsyncQueueParams,
        ),
      )
      // io.fdi <> ucietop.io.fdi
      ucietop.io.fault := fault

      if (afeParams.STANDALONE) {
        io.mbAfe_tx.get <> ucietop.io.mbAfe_tx.get
        io.mbAfe_rx.get <> ucietop.io.mbAfe_rx.get
      } else {
        io.phyAfe.get <> ucietop.io.phyAfe.get
        io.train.get <> ucietop.io.train.get
      }
      io.txSbAfe <> ucietop.io.sbTxIO
      io.rxSbAfe <> ucietop.io.sbRxIO
      // dontTouch(topIO.out(0)._1)
      // topIO.out(0)._1.mbAfe_tx <> ucietop.io.mbAfe_tx
      // topIO.out(0)._1.mbAfe_rx <> ucietop.io.mbAfe_rx
      // topIO.out(0)._1.rxSbAfe <> ucietop.io.sbRxIO
      // topIO.out(0)._1.txSbAfe <> ucietop.io.sbTxIO

      // Hamming encode and decode
      val hammingEncoder = Module(new HammingEncode(protoParams))
      val hammingDecoder = Module(new HammingDecode(protoParams))
      // Defaults
      hammingEncoder.io.data := 0.U
      hammingDecoder.io.data := 0.U
      hammingDecoder.io.checksum := 0.U

      // Sideband node for protocol layer
      val protocol_sb_node =
        Module(new SidebandNode((new SidebandParams), fdiParams))

      protocol_sb_node.io.outer.rx.bits := ucietop.io.fdi_lpConfig.bits
      protocol_sb_node.io.outer.rx.valid := ucietop.io.fdi_lpConfig.valid
      ucietop.io.fdi_lpConfigCredit := protocol_sb_node.io.outer.rx.credit

      ucietop.io.fdi_plConfig.bits := protocol_sb_node.io.outer.tx.bits
      ucietop.io.fdi_plConfig.valid := protocol_sb_node.io.outer.tx.valid
      protocol_sb_node.io.outer.tx.credit := ucietop.io.fdi_plConfigCredit

      protocol_sb_node.io.inner.layer_to_node.bits := Cat(
        regNode.module.io.sb_csrs.sideband_mailbox_sw_to_node_data_high,
        regNode.module.io.sb_csrs.sideband_mailbox_sw_to_node_data_low,
        regNode.module.io.sb_csrs.sidebank_mailbox_sw_to_node_index_high,
        regNode.module.io.sb_csrs.sideband_mailbox_sw_to_node_index_low,
      )
      protocol_sb_node.io.inner.layer_to_node.valid := regNode.module.io.sb_csrs.sideband_mailbox_sw_valid

      regNode.module.io.sb_csrs.sideband_mailbox_index_low := protocol_sb_node.io.inner.node_to_layer
        .bits(31, 0)
      regNode.module.io.sb_csrs.sideband_mailbox_index_high := protocol_sb_node.io.inner.node_to_layer
        .bits(63, 32)
      regNode.module.io.sb_csrs.sideband_mailbox_data_low := protocol_sb_node.io.inner.node_to_layer
        .bits(95, 64)
      regNode.module.io.sb_csrs.sideband_mailbox_data_high := protocol_sb_node.io.inner.node_to_layer
        .bits(127, 96)
      protocol_sb_node.io.inner.node_to_layer.ready := regNode.module.io.sb_csrs.sideband_mailbox_ready
      regNode.module.io.sb_csrs.sideband_mailbox_valid := protocol_sb_node.io.inner.node_to_layer.valid

      val tlBundleParams = new TLBundleParameters(
        addressBits = tlParams.addressWidth,
        dataBits = tlParams.dataWidth,
        sourceBits = tlParams.sourceIDWidth,
        sinkBits = tlParams.sinkIDWidth,
        sizeBits = tlParams.sizeWidth,
        echoFields = Nil,
        requestFields = Nil,
        responseFields = Nil,
        hasBCE = false,
      )

      println(s"tlParams.dataWidth: ${tlParams.dataWidth}")
      println(s"tlBundleParams.dataWidth: ${tlBundleParams.dataBits}")

      val client_tl = clientNode.out(0)._1
      val client_edge = clientNode.out(0)._2
      val manager_tl = managerNode.in(0)._1
      val manager_edge = managerNode.in(0)._2

      val clientParams = client_edge.bundle
      val managerParams = manager_edge.bundle
      val mergedParams = clientParams.union(managerParams).union(tlBundleParams)

      println("KIWI")
      println(managerParams)
      println(tlBundleParams)

      require(
        mergedParams.echoFields.isEmpty,
        "UCIe does not support TileLink with echo fields",
      )
      require(
        mergedParams.requestFields.isEmpty,
        "UCIe does not support TileLink with request fields",
      )
      require(
        mergedParams.responseFields.isEmpty,
        "UCIe does not support TileLink with response fields",
      )
      require(
        mergedParams == tlBundleParams,
        s"UCIe is misconfigured, the combined inwards/outwards parameters cannot be serialized using the provided bundle params\n$mergedParams > $tlBundleParams",
      )

      val inwardA = Module(
        new Queue((new TLBundleA(mergedParams)), 16, pipe = true, flow = true),
      )
      val inwardD = Module(
        new Queue((new TLBundleD(mergedParams)), 16, pipe = true, flow = true),
      )

      val outwardA =
        Module(new CreditedToDecoupledMsg(new TLBundleA(mergedParams), 4, 16))
      val outwardD =
        Module(new CreditedToDecoupledMsg(new TLBundleD(mergedParams), 4, 16))

      // =======================
      // TL TX packets from System to the UCIe stack, push on the inward queue.
      // The TX packets can be A request from manager node or D response from
      // the client node. This needs to be arbitrated to be sent to partner die.
      // =======================
      val txArbiter = Module(new Arbiter(new TLBundleAUnionD(tlParams), 2))
      val txATLPayload = Wire(new TLBundleAUnionD(tlParams))
      val txDTLPayload = Wire(new TLBundleAUnionD(tlParams))
      // val txTLPayload = Wire(new TLBundleAUnionD(tlParams))

      val aHasData = manager_edge.hasData(manager_tl.a.bits)
      val rx_fire = ucietop.io.TLplData_valid
      val uciRxPayload = Wire(
        new UCIRawPayloadFormat(tlParams, protoParams),
      ) // User-defined UCIe flit for streaming
      val uciTxPayload = Wire(
        new UCIRawPayloadFormat(tlParams, protoParams),
      ) // User-defined UCIe flit for streaming

      // defaults
      uciRxPayload := 0.U.asTypeOf(
        new UCIRawPayloadFormat(tlParams, protoParams),
      )
      uciTxPayload := 0.U.asTypeOf(
        new UCIRawPayloadFormat(tlParams, protoParams),
      )
      txATLPayload := 0.U.asTypeOf(new TLBundleAUnionD(tlParams))
      txDTLPayload := 0.U.asTypeOf(new TLBundleAUnionD(tlParams))

      /* manager_tl.a.ready = (inward.io.enq.ready & ~ucietop.io.fdi_lpStallAck
       * & (ucietop.io.TLplStateStatus === PhyState.active)) inward.io.enq.valid
       * := manager_tl.a.fire */

      // A request to partner die logic
      // enqueue on the A channel queue
      manager_tl.a.ready := (inwardA.io.enq.ready & ~ucietop.io.fdi_lpStallAck &
        (ucietop.io.TLplStateStatus === PhyState.active))
      inwardA.io.enq.valid := manager_tl.a.fire
      inwardA.io.enq.bits <> manager_tl.a.bits

      val creditedMsgA =
        Module(new DecoupledtoCreditedMsg(new TLBundleA(mergedParams), 4, 16))
      inwardA.io.deq.ready := creditedMsgA.io.in.ready
      creditedMsgA.io.in.valid := inwardA.io.deq.fire
      creditedMsgA.io.in.bits := inwardA.io.deq.bits
      creditedMsgA.io.credit.valid := rx_fire
      creditedMsgA.io.credit.bits := uciRxPayload.cmd.tlACredit

      // D response to partner die's A request logic
      client_tl.d.ready := (inwardD.io.enq.ready & ~ucietop.io.fdi_lpStallAck &
        (ucietop.io.TLplStateStatus === PhyState.active))
      inwardD.io.enq.valid := client_tl.d.fire
      inwardD.io.enq.bits <> client_tl.d.bits

      val creditedMsgD =
        Module(new DecoupledtoCreditedMsg(new TLBundleD(mergedParams), 4, 16))
      inwardD.io.deq.ready := creditedMsgD.io.in.ready
      creditedMsgD.io.in.valid := inwardD.io.deq.fire
      creditedMsgD.io.in.bits := inwardD.io.deq.bits
      creditedMsgD.io.credit.valid := rx_fire
      creditedMsgD.io.credit.bits := uciRxPayload.cmd.tlDCredit

      // Arbitrate the A and D channels from the credited msgs
      creditedMsgA.io.out.ready := txArbiter.io.in(0).ready
      txArbiter.io.in(0).valid := creditedMsgA.io.out.fire
      /* txArbiter.io.in(0).bits <> creditedMsgA.io.out.bits.asTypeOf(new
       * TLBundleAUnionD(tlParams)) */
      txArbiter.io.in(0).bits.opcode := creditedMsgA.io.out.bits.opcode
      txArbiter.io.in(0).bits.param := creditedMsgA.io.out.bits.param
      txArbiter.io.in(0).bits.size := creditedMsgA.io.out.bits.size
      txArbiter.io.in(0).bits.source := creditedMsgA.io.out.bits.source
      txArbiter.io.in(0).bits.sink := 0.U
      txArbiter.io.in(0).bits.address := creditedMsgA.io.out.bits.address
      txArbiter.io.in(0).bits.mask := creditedMsgA.io.out.bits.mask
      txArbiter.io.in(0).bits.data := creditedMsgA.io.out.bits.data
      txArbiter.io.in(0).bits.msgType := UCIProtoMsgTypes.TLA

      creditedMsgD.io.out.ready := txArbiter.io.in(1).ready
      txArbiter.io.in(1).valid := creditedMsgD.io.out.fire
      /* txArbiter.io.in(1).bits <> creditedMsgD.io.out.bits.asTypeOf(new
       * TLBundleAUnionD(tlParams)) */
      txArbiter.io.in(1).bits.opcode := creditedMsgD.io.out.bits.opcode
      txArbiter.io.in(1).bits.param := creditedMsgD.io.out.bits.param
      txArbiter.io.in(1).bits.size := creditedMsgD.io.out.bits.size
      txArbiter.io.in(1).bits.source := creditedMsgD.io.out.bits.source
      txArbiter.io.in(1).bits.sink := creditedMsgD.io.out.bits.sink
      txArbiter.io.in(1).bits.address := 0.U
      txArbiter.io.in(1).bits.mask := 0.U
      txArbiter.io.in(1).bits.data := creditedMsgD.io.out.bits.data
      txArbiter.io.in(1).bits.msgType := UCIProtoMsgTypes.TLD

      /* ============== Translated TL packet coming out of the outward queue to
       * the system ======== */
      // dequeue the rx TL packets and orchestrate on the client/manager node
      // val isRequest = outwardA.io.deq.bits.msgType
      // val isResponse = outwardD.io.deq.bits.msgType

      outwardA.io.out.ready := client_tl.a.ready // if A request send to client node
      outwardD.io.out.ready := manager_tl.d.ready // if D response send to manager node

      client_tl.a.bits <> outwardA.io.out.bits
      client_tl.a.valid := outwardA.io.out.fire

      manager_tl.d.bits <> outwardD.io.out.bits
      manager_tl.d.valid := outwardD.io.out.fire

      // ============ Below code should run on the UCIe clock? ==============
      val checksum_reg = RegInit(0.U(64.W))
      checksum_reg := hammingEncoder.io.checksum

      val tx_pipe =
        Module(new Queue(new UCIRawPayloadFormat(tlParams, protoParams), 1))
      tx_pipe.io.enq.bits := uciTxPayload
      tx_pipe.io.enq.valid := txArbiter.io.out.fire
      // Dequeue the TX TL packets and translate to UCIe flit
      txArbiter.io.out.ready := tx_pipe.io.enq.ready
      tx_pipe.io.deq.ready := ucietop.io.TLlpData_ready // if pl_trdy is asserted
      // specs implies that these needs to be asserted at the same time
      ucietop.io.TLlpData_valid := tx_pipe.io.deq.fire & (~ucietop.io.fdi_lpStallAck)
      ucietop.io.TLlpData_irdy := tx_pipe.io.deq.fire & (~ucietop.io.fdi_lpStallAck)
      ucietop.io.TLlpData_bits := Cat(
        tx_pipe.io.deq.bits.asUInt(511, 64),
        checksum_reg.asUInt,
      ) // assign uciTXPayload to the FDI lp data signa

      val creditA = (txArbiter.io.out.bits.msgType === UCIProtoMsgTypes.TLA)
      val creditB = (txArbiter.io.out.bits.msgType === UCIProtoMsgTypes.TLB)
      val creditC = (txArbiter.io.out.bits.msgType === UCIProtoMsgTypes.TLC)
      val creditD = (txArbiter.io.out.bits.msgType === UCIProtoMsgTypes.TLD)
      val creditE = (txArbiter.io.out.bits.msgType === UCIProtoMsgTypes.TLE)

      outwardA.io.credit.ready := tx_pipe.io.deq.fire && creditA
      outwardD.io.credit.ready := tx_pipe.io.deq.fire && creditD

      val txACredit = WireDefault(0.U(protoParams.creditWidth.W))
      val txDCredit = WireDefault(0.U(protoParams.creditWidth.W))

      when(outwardA.io.credit.valid) {
        txACredit := outwardA.io.credit.bits
      }.elsewhen(outwardD.io.credit.valid) {
        txDCredit := outwardD.io.credit.bits
      }

      // Translation based on the uciPayload formatting from outgoing TL packet
      when(txArbiter.io.out.fire) {
        // form the cmd header with TL message type
        uciTxPayload.cmd.msgType := txArbiter.io.out.bits.msgType
        uciTxPayload.cmd.hostID := 0.U
        uciTxPayload.cmd.partnerID := 0.U
        uciTxPayload.cmd.tlACredit := txACredit
        uciTxPayload.cmd.tlBCredit := 0.U // & outwardB.io.credit.valid & creditB
        uciTxPayload.cmd.tlCCredit := 0.U // & outwardC.io.credit.valid & creditC
        uciTxPayload.cmd.tlDCredit := txDCredit
        uciTxPayload.cmd.tlECredit := 0.U // & outwardE.io.credit.valid & creditE
        uciTxPayload.cmd.reservedCmd := 0.U
        // header 1
        uciTxPayload.header1.address := txArbiter.io.out.bits.address
        // header 2
        uciTxPayload.header2.opcode := txArbiter.io.out.bits.opcode
        uciTxPayload.header2.param := txArbiter.io.out.bits.param
        uciTxPayload.header2.size := txArbiter.io.out.bits.size
        uciTxPayload.header2.source := txArbiter.io.out.bits.source
        uciTxPayload.header2.sink := txArbiter.io.out.bits.sink
        uciTxPayload.header2.mask := txArbiter.io.out.bits.mask
        uciTxPayload.header2.reservedh2 := 0.U
        // uciTxPayload.header2.denied  := txArbiter.io.out.bits.denied
        // uciTxPayload.header2.corrupt := txArbiter.io.out.bits.corrupt
        // data mapping
        // TODO: replace with FP coding
        uciTxPayload.data(0) := txArbiter.io.out.bits.data(63, 0)
        uciTxPayload.data(1) := txArbiter.io.out.bits.data(127, 64)
        uciTxPayload.data(2) := txArbiter.io.out.bits.data(191, 128)
        uciTxPayload.data(3) := txArbiter.io.out.bits.data(255, 192)
        // TODO: add ECC/checksum functionality, for now tieing to 0
        hammingEncoder.io.data := uciTxPayload.asUInt(511, 64)
        uciTxPayload.ecc := 0.U
      }

      // =======================
      /* TL RX packets coming from the UCIe stack to the System, push on the
       * outward queue */
      // =======================
      val rxTLPayload = Wire(new TLBundleAUnionD(tlParams))
      rxTLPayload := 0.U.asTypeOf(new TLBundleAUnionD(tlParams))
      // ucietop.io.fdi_lpData.irdy := outward.io.enq.ready
      // map the uciRxPayload and the plData based on the uciPayload formatting
      // map the uciRxPayload to the rxTLPayload TLBundle
      when(rx_fire) {
        // ucie cmd
        uciRxPayload.cmd := ucietop.io
          .TLplData_bits(511, 448)
          .asTypeOf(new UCICmdFormat(protoParams))
        // ucie header 1
        uciRxPayload.header1 := ucietop.io
          .TLplData_bits(447, 384)
          .asTypeOf(new UCIHeader1Format(tlParams))
        // ucie header 2
        uciRxPayload.header2 := ucietop.io
          .TLplData_bits(383, 320)
          .asTypeOf(new UCIHeader2Format(tlParams))
        // ucie data payload
        uciRxPayload.data(3) := ucietop.io.TLplData_bits(319, 256)
        uciRxPayload.data(2) := ucietop.io.TLplData_bits(255, 192)
        uciRxPayload.data(1) := ucietop.io.TLplData_bits(191, 128)
        uciRxPayload.data(0) := ucietop.io.TLplData_bits(127, 64)
        // ucie ecc
        uciRxPayload.ecc := ucietop.io.TLplData_bits(63, 0)
        hammingDecoder.io.data := ucietop.io.TLplData_bits(511, 64)
        hammingDecoder.io.checksum := ucietop.io.TLplData_bits(63, 0)

        // map the uciRxPayload to the rxTLPayload
        rxTLPayload.address := uciRxPayload.header1.address
        rxTLPayload.opcode := uciRxPayload.header2.opcode
        rxTLPayload.param := uciRxPayload.header2.param
        rxTLPayload.size := uciRxPayload.header2.size
        rxTLPayload.source := uciRxPayload.header2.source
        rxTLPayload.sink := uciRxPayload.header2.sink
        rxTLPayload.mask := uciRxPayload.header2.mask
        // rxTLPayload.denied  := uciRxPayload.header2.denied
        // rxTLPayload.corrupt := uciRxPayload.header2.corrupt
        rxTLPayload.data := Cat(
          uciRxPayload.data(0),
          uciRxPayload.data(1),
          uciRxPayload.data(2),
          uciRxPayload.data(3),
        )
        rxTLPayload.msgType := uciRxPayload.cmd.msgType
      }
      fault := ~(hammingDecoder.io.matches)

      outwardA.io.in.bits.address := rxTLPayload.address
      outwardA.io.in.bits.opcode := rxTLPayload.opcode
      outwardA.io.in.bits.param := rxTLPayload.param
      outwardA.io.in.bits.size := rxTLPayload.size
      outwardA.io.in.bits.source := rxTLPayload.source
      outwardA.io.in.bits.mask := rxTLPayload.mask
      outwardA.io.in.bits.data := rxTLPayload.data
      outwardA.io.in.bits.corrupt := false.B
      outwardA.io.in.valid := false.B

      outwardD.io.in.bits.opcode := rxTLPayload.opcode
      outwardD.io.in.bits.param := rxTLPayload.param
      outwardD.io.in.bits.size := rxTLPayload.size
      outwardD.io.in.bits.source := rxTLPayload.source
      outwardD.io.in.bits.sink := rxTLPayload.sink
      outwardD.io.in.bits.data := rxTLPayload.data
      outwardD.io.in.bits.denied := false.B
      outwardD.io.in.bits.corrupt := false.B
      outwardD.io.in.valid := false.B

      // Queue the translated RX TL packet to send to the system
      when(rx_fire) {
        when(
          uciRxPayload.cmd.msgType === UCIProtoMsgTypes.TLA && outwardA.io.in.ready,
        ) {
          outwardA.io.in.valid := true.B
        }.elsewhen(
          uciRxPayload.cmd.msgType === UCIProtoMsgTypes.TLD && outwardD.io.in.ready,
        ) {
          outwardD.io.in.valid := true.B
        }
      }
      // when the RX queues are ready to get data
      val TLready_to_rcv = outwardA.io.in.ready || outwardD.io.in.ready
      ucietop.io.TLready_to_rcv := TLready_to_rcv

      /* soft resets: can be reset or flush and reset, in flush and reset, the
       * packets are */
      // sent out before triggering reset
      ucietop.io.soft_reset := (regNode.module.io.d2d_csrs.d2d_state_can_reset |
        regNode.module.io.d2d_csrs.d2d_flush_and_reset)
    }
  }
}
