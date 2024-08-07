package edu.berkeley.cs.ucie.digital
package protocol

import chisel3._
import chisel3.util._

import interfaces._
import javax.swing.InputMap

/**
  * Class to handle the FDI signalling between the D2D adapter and protocol layer. The class
  * interacts with the tilelink front to get TL packets translated to UCIe flit and sent over
  * the FDI signals to the D2D adapter. It also instantiates sideband node to orchestrate
  * register access from system over the SB messaging. Finally, it handles the auxillary
  * signalling required for link initialization and link managements.
  * @param fdiParams
  * @param protoParams
  */
class ProtocolLayer(val fdiParams: FdiParams) extends Module {
    val io = IO(new Bundle{
        val fdi = new Fdi(fdiParams)
        val TLplStateStatus = Output(PhyState())
        val TLlpData_valid = Input(Bool())
        val TLlpData_bits = Input(Bits((8 * fdiParams.width).W)) 
        val TLlpData_irdy = Input(Bool())
        val TLlpData_ready = Output(Bool())
        val TLplData_bits = Output(Bits((8 * fdiParams.width).W))
        val TLplData_valid = Output(Bool())
        val TLready_to_rcv = Input(Bool())
        val fault = Input(Bool())
        val soft_reset = Input(Bool())
    })

    io.TLplStateStatus := io.fdi.plStateStatus

    io.fdi.lpData.bits := io.TLlpData_bits
    io.fdi.lpData.valid := io.TLlpData_valid
    io.fdi.lpData.irdy := io.TLlpData_irdy
    io.TLlpData_ready := io.fdi.lpData.ready

    io.TLplData_valid := io.fdi.plData.valid
    io.TLplData_bits := io.fdi.plData.bits

    // Constants for the FDI signals not used in v1
    io.fdi.lpRetimerCrd := false.B
    io.fdi.lpCorruptCrc := false.B
    io.fdi.lpDllp.valid := false.B
    io.fdi.lpDllp.bits := 0.U
    io.fdi.lpDllpOfc := false.B
    // Dynamic clock gating feature not supported in v1
    io.fdi.lpClkAck := true.B
    io.fdi.lpWakeReq := true.B

    // Tie lpStream to streaming protocol on stack 0
    val streaming = Wire(new ProtoStream())
    streaming.protoStack := ProtoStack.stack0
    streaming.protoType := ProtoStreamType.Stream
    io.fdi.lpStream <> streaming

    val sb_linkReset_req = io.soft_reset

    // Refer to section 8.2.7 for rx_active_req/sts handshake
    val lp_rx_active_sts_reg = RegInit(false.B)
    // lpRxActiveStatus can change before the plStateStatus becomes Active
    // val lp_rx_active_pl_state = (io.fdi.plStateStatus === PhyState.reset ||
    //                              io.fdi.plStateStatus === PhyState.retrain ||
    //                              io.fdi.plStateStatus === PhyState.active)
    val lp_rx_active_pl_state = (io.fdi.plStateStatus === PhyState.active)

    when(io.fdi.plRxActiveReq && io.TLready_to_rcv){//&& lp_rx_active_pl_state) {
        lp_rx_active_sts_reg := true.B
    }
    io.fdi.lpRxActiveStatus := lp_rx_active_sts_reg

    // Refer to section 8.2.8 for FDI bringup and state req logic
    val lp_state_req_reg = RegInit(PhyStateReq.nop)
    //val lp_state_req_prev = RegInit(PhyState.nop)
    //lp_state_req_prev := io.fdi.plStateStatus

    // Removed: lp_state_req_prev === PhyState.nop &
    val reqActive = ((io.fdi.plStateStatus === PhyState.reset &
                    io.fdi.plInbandPres) || io.fdi.plStateStatus === PhyState.linkReset)

    when(reqActive) {
        lp_state_req_reg := PhyStateReq.active
    }.elsewhen(~reqActive && sb_linkReset_req) {// TODO: SB register to initiate LinkReset
        lp_state_req_reg := PhyStateReq.linkReset
    }.otherwise { lp_state_req_reg := PhyStateReq.nop }

    io.fdi.lpStateReq := lp_state_req_reg

    // lpLinkError should be asserted when there is an error detected by protocol layer
    // should be done as a ECC check in UCIe flit
    io.fdi.lpLinkError := io.fault

    // Refer to section 8.3.2
    // Whent he lpStallAck is asserted the TL A channel is stalled and the lp irdy and valid
    // signals are deasserted in the UCITLFront class.
    val lp_stall_reg = RegInit(false.B)
    lp_stall_reg := io.fdi.plStallReq
    io.fdi.lpStallAck := lp_stall_reg

    /** pl_protocol
      * saves pl_protocol and flitfmt, resets and negotiates if protocol/flitfmt is not raw
      * Expects ProtocolID.STREAM and ProtocolFlitFmt.RAW if implemented
      * WARNING: HAS NOT IMPLEMENTED RENEGOTIATION LOGIC, the d2d ties these to raw
      */
    val pl_protocol_reg = RegInit(0.U(3.W))
    val pl_protocol_flitfmt_reg = RegInit(0.U(4.W))
    when (io.fdi.plProtocolValid) {
      pl_protocol_reg := io.fdi.plProtocol.asUInt
      pl_protocol_flitfmt_reg := io.fdi.plProtocolFlitFormat.asUInt
    }

    // TODO: these are SB messaging signals
    io.fdi.lpConfig.bits := 0.asUInt(fdiParams.sbWidth.W)
    io.fdi.lpConfig.valid := false.B
    //io.fdi.lpConfigCredit := false.B
    //io.fdi.lpConfig
    io.fdi.plConfigCredit := false.B
}