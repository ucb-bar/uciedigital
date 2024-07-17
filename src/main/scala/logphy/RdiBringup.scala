package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import sideband.{SBM, SBMessage_factory}
import interfaces._

class RdiBringupIO extends Bundle {
  // Tie to 1 if clock gating not supported.
  val plClkReq = Output(Bool())
  val lpClkAck = Input(Bool())

  // Tie to 1 if clock gating not supported.
  val lpWakeReq = Input(Bool())
  val plWakeAck = Output(Bool())

  val lpStateReq = Input(PhyStateReq())
  val plStateStatus = Output(PhyState())

  val plStallReq = Output(Bool())
  val lpStallAck = Input(Bool())

  val lpLinkError = Input(Bool())
}

class RdiBringup extends Module {

  val io = IO(new Bundle {
    val rdiIO = new RdiBringupIO
    val sbTrainIO = Flipped(new SBMsgWrapperTrainIO)
    val active = Output(Bool())
    val internalError = Input(Bool())
    val internalRetrain = Input(Bool())
  })

  io.rdiIO.plClkReq := true.B
  io.rdiIO.plWakeAck := true.B

  private object ResetSubState extends ChiselEnum {
    val CLK_HANDSHAKE, LP_WAKE_HANDSHAKE, WAIT_LP_STATE_REQ, REQ_ACTIVE_SEND,
        REQ_ACTIVE_WAIT, RESP_ACTIVE_SEND, RESP_ACTIVE_WAIT = Value
  }

  private object StallReqAckState extends ChiselEnum {
    val ACTIVE, LP_STALLACK_WAIT, LP_STALLACK_DEASSERT = Value
  }

  private val state = RegInit(PhyState.reset)
  io.rdiIO.plStateStatus := state
  private val nextState = WireInit(state)
  io.active := state === PhyState.active
  io.sbTrainIO.msgReq.noenq()
  io.sbTrainIO.msgReqStatus.nodeq()
  state := nextState
  when(io.internalError || io.rdiIO.lpLinkError) {
    nextState := PhyState.linkError
  }.elsewhen(io.internalRetrain) {
    nextState := PhyState.retrain
  }

  private val resetSubstate = RegInit(ResetSubState.WAIT_LP_STATE_REQ)
  when(state =/= PhyState.reset && nextState === PhyState.reset) {
    resetSubstate := ResetSubState.WAIT_LP_STATE_REQ
  }

  private val stallReqAckState = RegInit(StallReqAckState.ACTIVE)
  when(state =/= PhyState.active && nextState === PhyState.active) {
    stallReqAckState := StallReqAckState.ACTIVE
  }

  io.rdiIO.plStallReq := stallReqAckState === StallReqAckState.LP_STALLACK_WAIT
  private val prevReq = RegInit(PhyStateReq.nop)
  prevReq := io.rdiIO.lpStateReq

  /** TODO: Implement Table 8-3 from spec */
  when(io.rdiIO.lpStateReq =/= PhyStateReq.nop) {
    when(state =/= PhyState.reset || prevReq === PhyStateReq.nop) {
      nextState := io.rdiIO.lpStateReq.asUInt.asTypeOf(PhyState())
    }
  }

  switch(state) {
    is(PhyState.reset) {
      switch(resetSubstate) {
        is(ResetSubState.WAIT_LP_STATE_REQ) {
          when(
            nextState === PhyState.active,
          ) {
            state := PhyState.reset
            resetSubstate := ResetSubState.REQ_ACTIVE_SEND
          }
        }
        is(ResetSubState.REQ_ACTIVE_SEND) {
          io.sbTrainIO.msgReq.valid := true.B
          io.sbTrainIO.msgReq.bits.msg := SBMessage_factory(
            base = SBM.LINK_MGMT_RDI_REQ_ACTIVE,
            src = "PHY",
            dst = "PHY",
          )

          /** TODO: how many timeout cycles here? */
          io.sbTrainIO.msgReq.bits.timeoutCycles := 1_000_000.U
          when(io.sbTrainIO.msgReq.fire) {
            resetSubstate := ResetSubState.REQ_ACTIVE_WAIT
          }
        }
        is(ResetSubState.REQ_ACTIVE_WAIT) {
          io.sbTrainIO.msgReqStatus.ready := true.B
          when(io.sbTrainIO.msgReqStatus.fire) {
            resetSubstate := ResetSubState.RESP_ACTIVE_SEND
          }
        }
        is(ResetSubState.RESP_ACTIVE_SEND) {
          io.sbTrainIO.msgReq.valid := true.B
          io.sbTrainIO.msgReq.bits.msg := SBMessage_factory(
            base = SBM.LINK_MGMT_RDI_RSP_ACTIVE,
            src = "PHY",
            dst = "PHY",
          )

          /** TODO: how many timeout cycles here? */
          io.sbTrainIO.msgReq.bits.timeoutCycles := 1_000_000.U
          when(io.sbTrainIO.msgReqStatus.fire) {
            resetSubstate := ResetSubState.RESP_ACTIVE_WAIT
          }
        }
        is(ResetSubState.RESP_ACTIVE_WAIT) {
          io.sbTrainIO.msgReqStatus.ready := true.B
          when(io.sbTrainIO.msgReqStatus.fire) {
            nextState := PhyState.active
          }
        }
      }
    }
    is(PhyState.active) {
      val nextStateReq = RegInit(PhyState.reset)
      switch(stallReqAckState) {
        is(StallReqAckState.ACTIVE) {
          when(
            nextState === PhyState.retrain || nextState === PhyState.linkReset || nextState === PhyState.disabled,
          ) {

            /** pl_stallreq and lp_stallack exchange */
            stallReqAckState := StallReqAckState.LP_STALLACK_WAIT
            state := PhyState.active
            nextStateReq := nextState
          }
        }
        is(StallReqAckState.LP_STALLACK_WAIT) {
          when(io.rdiIO.lpStallAck) {
            stallReqAckState := StallReqAckState.LP_STALLACK_DEASSERT
            state := PhyState.active
          }
        }
        is(StallReqAckState.LP_STALLACK_DEASSERT) {
          when(!io.rdiIO.lpStallAck) {
            state := nextStateReq
          }
        }
      }
    }
  }

}
