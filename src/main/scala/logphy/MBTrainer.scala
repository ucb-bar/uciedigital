package edu.berkeley.cs.ucie.digital
package logphy

import interfaces._
import chisel3._
import chisel3.util._
import sideband.{SBM, SBMessage_factory}

class TrainingOperation extends Bundle {
  val pattern = TransmitPattern()
}

class MBTrainer(
    linkTrainingParams: LinkTrainingParams,
    afeParams: AfeParams,
    maxPatternCount: Int,
) extends Module {

  val sbClockFreq =
    linkTrainingParams.sbClockFreqAnalog / afeParams.sbSerializerRatio

  val io = IO(new Bundle {
    val trainingOperationIO = Valid(new TrainingOperation)
    val sbTrainIO = Flipped(new SBMsgWrapperTrainIO)
    val sbMsgWrapperReset = Output(Bool())
    val patternGeneratorIO =
      Flipped(new PatternGeneratorIO(afeParams, maxPatternCount))
  })

  private object State extends ChiselEnum {
    val WAIT_PTTEST_REQ_SEND, WAIT_PTTEST_REQ, SEND_PTTEST_REQ,
        WAIT_AND_SEND_PTTEST_RESP, ERR = Value
  }
  private val currentState = RegInit(State.WAIT_PTTEST_REQ_SEND)
  val operation = Reg(new TrainingOperation)
  private val txDtoCPointReq = RegInit(0.U.asTypeOf(new TxDtoCPointReq))

  io.sbMsgWrapperReset := false.B
  when(io.trainingOperationIO.valid) {
    currentState := State.SEND_PTTEST_REQ
    io.sbMsgWrapperReset := true.B

    txDtoCPointReq.dataPattern :=
  }

  private class TxDtoCPointReq extends Bundle {
    val reserved = 0.U(4.W)
    val comparisonMode = UInt(1.W)
    val iterationCount = UInt(16.W)
    val idleCount = UInt(16.W)
    val burstCount = UInt(16.W)
    val patternMode = UInt(1.W)
    val clockPhaseControl = UInt(4.W)
    val validPattern = UInt(3.W)
    val dataPattern = UInt(3.W)
  }

  def formStartTxDtoCPointReq(
      maxErrors: UInt,
      req: TxDtoCPointReq,
      reqType: MessageRequestType.Type,
  ): MessageRequest = {
    val data = Wire(UInt(64.W))
    val msgReq = Wire(new MessageRequest)
    data := req.asTypeOf(UInt(64.W))
    msgReq.msg := SBMessage_factory(
      SBM.MBTRAIN_START_TX_INIT_D_TO_C_POINT_TEST_REQ,
      src = "PHY",
      remote = false,
      dst = "PHY",
      data,
      msgInfo = maxErrors(15, 0),
    )
    msgReq.timeoutCycles := (0.008 * sbClockFreq).toInt.U
    msgReq.reqType := reqType
    msgReq
  }


  switch(currentState) {
    is(State.WAIT_PTTEST_REQ_SEND) {
      io.sbTrainIO.msgReq.valid := true.B
      val txDtoCPointReq = new TxDtoCPointReq
      txDtoCPointReq := DontCare
      io.sbTrainIO.msgReq.bits.msg := formStartTxDtoCPointReq(
        0.U,
        txDtoCPointReq,
        MessageRequestType.RECEIVE,
      )
      when(io.sbTrainIO.msgReq.fire) {
        currentState := State.WAIT_PTTEST_REQ
      }
    }
    is(State.WAIT_PTTEST_REQ) {
      io.sbTrainIO.msgReqStatus.ready := true.B
      when(io.sbTrainIO.msgReqStatus.fire) {
        txDtoCPointReq := io.sbTrainIO.msgReqStatus.bits.data
          .asTypeOf(new TxDtoCPointReq)

        when(
          io.sbTrainIO.msgReqStatus.bits.status === MessageRequestStatusType.ERR,
        ) {
          assert(
            false.B,
            "SB Message wrapper should not throw error in MB trainer",
          )
        }.otherwise {
          currentState := State.WAIT_AND_SEND_PTTEST_RESP
        }
      }
    }
    is(State.SEND_PTTEST_REQ) {

    }
    is(State.WAIT_AND_SEND_PTTEST_RESP) {}
  }

}
