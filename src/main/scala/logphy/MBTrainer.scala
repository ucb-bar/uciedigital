package edu.berkeley.cs.ucie.digital
package logphy

import interfaces._
import chisel3._
import chisel3.util._
import sideband.{SBM, SBMessage_factory}

/** TODO: make timeout cycles optional */
class TrainingOperation(afeParams: AfeParams, maxPatternCount: Int)
    extends Bundle {
  val maxPatternCountWidth = log2Ceil(maxPatternCount + 1)
  val pattern = Input(TransmitPattern())
  val patternUICount = Input(UInt(maxPatternCountWidth.W))
  val triggerNew = Input(Bool())
  val triggerExit = Input(Bool())
  val outputValid = Output(Bool())
  val errorCounts = Output(Vec(afeParams.mbLanes, UInt(maxPatternCountWidth.W)))
}

class MBTrainer(
    linkTrainingParams: LinkTrainingParams,
    afeParams: AfeParams,
    maxPatternCount: Int,
) extends Module {

  val sbClockFreq =
    linkTrainingParams.sbClockFreqAnalog / afeParams.sbSerializerRatio

  val io = IO(new Bundle {
    val trainingOperationIO = new TrainingOperation(afeParams, maxPatternCount)
    val sbTrainIO = Flipped(new SBMsgWrapperTrainIO)
    val sbMsgWrapperReset = Output(Bool())
    val patternGeneratorIO =
      Flipped(new PatternGeneratorIO(afeParams, maxPatternCount))
    val complete = Output(Bool())
    val err = Output(Bool())
  })

  io.sbTrainIO.msgReq.noenq()
  io.sbTrainIO.msgReqStatus.nodeq()
  io.sbTrainIO.msgReq.bits.repeat := false.B
  io.patternGeneratorIO.transmitReq.noenq()
  io.patternGeneratorIO.resp.nodeq()

  private object State extends ChiselEnum {
    val WAIT_PTTEST_REQ_SEND, WAIT_PTTEST_REQ, SEND_PTTEST_REQ,
        SEND_PTTEST_REQ_WAIT, SEND_PTTEST_RESP, SEND_PTTEST_RESP_WAIT,
        TRAIN_SEND_PATTERN_REQ, TRAIN_WAIT_PATTERN_RESP,
        TRAIN_PATTERN_FINISHED_SEND, TRAIN_PATTERN_FINISHED_WAIT,
        PTTEST_RESULTS_REQ_SEND, PTTEST_RESULTS_REQ_WAIT,
        PTTEST_RESULTS_RESP_SEND, PTTEST_RESULTS_RESP_WAIT,
        PTTEST_END_TEST_REQ_SEND, PTTEST_END_TEST_REQ_WAIT,
        PTTEST_END_TEST_RESP_SEND, PTTEST_END_TEST_RESP_WAIT, COMPLETE, ERR =
      Value
  }
  private val currentState = RegInit(State.WAIT_PTTEST_REQ_SEND)

  io.complete := currentState === State.COMPLETE || currentState === State.ERR
  io.err := currentState === State.ERR

  private val txDtoCPointReq = RegInit(0.U.asTypeOf(new TxDtoCPointReq))

  io.sbMsgWrapperReset := false.B
  when(io.trainingOperationIO.triggerNew) {
    currentState := State.SEND_PTTEST_REQ
    io.sbMsgWrapperReset := true.B

    val pointReq = Wire(new TxDtoCPointReq)
    pointReq := DontCare
    pointReq.dataPattern := io.trainingOperationIO.pattern.asUInt
    pointReq.iterationCount := io.trainingOperationIO.patternUICount
    txDtoCPointReq := pointReq
  }.elsewhen(io.trainingOperationIO.triggerExit) {
    currentState := State.PTTEST_END_TEST_REQ_SEND
    io.sbMsgWrapperReset := true.B
  }

  private class TxDtoCPointReq extends Bundle {
    val reserved = UInt(4.W)
    val comparisonMode = UInt(1.W)
    val iterationCount = UInt(16.W)
    val idleCount = UInt(16.W)
    val burstCount = UInt(16.W)
    val patternMode = UInt(1.W)
    val clockPhaseControl = UInt(4.W)
    val validPattern = UInt(3.W)
    val dataPattern = UInt(3.W)
  }

  private class TxDtoCResultsResp extends Bundle {
    val msgInfo = new Bundle {
      val reserved = UInt(10.W)
      val validLaneComparisonResults = UInt(1.W)
      val cumulativeResults = UInt(1.W)
      val redundantLaneComparison = UInt(4.W)
    }
    val data = new Bundle {
      val reserved = UInt(48.W)
      val lanePassResults = Vec(afeParams.mbLanes, Bool())
    }
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
      remote = true,
      dst = "PHY",
      data,
      msgInfo = maxErrors(15, 0),
    )
    msgReq.timeoutCycles := (0.008 * sbClockFreq).toInt.U
    msgReq.reqType := reqType
    msgReq.repeat := false.B
    msgReq
  }

  def formStartTxDtoCPointResp(
      reqType: MessageRequestType.Type,
  ): MessageRequest = {
    val msgReq = Wire(new MessageRequest)
    msgReq.msg := SBMessage_factory(
      SBM.MBTRAIN_START_TX_INIT_D_TO_C_POINT_TEST_RESP,
      src = "PHY",
      remote = true,
      dst = "PHY",
    )
    msgReq.timeoutCycles := (0.008 * sbClockFreq).toInt.U
    msgReq.reqType := reqType
    msgReq.repeat := false.B
    msgReq
  }

  private val errorCount = Reg(
    chiselTypeOf(io.patternGeneratorIO.resp.bits.errorCount),
  )
  private val errorCountValid = RegInit(false.B)
  io.trainingOperationIO.errorCounts := errorCount
  io.trainingOperationIO.outputValid := errorCountValid

  switch(currentState) {
    is(State.WAIT_PTTEST_REQ_SEND) {

      /** Send the request to the SB trainer to wait for the Pt Test Req msg */
      io.sbTrainIO.msgReq.valid := true.B
      val txDtoCPointReq = Wire(new TxDtoCPointReq)
      txDtoCPointReq := DontCare
      io.sbTrainIO.msgReq.bits := formStartTxDtoCPointReq(
        0.U,
        txDtoCPointReq,
        MessageRequestType.RECEIVE,
      )
      when(io.sbTrainIO.msgReq.fire) {
        currentState := State.WAIT_PTTEST_REQ
      }
    }
    is(State.WAIT_PTTEST_REQ) {

      /** Wait for SB trainer to indicate that Pt Test Req message was received
        */
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
          currentState := State.SEND_PTTEST_RESP
        }
      }
    }
    is(State.SEND_PTTEST_REQ) {

      /** Send Pt Test Req message (skip if already received Pt Test Req) */
      io.sbTrainIO.msgReq.valid := true.B
      io.sbTrainIO.msgReq.bits := formStartTxDtoCPointReq(
        0.U,
        txDtoCPointReq,
        MessageRequestType.SEND,
      )
      when(io.sbTrainIO.msgReq.fire) {
        currentState := State.SEND_PTTEST_REQ_WAIT
      }
    }
    is(State.SEND_PTTEST_REQ_WAIT) {

      /** Wait for Pt Test Request to be received (skip if already received pt
        * test request)
        */
      receiveSBMsg(State.SEND_PTTEST_RESP)
    }
    is(State.SEND_PTTEST_RESP) {

      /** Have SB Trainer send a response */
      sendSBReq(
        SBM.MBTRAIN_START_TX_INIT_D_TO_C_POINT_TEST_RESP,
        MessageRequestType.EXCHANGE,
        State.SEND_PTTEST_RESP_WAIT,
      )
    }
    is(State.SEND_PTTEST_RESP_WAIT) {

      /** Wait for SB trainer to indicate Start Pt Test response was a success
        */
      receiveSBMsg(State.TRAIN_SEND_PATTERN_REQ)
    }
    is(State.TRAIN_SEND_PATTERN_REQ) {

      /** Request pattern generator to complete the pattern test */
      io.patternGeneratorIO.transmitReq.bits.pattern := txDtoCPointReq.dataPattern
        .asUInt(1, 0)
        .asTypeOf(TransmitPattern())
      io.patternGeneratorIO.transmitReq.bits.timeoutCycles := (0.008 * sbClockFreq).toInt.U
      io.patternGeneratorIO.transmitReq.bits.patternCountMax := txDtoCPointReq.iterationCount
      io.patternGeneratorIO.transmitReq.bits.patternDetectedCountMax := txDtoCPointReq.iterationCount
      io.patternGeneratorIO.transmitReq.valid := true.B
      when(io.patternGeneratorIO.transmitReq.fire) {
        currentState := State.TRAIN_WAIT_PATTERN_RESP
      }
    }
    is(State.TRAIN_WAIT_PATTERN_RESP) {

      /** Wait for pattern generator to indicate that test is complete */
      io.patternGeneratorIO.resp.ready := true.B
      when(io.patternGeneratorIO.resp.fire) {
        when(
          io.patternGeneratorIO.resp.bits.status === MessageRequestStatusType.ERR,
        ) {
          currentState := State.ERR
        }
          .otherwise {
            errorCount := io.patternGeneratorIO.resp.bits.errorCount
            currentState := State.PTTEST_RESULTS_REQ_SEND
          }
      }
    }
    is(State.PTTEST_RESULTS_REQ_SEND) {

      /** Request SB trainer to send results request message */
      io.sbTrainIO.msgReq.valid := true.B
      io.sbTrainIO.msgReq.bits.msg := SBMessage_factory(
        SBM.MBTRAIN_TX_INIT_D_TO_C_RESULTS_REQ,
        "PHY",
        true,
        "PHY",
      )
      io.sbTrainIO.msgReq.bits.reqType := MessageRequestType.EXCHANGE
      io.sbTrainIO.msgReq.bits.timeoutCycles := (0.008 * sbClockFreq).toInt.U
      when(io.sbTrainIO.msgReq.fire) {

        /** Received result req, no need to send result req */
        currentState := State.PTTEST_RESULTS_REQ_WAIT
      }
    }
    is(State.PTTEST_RESULTS_REQ_WAIT) {

      /** Wait for SB trainer to indicate that results was a success */
      receiveSBMsg(State.PTTEST_RESULTS_RESP_SEND)
    }
    is(State.PTTEST_RESULTS_RESP_SEND) {

      /** Request SB messenger to exchange results resp */
      val resultsResp = Wire(new TxDtoCResultsResp)
      resultsResp := DontCare
      io.sbTrainIO.msgReq.bits.msg := SBMessage_factory(
        SBM.MBTRAIN_TX_INIT_D_TO_C_RESULTS_RESP,
        "PHY",
        true,
        "PHY",
        resultsResp.data.asUInt,
        resultsResp.msgInfo.asUInt,
      )
      io.sbTrainIO.msgReq.bits.reqType := MessageRequestType.EXCHANGE
      io.sbTrainIO.msgReq.bits.timeoutCycles := (0.008 * sbClockFreq).toInt.U
      io.sbTrainIO.msgReq.valid := true.B
      when(io.sbTrainIO.msgReq.fire) {
        currentState := State.PTTEST_RESULTS_RESP_WAIT
      }
    }
    is(State.PTTEST_RESULTS_RESP_WAIT) {
      receiveSBMsg(State.TRAIN_PATTERN_FINISHED_SEND)
    }
    is(State.TRAIN_PATTERN_FINISHED_SEND) {

      /** From here, wait for a few things: (1) re-trigger training (2) finish
        * training trigger (3) SB message indicating training is finished
        */

      io.sbTrainIO.msgReq.valid := true.B
      errorCountValid := true.B
      io.sbTrainIO.msgReq.bits.msg := SBMessage_factory(
        SBM.MBTRAIN_END_TX_INIT_D_TO_C_POINT_TEST_REQ,
        "PHY",
        true,
        "PHY",
      )
      io.sbTrainIO.msgReq.bits.reqType := MessageRequestType.RECEIVE
      io.sbTrainIO.msgReq.bits.timeoutCycles := (0.008 * sbClockFreq).toInt.U
      when(io.sbTrainIO.msgReq.fire) {
        currentState := State.TRAIN_PATTERN_FINISHED_WAIT
      }
    }
    is(State.TRAIN_PATTERN_FINISHED_WAIT) {
      val nextState = State.PTTEST_END_TEST_RESP_SEND
      receiveSBMsg(nextState)
    }
    is(State.PTTEST_END_TEST_REQ_SEND) {
      val reqType = MessageRequestType.SEND
      sendSBReq(
        SBM.MBTRAIN_END_TX_INIT_D_TO_C_POINT_TEST_REQ,
        reqType,
        State.PTTEST_END_TEST_REQ_WAIT,
      )
    }
    is(State.PTTEST_END_TEST_REQ_WAIT) {
      receiveSBMsg(State.PTTEST_END_TEST_RESP_SEND)
    }
    is(State.PTTEST_END_TEST_RESP_SEND) {
      sendSBReq(
        SBM.MBTRAIN_END_TX_INIT_D_TO_C_POINT_TEST_RESP,
        MessageRequestType.EXCHANGE,
        State.PTTEST_END_TEST_RESP_WAIT,
      )
    }
    is(State.PTTEST_END_TEST_RESP_WAIT) {
      receiveSBMsg(State.COMPLETE)
    }
    is(State.COMPLETE) {}
    is(State.ERR) {}

  }

  private def receiveSBMsg(nextState: State.Type) = {
    io.sbTrainIO.msgReqStatus.ready := true.B
    when(io.sbTrainIO.msgReqStatus.fire) {
      when(
        io.sbTrainIO.msgReqStatus.bits.status === MessageRequestStatusType.ERR,
      ) {
        currentState := State.ERR
      }.otherwise {
        currentState := nextState
      }
    }
  }

  private def sendSBReq(
      message: BitPat,
      reqType: MessageRequestType.Type,
      nextState: State.Type,
  ): Unit = {
    io.sbTrainIO.msgReq.bits.msg := SBMessage_factory(
      message,
      "PHY",
      remote = true,
      "PHY",
    )
    io.sbTrainIO.msgReq.bits.reqType := reqType
    io.sbTrainIO.msgReq.bits.timeoutCycles := (0.008 * sbClockFreq).toInt.U
    io.sbTrainIO.msgReq.valid := true.B
    when(io.sbTrainIO.msgReq.fire) {
      currentState := nextState
    }
  }

}
