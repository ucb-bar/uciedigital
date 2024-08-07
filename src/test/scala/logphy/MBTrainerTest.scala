package edu.berkeley.cs.ucie.digital
package logphy

import interfaces._
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor
import chiseltest._
import edu.berkeley.cs.ucie.digital.sideband.{SBM, SBMessage_factory}
import org.scalatest.flatspec.AnyFlatSpec

class MBTrainerTest extends AnyFlatSpec with ChiselScalatestTester {

  val linkTrainingParams = LinkTrainingParams()
  val afeParams = AfeParams()
  val maxPatternCount = 1024
  val maxPatternCountWidth = log2Ceil(maxPatternCount + 1)
  behavior of "mainband trainer test"
  it should "correctly exchange SB out of reset message external trigger" in {
    test(new MBTrainer(linkTrainingParams, afeParams, maxPatternCount)) { c =>
      initPorts(c)
      var transmitPattern = TransmitPattern.LFSR
      val sbClockFreq =
        linkTrainingParams.sbClockFreqAnalog / afeParams.sbSerializerRatio
      val timeoutCyclesDefault = (0.008 * sbClockFreq).toInt
      var patternUICount = afeParams.mbLanes * afeParams.mbSerializerRatio * 4
      val maxErrors = 0

      initialExpectedValues(c)
      triggerOperation(
        c,
        transmitPattern,
        patternUICount,
        timeoutCyclesDefault,
        maxErrors,
      )

      var errorCount = Vec.Lit(
        Seq.fill(afeParams.mbLanes)(1.U(maxPatternCountWidth.W)): _*,
      )
      completeTrainingOperation(
        c,
        transmitPattern,
        timeoutCyclesDefault,
        patternUICount,
        errorCount,
      )

      transmitPattern = TransmitPattern.PER_LANE_ID
      patternUICount = afeParams.mbLanes * afeParams.mbSerializerRatio
      errorCount = Vec.Lit(
        Seq.tabulate(afeParams.mbLanes)(x => x.U(maxPatternCountWidth.W)): _*,
      )

      triggerOperation(
        c,
        transmitPattern,
        patternUICount,
        timeoutCyclesDefault,
        maxErrors,
      )

      completeTrainingOperation(
        c,
        transmitPattern,
        timeoutCyclesDefault,
        patternUICount,
        errorCount,
      )

      triggerExit(c, timeoutCyclesDefault)
      exitTraining(c, timeoutCyclesDefault)

    }
  }
  it should "correctly exchange SB out of reset message sideband trigger" in {
    test(new MBTrainer(linkTrainingParams, afeParams, maxPatternCount)) { c =>
      initPorts(c)
      var transmitPattern = TransmitPattern.LFSR
      val sbClockFreq =
        linkTrainingParams.sbClockFreqAnalog / afeParams.sbSerializerRatio
      val timeoutCyclesDefault = (0.008 * sbClockFreq).toInt
      var patternUICount = afeParams.mbLanes * afeParams.mbSerializerRatio * 4
      val maxErrors = 0

      initialExpectedValues(c)

      /** Trigger operation via sideband */
      triggerOperationSB(
        c,
        timeoutCyclesDefault,
        transmitPattern,
        patternUICount,
        maxErrors,
      )

      var errorCount = Vec.Lit(
        Seq.fill(afeParams.mbLanes)(1.U(maxPatternCountWidth.W)): _*,
      )

      completeTrainingOperation(
        c,
        transmitPattern,
        timeoutCyclesDefault,
        patternUICount,
        errorCount,
      )

      transmitPattern = TransmitPattern.PER_LANE_ID
      patternUICount = afeParams.mbLanes * afeParams.mbSerializerRatio
      errorCount = Vec.Lit(
        Seq.tabulate(afeParams.mbLanes)(x => x.U(maxPatternCountWidth.W)): _*,
      )

      /** NOTE: cannot re-trigger operation with sideband as sideband is not
        * expecting a begin training request
        */
      triggerOperation(
        c,
        transmitPattern,
        patternUICount,
        timeoutCyclesDefault,
        maxErrors,
      )

      completeTrainingOperation(
        c,
        transmitPattern,
        timeoutCyclesDefault,
        patternUICount,
        errorCount,
      )

      /** Trigger exit via sideband */
      triggerExitSB(c, timeoutCyclesDefault)

      exitTraining(c, timeoutCyclesDefault)

    }
  }

  private def triggerExitSB(c: MBTrainer, timeoutCyclesDefault: Int): Unit = {

    expectSBReq(
      c = c,
      bitPat = SBM.MBTRAIN_END_TX_INIT_D_TO_C_POINT_TEST_REQ,
      MessageRequestType.RECEIVE,
      timeoutCyclesDefault,
      msgInfo = 0,
      data = 0,
    )
    sbMsgSuccess(c)
  }

  private def triggerOperationSB(
      c: MBTrainer,
      timeoutCyclesDefault: Int,
      transmitPattern: TransmitPattern.Type,
      patternUICount: Int,
      maxErrors: Int,
  ): Unit = {
    val data = 0 | transmitPattern.litValue | (BigInt(patternUICount) << 43)
    expectSBReq(
      c,
      SBM.MBTRAIN_START_TX_INIT_D_TO_C_POINT_TEST_REQ,
      MessageRequestType.RECEIVE,
      timeoutCyclesDefault,
      msgInfo = 0,
      data = 0,
    )
    sbMsgSuccess(c, data)
  }

  private def initialExpectedValues(c: MBTrainer): Unit = {
    c.io.patternGeneratorIO.transmitReq.expectInvalid()
    c.io.patternGeneratorIO.resp.ready.expect(false.B)
    c.io.sbTrainIO.msgReqStatus.ready.expect(false.B)
    c.io.complete.expect(false.B)
    c.io.trainingOperationIO.outputValid.expect(false.B)
    c.io.err.expect(false.B)
    c.io.sbMsgWrapperReset.expect(false.B)
    c.clock.step()
  }

  private def exitTraining(c: MBTrainer, timeoutCyclesDefault: Int): Unit = {

    expectSBReq(
      c = c,
      bitPat = SBM.MBTRAIN_END_TX_INIT_D_TO_C_POINT_TEST_RESP,
      reqType = MessageRequestType.EXCHANGE,
      timeoutCyclesDefault = timeoutCyclesDefault,
      msgInfo = 0,
      data = 0,
    )

    sbMsgSuccess(c)

    c.clock.step()
    c.io.complete.expect(true.B)
    c.io.err.expect(false.B)
  }

  private def triggerExit(c: MBTrainer, timeoutCyclesDefault: Int): Unit = {
    c.io.trainingOperationIO.triggerExit.poke(true.B)
    c.clock.step()
    c.io.trainingOperationIO.triggerExit.poke(false.B)

    /** Expect Pt Test End Test Req */
    expectSBReq(
      c = c,
      bitPat = SBM.MBTRAIN_END_TX_INIT_D_TO_C_POINT_TEST_REQ,
      reqType = MessageRequestType.SEND,
      timeoutCyclesDefault = timeoutCyclesDefault,
      msgInfo = 0,
      data = 0,
    )

    sbMsgSuccess(c)
  }

  private def triggerOperation(
      c: MBTrainer,
      transmitPattern: TransmitPattern.Type,
      patternUICount: Int,
      timeoutCyclesDefault: Int,
      maxErrors: Int,
  ): Unit = {
    c.io.trainingOperationIO.triggerNew.poke(true.B)
    c.io.trainingOperationIO.pattern.poke(transmitPattern)
    c.io.trainingOperationIO.patternUICount.poke(patternUICount.U)
    c.io.trainingOperationIO.triggerExit.poke(false.B)
    c.clock.step()
    c.io.trainingOperationIO.triggerNew.poke(false.B)
    c.io.patternGeneratorIO.transmitReq.expectInvalid()
    c.io.patternGeneratorIO.resp.ready.expect(false.B)
    println("External trigger")

    val data = 0 | transmitPattern.litValue | (BigInt(patternUICount) << 43)
    expectSBReq(
      c,
      SBM.MBTRAIN_START_TX_INIT_D_TO_C_POINT_TEST_REQ,
      reqType = MessageRequestType.SEND,
      timeoutCyclesDefault,
      maxErrors,
      data,
    )
    println("Received SB request for Point Test Start Req")

    /** Complete PTTest SB request */
    sbMsgSuccess(c)
  }

  private def completeTrainingOperation(
      c: MBTrainer,
      transmitPattern: TransmitPattern.Type,
      timeoutCyclesDefault: Int,
      patternUICount: Int,
      errorCount: Vec[UInt],
  ): Unit = {

    println("********** BEGIN TRAINING OPERATION ***********")

    /** Expect PTTest SB response */
    expectSBReq(
      c,
      SBM.MBTRAIN_START_TX_INIT_D_TO_C_POINT_TEST_RESP,
      reqType = MessageRequestType.EXCHANGE,
      timeoutCyclesDefault,
      msgInfo = 0,
      data = 0,
    )
    println("Received SB request for Point Test Start Resp")

    /** Complete PTTest SB response */
    sbMsgSuccess(c)

    /** Expect pattern generator request */
    c.io.sbTrainIO.msgReqStatus.ready.expect(false.B)
    c.io.sbTrainIO.msgReq.expectInvalid()
    c.io.patternGeneratorIO.resp.ready.expect(false.B)
    c.io.patternGeneratorIO.transmitReq
      .expectDequeue(
        (chiselTypeOf(c.io.patternGeneratorIO.transmitReq.bits)).Lit(
          _.pattern -> transmitPattern,
          _.timeoutCycles -> timeoutCyclesDefault.U,
          _.patternCountMax -> patternUICount.U,
          _.patternDetectedCountMax -> patternUICount.U,
        ),
      )
    println("Received Pattern Generator request")

    /** Complete pattern generator request */
    c.io.sbTrainIO.msgReqStatus.ready.expect(false.B)
    c.io.sbTrainIO.msgReq.expectInvalid()
    c.io.patternGeneratorIO.resp.enqueueNow(
      (chiselTypeOf(c.io.patternGeneratorIO.resp.bits)).Lit(
        _.status -> MessageRequestStatusType.SUCCESS,
        _.errorCount -> errorCount,
      ),
    )

    c.io.trainingOperationIO.errorCounts.expect(errorCount)

    /** Expect Results Req req */
    expectSBReq(
      c,
      SBM.MBTRAIN_TX_INIT_D_TO_C_RESULTS_REQ,
      reqType = MessageRequestType.EXCHANGE,
      timeoutCyclesDefault,
      msgInfo = 0,
      data = 0,
    )
    println("Received SB request for Results request")

    /** Complete Results req */
    sbMsgSuccess(c)

    /** Expect Results Resp req */
    expectSBReq(
      c,
      SBM.MBTRAIN_TX_INIT_D_TO_C_RESULTS_RESP,
      reqType = MessageRequestType.EXCHANGE,
      timeoutCyclesDefault,
      msgInfo = 0,
      data = 0,
    )
    println("Received SB request for Results response")

    sbMsgSuccess(c)

    /** Now, the test should be waiting for either external intervention or a SB
      * request...
      */
    for (_ <- 0 until 10) {
      c.clock.step()
      c.io.patternGeneratorIO.transmitReq.expectInvalid()
      c.io.patternGeneratorIO.resp.ready.expect(false.B)
      c.io.complete.expect(false.B)
      c.io.err.expect(false.B)
      c.io.sbMsgWrapperReset.expect(false.B)
      c.io.trainingOperationIO.outputValid.expect(true.B)
      c.io.trainingOperationIO.errorCounts.expect(errorCount)
    }

  }

  private def expectSBReq(
      c: MBTrainer,
      bitPat: BitPat,
      reqType: MessageRequestType.Type,
      timeoutCyclesDefault: Int,
      msgInfo: Int,
      data: BigInt,
  ): Unit = {
    c.io.sbTrainIO.msgReqStatus.ready.expect(false.B)
    c.io.sbTrainIO.msgReq.expectDequeue(
      (new MessageRequest).Lit(
        _.msg -> (SBMessage_factory(
          bitPat,
          src = "PHY",
          remote = true,
          dst = "PHY",
          data,
          msgInfo = msgInfo,
        )).U,
        _.reqType -> reqType,
        _.timeoutCycles -> timeoutCyclesDefault.U,
        _.repeat -> false.B,
      ),
    )
    c.io.patternGeneratorIO.transmitReq.expectInvalid()
    c.io.patternGeneratorIO.resp.ready.expect(false.B)
  }

  private def sbMsgSuccess(c: MBTrainer, data: BigInt = 0): Unit = {
    c.io.sbTrainIO.msgReqStatus.enqueueNow(
      (new MessageRequestStatus).Lit(
        _.data -> data.U,
        _.status -> MessageRequestStatusType.SUCCESS,
      ),
    )
  }

  private def initPorts(c: MBTrainer): Unit = {
    c.io.sbTrainIO.msgReq.initSink().setSinkClock(c.clock)
    c.io.sbTrainIO.msgReqStatus.initSource().setSourceClock(c.clock)
    c.io.patternGeneratorIO.transmitReq
      .initSink()
      .setSinkClock(c.clock)
    c.io.patternGeneratorIO.resp
      .initSource()
      .setSourceClock(c.clock)
  }
}
