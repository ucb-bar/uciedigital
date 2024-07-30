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
      val transmitPattern = TransmitPattern.LFSR
      val sbClockFreq =
        linkTrainingParams.sbClockFreqAnalog / afeParams.sbSerializerRatio
      val timeoutCyclesDefault = (0.008 * sbClockFreq).toInt
      val patternUICount = afeParams.mbLanes * afeParams.mbSerializerRatio * 4

      /** Initial expected values */
      c.io.patternGeneratorIO.transmitReq.expectInvalid()
      c.io.patternGeneratorIO.resp.ready.expect(false.B)
      c.io.sbTrainIO.msgReqStatus.ready.expect(false.B)
      c.io.complete.expect(false.B)
      c.io.err.expect(false.B)
      c.io.sbMsgWrapperReset.expect(false.B)
      c.clock.step()

      /** Trigger externally */
      c.io.trainingOperationIO.triggerNew.poke(true.B)
      c.io.trainingOperationIO.pattern.poke(transmitPattern)
      c.io.trainingOperationIO.patternUICount.poke(patternUICount.U)
      c.clock.step()
      c.io.patternGeneratorIO.transmitReq.expectInvalid()
      c.io.patternGeneratorIO.resp.ready.expect(false.B)

      /** Expect PTTest SB request */
      val maxErrors = 0
      val data = 0 | transmitPattern.litValue | (BigInt(patternUICount) << 43)
      expectSBReq(
        c,
        SBM.MBTRAIN_START_TX_INIT_D_TO_C_POINT_TEST_REQ,
        reqType = MessageRequestType.SEND,
        timeoutCyclesDefault,
        maxErrors,
        data,
      )

      /** Complete PTTest SB request */
      sbMsgSuccess(c)

      /** Expect PTTest SB response */
      expectSBReq(
        c,
        SBM.MBTRAIN_START_TX_INIT_D_TO_C_POINT_TEST_RESP,
        reqType = MessageRequestType.EXCHANGE,
        timeoutCyclesDefault,
        msgInfo = 0,
        data = 0,
      )

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

      /** Complete pattern generator request */
      val errorCount = Vec.Lit(
        Seq.fill(afeParams.mbLanes)(1.U(maxPatternCountWidth.W)): _*,
      )
      c.io.sbTrainIO.msgReqStatus.ready.expect(false.B)
      c.io.sbTrainIO.msgReq.expectInvalid()
      c.io.patternGeneratorIO.resp.enqueueNow(
        (chiselTypeOf(c.io.patternGeneratorIO.resp.bits)).Lit(
          _.status -> MessageRequestStatusType.SUCCESS,
          _.errorCount -> errorCount,
        ),
      )

      /** Expect Results Req req */
      expectSBReq(
        c,
        SBM.MBTRAIN_TX_INIT_D_TO_C_RESULTS_REQ,
        reqType = MessageRequestType.EXCHANGE,
        timeoutCyclesDefault,
        msgInfo = 0,
        data = 0,
      )

      /** Complete Results req */
      sbMsgSuccess(c)

      // TODO: finish

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
      ),
    )
    c.io.patternGeneratorIO.transmitReq.expectInvalid()
    c.io.patternGeneratorIO.resp.ready.expect(false.B)
  }

  private def sbMsgSuccess(c: MBTrainer): Unit = {
    c.io.sbTrainIO.msgReqStatus.enqueueNow(
      (new MessageRequestStatus).Lit(
        _.data -> 0.U,
        _.status -> MessageRequestStatusType.SUCCESS,
      ),
    )
  }

  it should "correctly exchange SB out of reset message sideband trigger" in {
    test(new MBTrainer(linkTrainingParams, afeParams, maxPatternCount)) { c =>
      initPorts(c)

    }
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
