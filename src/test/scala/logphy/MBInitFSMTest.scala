package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import interfaces.{AfeParams, RdiParams}
import sideband.{SBM, SBMessage_factory, SidebandParams}
import org.scalatest.flatspec.AnyFlatSpec

class MBInitFSMTest extends AnyFlatSpec with ChiselScalatestTester {
  val linkTrainingParams = LinkTrainingParams(sbClockFreqAnalog = 8000)
  val sbParams = SidebandParams()
  val afeParams = AfeParams(sbSerializerRatio = 1)
  val rdiParams = RdiParams(128, 128)
  val sbClockFreq =
    linkTrainingParams.sbClockFreqAnalog / afeParams.sbSerializerRatio

  def formParamsReqMsg(
      req: Boolean,
      linkTrainingParams: LinkTrainingParams,
  ): UInt = {
    val data: Int = formMsgReqData(
      linkTrainingParams.mbTrainingParams.voltageSwing,
      linkTrainingParams.mbTrainingParams.maximumDataRate,
      linkTrainingParams.mbTrainingParams.clockMode,
      linkTrainingParams.mbTrainingParams.clockPhase,
      linkTrainingParams.mbTrainingParams.moduleId,
      linkTrainingParams.mbTrainingParams.ucieAx32,
    )
    SBMessage_factory(
      base =
        if (req) SBM.MBINIT_PARAM_CONFIG_REQ
        else SBM.MBINIT_PARAM_CONFIG_RESP,
      src = "PHY",
      remote = false,
      dst = "PHY",
      data = data,
      msgInfo = 0,
    ).U
  }

  def formParamsReqMsg(
      req: Boolean,
      voltageSwing: Int,
      maxDataRate: Int,
      clockMode: ClockModeParam.Type,
      clockPhase: Boolean,
      moduleId: Int,
      UCIeAx32: Boolean,
  ): MessageRequest = {
    val data: Int = formMsgReqData(
      voltageSwing,
      maxDataRate,
      clockMode,
      clockPhase,
      moduleId,
      UCIeAx32,
    )
    val msgReq: MessageRequest = (new MessageRequest).Lit(
      _.msg -> SBMessage_factory(
        base =
          if (req) SBM.MBINIT_PARAM_CONFIG_REQ
          else SBM.MBINIT_PARAM_CONFIG_RESP,
        src = "PHY",
        remote = false,
        dst = "PHY",
        data = data,
        msgInfo = 0,
      ).U,
      _.timeoutCycles -> (0.008 * sbClockFreq).toInt.U,
      // _.reqType -> (if (req) MessageRequestType.MSG_REQ
      //               else MessageRequestType.MSG_RESP),
    )
    msgReq
  }

  def formMsgReqData(
      voltageSwing: Int,
      maxDataRate: Int,
      clockMode: ClockModeParam.Type,
      clockPhase: Boolean,
      moduleId: Int,
      UCIeAx32: Boolean,
  ) = {
    var data: Int = 0
    data |= (if (UCIeAx32) 1 else 0) << 13
    data |= (moduleId & 0x3) << 11
    data |= (if (clockPhase) 1 else 0) << 10
    data |= (if (clockMode == ClockModeParam.strobe) 0 else 1) << 9
    data |= (voltageSwing & 0x1f) << 4
    data |= (maxDataRate & 0xf)
    data
  }

  behavior of "MBInitFSM"
  it should "perform parameter exchange -- basic sim" in {
    test(
      new MBInitFSM(linkTrainingParams, afeParams, maxPatternCount = 1 << 32),
    ) { c =>
      initializePorts(c)
      initialCheck(c)
      dequeueParamReq(c)
      enqueueParamReqResp(c)
      dequeueParamResp(c)
      enqueueParamRespResp(c)

      c.io.transition.expect(true.B)
      c.io.error.expect(false.B)
    }
  }

  behavior of "MBInitFSM"
  it should "perform parameter exchange with delays" in {
    test(
      new MBInitFSM(linkTrainingParams, afeParams, maxPatternCount = 1 << 32),
    ) { c =>
      initializePorts(c)
      initialCheck(c)
      dequeueParamReq(c)
      for (_ <- 0 until 10) {
        c.io.transition.expect(false.B)
      }
      enqueueParamReqResp(c)
      for (_ <- 0 until 10) {
        c.io.transition.expect(false.B)
      }
      dequeueParamResp(c)
      for (_ <- 0 until 10) {
        c.io.transition.expect(false.B)
      }
      enqueueParamRespResp(c)

      c.io.transition.expect(true.B)
      c.io.error.expect(false.B)
    }
  }

  behavior of "MBInitFSM"
  it should "timeout" in {
    test(
      new MBInitFSM(linkTrainingParams, afeParams, maxPatternCount = 1 << 32),
    ) { c =>
      c.clock.setTimeout((0.008 * sbClockFreq).toInt + 20)
      initializePorts(c)
      initialCheck(c)
      dequeueParamReq(c)
      for (_ <- 0 until 10) {
        c.io.transition.expect(false.B)
      }
      enqueueParamReqResp(c)
      dequeueParamResp(c)
      c.io.sbTrainIO.msgReqStatus.enqueueNow(
        (new MessageRequestStatus).Lit(
          _.status ->
            MessageRequestStatusType.ERR,
          _.data -> formMsgReqData(
            linkTrainingParams.mbTrainingParams.voltageSwing,
            linkTrainingParams.mbTrainingParams.maximumDataRate,
            linkTrainingParams.mbTrainingParams.clockMode,
            linkTrainingParams.mbTrainingParams.clockPhase,
            linkTrainingParams.mbTrainingParams.moduleId,
            linkTrainingParams.mbTrainingParams.ucieAx32,
          ).U,
        ),
      )
      c.io.transition.expect(true.B)
      c.io.error.expect(true.B)
    }
  }

  private def enqueueParamRespResp(c: MBInitFSM): Unit = {
    c.io.sbTrainIO.msgReqStatus.enqueueNow(
      (new MessageRequestStatus).Lit(
        _.status ->
          MessageRequestStatusType.SUCCESS,
        _.data -> formMsgReqData(
          linkTrainingParams.mbTrainingParams.voltageSwing,
          linkTrainingParams.mbTrainingParams.maximumDataRate,
          linkTrainingParams.mbTrainingParams.clockMode,
          linkTrainingParams.mbTrainingParams.clockPhase,
          linkTrainingParams.mbTrainingParams.moduleId,
          linkTrainingParams.mbTrainingParams.ucieAx32,
        ).U,
      ),
    )
  }

  private def dequeueParamResp(c: MBInitFSM): Unit = {
    c.io.sbTrainIO.msgReq.expectDequeue(
      formParamsReqMsg(
        false,
        linkTrainingParams.mbTrainingParams.voltageSwing,
        linkTrainingParams.mbTrainingParams.maximumDataRate,
        linkTrainingParams.mbTrainingParams.clockMode,
        linkTrainingParams.mbTrainingParams.clockPhase,
        linkTrainingParams.mbTrainingParams.moduleId,
        linkTrainingParams.mbTrainingParams.ucieAx32,
      ),
    )

    c.io.transition.expect(false.B)
    c.io.error.expect(false.B)
  }

  private def enqueueParamReqResp(c: MBInitFSM): Unit = {
    c.io.sbTrainIO.msgReqStatus.enqueueNow(
      (new MessageRequestStatus).Lit(
        _.status ->
          MessageRequestStatusType.SUCCESS,
        _.data -> formMsgReqData(
          linkTrainingParams.mbTrainingParams.voltageSwing,
          linkTrainingParams.mbTrainingParams.maximumDataRate,
          linkTrainingParams.mbTrainingParams.clockMode,
          linkTrainingParams.mbTrainingParams.clockPhase,
          linkTrainingParams.mbTrainingParams.moduleId,
          linkTrainingParams.mbTrainingParams.ucieAx32,
        ).U,
      ),
    )

    c.io.transition.expect(false.B)
    c.io.error.expect(false.B)
  }

  private def dequeueParamReq(c: MBInitFSM): Unit = {
    c.io.sbTrainIO.msgReq.expectDequeue(
      formParamsReqMsg(
        true,
        linkTrainingParams.mbTrainingParams.voltageSwing,
        linkTrainingParams.mbTrainingParams.maximumDataRate,
        linkTrainingParams.mbTrainingParams.clockMode,
        linkTrainingParams.mbTrainingParams.clockPhase,
        linkTrainingParams.mbTrainingParams.moduleId,
        linkTrainingParams.mbTrainingParams.ucieAx32,
      ),
    )
    c.io.transition.expect(false.B)
    c.io.error.expect(false.B)
  }

  private def initialCheck(c: MBInitFSM): Unit = {
    c.io.transition.expect(false.B)
    c.io.error.expect(false.B)
    c.clock.step()
  }

  private def initializePorts(c: MBInitFSM) = {
    c.io.sbTrainIO.msgReq.initSink().setSinkClock(c.clock)
    c.io.sbTrainIO.msgReqStatus.initSource().setSourceClock(c.clock)
    c.io.patternGeneratorIO.transmitReq.initSink().setSinkClock(c.clock)
    c.io.patternGeneratorIO.transmitPatternStatus
      .initSource()
      .setSourceClock(c.clock)
  }
}
