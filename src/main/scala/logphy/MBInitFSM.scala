package edu.berkeley.cs.ucie.digital
package logphy

import interfaces._
import chisel3._
import chisel3.util._
import sideband.{SBM, SBMessage_factory}

case class MBTrainingParams(
    voltageSwing: Int = 0,
    maximumDataRate: Int = 0,
    clockMode: ClockModeParam.Type = ClockModeParam.strobe,
    clockPhase: Boolean = false,
    moduleId: Int = 0,
    ucieAx32: Boolean = false,
)

class MBInitFSM(
    linkTrainingParams: LinkTrainingParams,
    afeParams: AfeParams,
    maxPatternCount: Int,
) extends Module {

  val io = IO(new Bundle {
    val sbTrainIO = Flipped(new SBMsgWrapperTrainIO)
    val patternGeneratorIO =
      Flipped(new PatternGeneratorIO(afeParams, maxPatternCount))
    val transition = Output(Bool())
    val error = Output(Bool())
  })
  private object State extends ChiselEnum {
    val PARAM, REPAIR_CLK, REPAIR_VAL, IDLE, ERR = Value
  }

  private object ParamSubState extends ChiselEnum {
    val SEND_REQ, WAIT_REQ, SEND_RESP, WAIT_RESP = Value
  }

  // private object RepairClkSubState extends ChiselEnum {
  //   val SEND_INIT_REQ, WAIT_INIT_REQ, SEND_INIT_RESP, WAIT_INIT_RESP,
  //       SEND_CLOCK, WAIT_CLOCK, SEND_RESULT_REQ, WAIT_RESULT_REQ,
  //       SEND_RESULT_RESP, WAIT_RESULT_RESP = Value
  // }

  private val state = RegInit(State.PARAM)
  private val nextState = WireInit(state)
  private val paramSubState = RegInit(ParamSubState.SEND_REQ)
  when(nextState === State.PARAM && state =/= State.PARAM) {
    paramSubState := ParamSubState.SEND_REQ
  }

  io.transition := nextState === State.IDLE || nextState === State.ERR
  io.error := state === State.ERR
  state := nextState

  io.sbTrainIO.msgReq.noenq()
  io.sbTrainIO.msgReqStatus.nodeq()
  io.patternGeneratorIO.transmitReq.noenq()
  io.patternGeneratorIO.resp.nodeq()

  /** Initialize params */
  private val voltageSwing = RegInit(
    linkTrainingParams.mbTrainingParams.voltageSwing.U(5.W),
  )
  private val maxDataRate = RegInit(
    linkTrainingParams.mbTrainingParams.maximumDataRate.U(4.W),
  )
  private val clockMode = RegInit(
    linkTrainingParams.mbTrainingParams.clockMode,
  )
  private val clockPhase = RegInit(
    linkTrainingParams.mbTrainingParams.clockPhase.B,
  )
  // TODO: not implemented
  private val moduleId = RegInit(
    linkTrainingParams.mbTrainingParams.moduleId.U(2.W),
  )
  // TODO: not implemented
  private val ucieAx32 = RegInit(
    linkTrainingParams.mbTrainingParams.ucieAx32.B,
  )

  val sbClockFreq =
    linkTrainingParams.sbClockFreqAnalog / afeParams.sbSerializerRatio

  switch(state) {
    is(State.PARAM) {

      /** TODO: where am i actually setting up the params? */
      def formParamsReqMsg(
          req: Boolean,
          voltageSwing: UInt,
          maxDataRate: UInt,
          clockMode: ClockModeParam.Type,
          clockPhase: Bool,
          moduleId: UInt,
          UCIeAx32: Bool,
      ): MessageRequest = {
        val data = Wire(UInt(64.W))
        val msgReq = Wire(new MessageRequest)
        data := Cat(
          0.U(50.W),
          UCIeAx32,
          moduleId(1, 0),
          clockPhase,
          clockMode.asUInt,
          voltageSwing(4, 0),
          maxDataRate(3, 0),
        )
        msgReq.msg := SBMessage_factory(
          if (req) SBM.MBINIT_PARAM_CONFIG_REQ
          else SBM.MBINIT_PARAM_CONFIG_RESP,
          "PHY",
          remote = true,
          "PHY",
          data,
        )

        msgReq.timeoutCycles := (0.008 * sbClockFreq).toInt.U
        msgReq.reqType := MessageRequestType.EXCHANGE
        msgReq
      }

      val reqData = RegInit(0.U(64.W))

      switch(paramSubState) {
        is(ParamSubState.SEND_REQ) {
          io.sbTrainIO.msgReq.valid := true.B
          io.sbTrainIO.msgReq.bits := formParamsReqMsg(
            true,
            voltageSwing,
            maxDataRate,
            clockMode,
            clockPhase,
            moduleId,
            ucieAx32,
          )
          when(io.sbTrainIO.msgReq.fire) {
            paramSubState := ParamSubState.WAIT_REQ
          }
        }
        is(ParamSubState.WAIT_REQ) {
          io.sbTrainIO.msgReqStatus.ready := true.B
          when(io.sbTrainIO.msgReqStatus.fire) {
            reqData := io.sbTrainIO.msgReqStatus.bits.data
            when(
              io.sbTrainIO.msgReqStatus.bits.status === MessageRequestStatusType.ERR,
            ) {
              nextState := State.ERR
            }.otherwise {
              paramSubState := ParamSubState.SEND_RESP
            }
          }
        }
        is(ParamSubState.SEND_RESP) {
          io.sbTrainIO.msgReq.valid := true.B
          val exchangedMaxDataRate = Wire(UInt(4.W))
          exchangedMaxDataRate := Mux(
            maxDataRate >= reqData(3, 0),
            reqData(3, 0),
            maxDataRate,
          )
          maxDataRate := exchangedMaxDataRate

          io.sbTrainIO.msgReq.bits := formParamsReqMsg(
            false,
            0.U,
            exchangedMaxDataRate,
            ClockModeParam(reqData(9, 9)),
            reqData(10, 10).asBool,
            0.U,
            0.B,
          )
          when(io.sbTrainIO.msgReq.fire) {
            paramSubState := ParamSubState.WAIT_RESP
          }
        }
        is(ParamSubState.WAIT_RESP) {
          io.sbTrainIO.msgReqStatus.ready := true.B
          when(io.sbTrainIO.msgReqStatus.fire) {
            when(
              io.sbTrainIO.msgReqStatus.bits.status === MessageRequestStatusType.ERR,
            ) {
              nextState := State.ERR
            }.otherwise {
              nextState := State.IDLE
            }
          }

        }
      }
    }

  }

}
