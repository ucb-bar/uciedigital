package edu.berkeley.cs.ucie.digital
package logphy

import interfaces._
import sideband._
import chisel3._
import chisel3.util._

/** Implementation TODOs:
  *   - implement plStallReq
  *   - retrain state
  */

case class LinkTrainingParams(
    /** The amount of cycles to wait after driving the PLL frequency */
    pllWaitTime: Int = 100,
    maxSBMessageSize: Int = 128,
    mbTrainingParams: MBTrainingParams = MBTrainingParams(),
    sbClockFreqAnalog: Int = 800_000_000,
    maxPatternCount: Int = 1 << 32,
)

class SidebandFSMIO(
    sbParams: SidebandParams,
) extends Bundle {
  val rxData = Decoupled(Bits(sbParams.sbNodeMsgWidth.W))
  val patternTxData = Flipped(
    Decoupled(Bits(sbParams.sbNodeMsgWidth.W)),
  )
  val packetTxData = Flipped(
    Decoupled(Bits(sbParams.sbNodeMsgWidth.W)),
  )
  val rxMode = Input(RXTXMode())
  val txMode = Input(RXTXMode())
  val rxEn = Input(Bool())
  val pllLock = Output(Bool())
}

class MainbandFSMIO(
    afeParams: AfeParams,
) extends Bundle {
  val rxEn = Input(Bool())
  val pllLock = Output(Bool())
  val txFreqSel = Input(SpeedMode())
  val mainbandIO = new MainbandIO(afeParams)
}

class LinkTrainingFSM(
    linkTrainingParams: LinkTrainingParams,
    sbParams: SidebandParams,
    afeParams: AfeParams,
) extends Module {

  val sbClockFreq =
    linkTrainingParams.sbClockFreqAnalog / afeParams.sbSerializerRatio

  val io = IO(new Bundle {
    val mainbandFSMIO = Flipped(new MainbandFSMIO(afeParams))
    val sidebandFSMIO = Flipped(new SidebandFSMIO(sbParams))
    val rdi = new Bundle {
      val rdiBringupIO = new RdiBringupIO
    }
    val trainingOperationIO =
      Input(
        new TrainingOperation(afeParams, linkTrainingParams.maxPatternCount),
      )
    val currentState = Output(LinkTrainingState())
  })

  val patternGenerator = Module(
    new PatternGenerator(afeParams, sbParams, maxPatternCount = 1024),
  )
  val sbMsgWrapper = Module(new SBMsgWrapper(sbParams))

  private val msgSource = WireInit(MsgSource.PATTERN_GENERATOR)
  io.mainbandFSMIO.mainbandIO <> patternGenerator.io.mainbandIO

  patternGenerator.io.patternGeneratorIO.transmitReq.noenq()
  patternGenerator.io.patternGeneratorIO.resp.nodeq()
  sbMsgWrapper.io.trainIO.msgReq.noenq()
  sbMsgWrapper.io.trainIO.msgReqStatus.nodeq()

  io.sidebandFSMIO.patternTxData <> patternGenerator.io.sidebandLaneIO.txData
  io.sidebandFSMIO.packetTxData <> sbMsgWrapper.io.laneIO.txData
  when(msgSource === MsgSource.PATTERN_GENERATOR) {
    io.sidebandFSMIO.rxData <> patternGenerator.io.sidebandLaneIO.rxData
    sbMsgWrapper.io.laneIO.rxData.noenq()
  }.otherwise {
    io.sidebandFSMIO.rxData <> sbMsgWrapper.io.laneIO.rxData
    patternGenerator.io.sidebandLaneIO.rxData.noenq()
  }

  private val currentState = RegInit(LinkTrainingState.reset)
  private val nextState = WireInit(currentState)

  private object ResetSubState extends ChiselEnum {
    val INIT, FREQ_SEL_CYC_WAIT, FREQ_SEL_LOCK_WAIT = Value
  }
  private val resetSubState = RegInit(ResetSubState.INIT)
  when(
    nextState === LinkTrainingState.reset && currentState =/= LinkTrainingState.reset,
  ) {
    resetSubState := ResetSubState.INIT
  }

  private object SBInitSubState extends ChiselEnum {
    val SEND_CLOCK, WAIT_CLOCK, SB_OUT_OF_RESET_EXCH, SB_OUT_OF_RESET_WAIT,
        SB_DONE_REQ, SB_DONE_REQ_WAIT, SB_DONE_RESP, SB_DONE_RESP_WAIT = Value
  }
  private val sbInitSubState = RegInit(SBInitSubState.SEND_CLOCK)
  when(
    nextState === LinkTrainingState.sbInit && currentState =/= LinkTrainingState.sbInit,
  ) {
    sbInitSubState := SBInitSubState.SEND_CLOCK
  }

  private val mbInit = Module(
    new MBInitFSM(
      linkTrainingParams,
      afeParams,
      maxPatternCount = linkTrainingParams.maxPatternCount,
    ),
  )
  mbInit.reset := ((nextState === LinkTrainingState.mbInit) && (currentState =/= LinkTrainingState.mbInit)) || reset.asBool

  /** initialize MBInit IOs */
  mbInit.io.sbTrainIO.msgReq.nodeq()
  mbInit.io.sbTrainIO.msgReqStatus.noenq()
  mbInit.io.patternGeneratorIO.transmitReq.nodeq()
  mbInit.io.patternGeneratorIO.resp.noenq()

  private val mbTrainer = Module(
    new MBTrainer(
      linkTrainingParams = linkTrainingParams,
      afeParams = afeParams,
      maxPatternCount = linkTrainingParams.maxPatternCount,
    ),
  )
  mbTrainer.reset := ((nextState === LinkTrainingState.mbTrain) && (currentState =/= LinkTrainingState.mbTrain)) || reset.asBool
  mbTrainer.io.sbTrainIO.msgReq.nodeq()
  mbTrainer.io.sbTrainIO.msgReqStatus.noenq()
  mbTrainer.io.patternGeneratorIO.transmitReq.nodeq()
  mbTrainer.io.patternGeneratorIO.resp.noenq()
  mbTrainer.io.trainingOperationIO <> io.trainingOperationIO

  private val rdiBringup = Module(new RdiBringup)
  rdiBringup.io.rdiIO <> io.rdi.rdiBringupIO
  rdiBringup.io.sbTrainIO.msgReq.nodeq()
  rdiBringup.io.sbTrainIO.msgReqStatus.noenq()
  val plStateStatus = WireInit(rdiBringup.io.rdiIO.plStateStatus)

  currentState := PriorityMux(
    Seq(
      (rdiBringup.io.rdiIO.plStateStatus === PhyState.reset, nextState),
      (
        rdiBringup.io.rdiIO.plStateStatus === PhyState.active,
        LinkTrainingState.active,
      ),
      (
        rdiBringup.io.rdiIO.plStateStatus === PhyState.retrain,
        LinkTrainingState.retrain,
      ),
      (
        rdiBringup.io.rdiIO.plStateStatus === PhyState.linkError,
        LinkTrainingState.linkError,
      ),
    ),
  )
  io.sidebandFSMIO.rxMode := Mux(
    currentState === LinkTrainingState.sbInit &&
      (sbInitSubState === SBInitSubState.SEND_CLOCK ||
        sbInitSubState === SBInitSubState.WAIT_CLOCK ||
        sbInitSubState === SBInitSubState.SB_OUT_OF_RESET_EXCH ||
        sbInitSubState === SBInitSubState.SB_OUT_OF_RESET_WAIT),
    RXTXMode.RAW,
    RXTXMode.PACKET,
  )
  io.sidebandFSMIO.txMode := io.sidebandFSMIO.rxMode

  /** TODO: should these ever be false? */
  io.sidebandFSMIO.rxEn := true.B
  io.mainbandFSMIO.rxEn := (currentState =/= LinkTrainingState.reset)

  /** TODO: what is default speed selection? */
  io.mainbandFSMIO.txFreqSel := SpeedMode.speed4
  io.currentState := currentState
  val resetFreqCtrValue = WireInit(false.B)
  resetFreqCtrValue := false.B

  rdiBringup.io.internalError := currentState === LinkTrainingState.linkError

  /** TODO: need to set accurately */
  rdiBringup.io.internalRetrain := false.B

  private object ActiveSubState extends ChiselEnum {
    val IDLE = Value
  }
  private val activeSubState = RegInit(ActiveSubState.IDLE)
  when(
    currentState =/= LinkTrainingState.active && nextState === LinkTrainingState.active,
  ) {
    activeSubState := ActiveSubState.IDLE
  }

  switch(currentState) {
    is(LinkTrainingState.reset) {
      io.mainbandFSMIO.rxEn := false.B
      io.sidebandFSMIO.rxEn := true.B
      val (freqSelCtrValue, _) = Counter(
        (1 until linkTrainingParams.pllWaitTime),
        reset = resetFreqCtrValue,
      )
      switch(resetSubState) {
        is(ResetSubState.INIT) {
          when(io.mainbandFSMIO.pllLock && io.sidebandFSMIO.pllLock) {
            io.mainbandFSMIO.txFreqSel := SpeedMode.speed4
            resetSubState := ResetSubState.FREQ_SEL_CYC_WAIT
            resetFreqCtrValue := true.B
          }
        }
        is(ResetSubState.FREQ_SEL_CYC_WAIT) {
          when(freqSelCtrValue === (linkTrainingParams.pllWaitTime - 1).U) {
            resetSubState := ResetSubState.FREQ_SEL_LOCK_WAIT
          }
        }
        is(ResetSubState.FREQ_SEL_LOCK_WAIT) {
          when(
            io.mainbandFSMIO.pllLock && io.sidebandFSMIO.pllLock,
            /** TODO: what is "Local SoC/Firmware not keeping the Physical Layer
              * in RESET"
              */
          ) {
            nextState := LinkTrainingState.sbInit
          }
        }

      }
    }
    is(LinkTrainingState.sbInit) {

      /** UCIe Module mainband (MB) transmitters remain tri-stated, SB
        * Transmitters continue to be held Low, SB Receivers continue to be
        * enabled
        */

      switch(sbInitSubState) {
        is(SBInitSubState.SEND_CLOCK) {
          patternGenerator.io.patternGeneratorIO.transmitReq.bits.pattern := TransmitPattern.CLOCK

          /** Timeout occurs after 8ms */
          patternGenerator.io.patternGeneratorIO.transmitReq.bits.timeoutCycles := (
            0.008 * sbClockFreq,
          ).toInt.U
          patternGenerator.io.patternGeneratorIO.transmitReq.bits.patternCountMax := (128 + 64 * 4).U
          patternGenerator.io.patternGeneratorIO.transmitReq.bits.patternDetectedCountMax := (128).U

          patternGenerator.io.patternGeneratorIO.transmitReq.valid := true.B
          msgSource := MsgSource.PATTERN_GENERATOR
          when(patternGenerator.io.patternGeneratorIO.transmitReq.fire) {
            sbInitSubState := SBInitSubState.WAIT_CLOCK
          }
        }
        is(SBInitSubState.WAIT_CLOCK) {
          patternGenerator.io.patternGeneratorIO.resp.ready := true.B
          msgSource := MsgSource.PATTERN_GENERATOR
          when(
            patternGenerator.io.patternGeneratorIO.resp.fire,
          ) {
            switch(
              patternGenerator.io.patternGeneratorIO.resp.bits.status,
            ) {
              is(MessageRequestStatusType.SUCCESS) {
                sbInitSubState := SBInitSubState.SB_OUT_OF_RESET_EXCH
              }
              is(MessageRequestStatusType.ERR) {
                nextState := LinkTrainingState.linkError
              }
            }
          }
        }
        is(SBInitSubState.SB_OUT_OF_RESET_EXCH) {
          val bitPat = SBM.SBINIT_OUT_OF_RESET
          val reqType = MessageRequestType.EXCHANGE
          val timeout = (0.008 * sbClockFreq).toInt
          sendSidebandReq(bitPat, reqType, true, timeout)

          when(sbMsgWrapper.io.trainIO.msgReq.fire) {
            sbInitSubState := SBInitSubState.SB_OUT_OF_RESET_WAIT
          }
        }
        is(SBInitSubState.SB_OUT_OF_RESET_WAIT) {
          sbMsgWrapper.io.trainIO.msgReqStatus.ready := true.B
          msgSource := MsgSource.SB_MSG_WRAPPER
          when(sbMsgWrapper.io.trainIO.msgReqStatus.fire) {
            switch(sbMsgWrapper.io.trainIO.msgReqStatus.bits.status) {
              is(MessageRequestStatusType.SUCCESS) {
                sbInitSubState := SBInitSubState.SB_DONE_REQ
              }
              is(MessageRequestStatusType.ERR) {
                nextState := LinkTrainingState.linkError
              }
            }
          }
        }
        is(SBInitSubState.SB_DONE_REQ) {
          sendSidebandReq(
            SBM.SBINIT_DONE_REQ,
            MessageRequestType.EXCHANGE,
            false,
            (0.008 * sbClockFreq).toInt,
          )
          when(sbMsgWrapper.io.trainIO.msgReq.fire) {
            sbInitSubState := SBInitSubState.SB_DONE_REQ_WAIT
          }
        }
        is(SBInitSubState.SB_DONE_REQ_WAIT) {
          sbMsgWrapper.io.trainIO.msgReqStatus.ready := true.B
          msgSource := MsgSource.SB_MSG_WRAPPER
          when(sbMsgWrapper.io.trainIO.msgReqStatus.fire) {
            switch(sbMsgWrapper.io.trainIO.msgReqStatus.bits.status) {
              is(MessageRequestStatusType.SUCCESS) {
                sbInitSubState := SBInitSubState.SB_DONE_RESP
              }
              is(MessageRequestStatusType.ERR) {
                nextState := LinkTrainingState.linkError
              }
            }
          }
        }
        is(SBInitSubState.SB_DONE_RESP) {
          sendSidebandReq(
            SBM.SBINIT_DONE_RESP,
            MessageRequestType.EXCHANGE,
            false,
            (0.008 * sbClockFreq).toInt,
          )
          when(sbMsgWrapper.io.trainIO.msgReq.fire) {
            sbInitSubState := SBInitSubState.SB_DONE_RESP_WAIT
          }
        }
        is(SBInitSubState.SB_DONE_RESP_WAIT) {
          sbMsgWrapper.io.trainIO.msgReqStatus.ready := true.B
          msgSource := MsgSource.SB_MSG_WRAPPER
          when(sbMsgWrapper.io.trainIO.msgReqStatus.fire) {
            switch(sbMsgWrapper.io.trainIO.msgReqStatus.bits.status) {
              is(MessageRequestStatusType.SUCCESS) {
                nextState := LinkTrainingState.mbInit
              }
              is(MessageRequestStatusType.ERR) {
                nextState := LinkTrainingState.linkError
              }
            }
          }
        }
      }
    }
    is(LinkTrainingState.mbInit) {

      mbInit.io.sbTrainIO <> sbMsgWrapper.io.trainIO
      mbInit.io.patternGeneratorIO <> patternGenerator.io.patternGeneratorIO
      msgSource := MsgSource.SB_MSG_WRAPPER
      when(mbInit.io.transition) {
        nextState := Mux(
          mbInit.io.error,
          LinkTrainingState.linkError,
          LinkTrainingState.mbTrain,
        )
      }
    }
    is(LinkTrainingState.mbTrain) {

      mbTrainer.io.sbTrainIO <> sbMsgWrapper.io.trainIO
      sbMsgWrapper.reset := mbTrainer.io.sbMsgWrapperReset
      mbTrainer.io.patternGeneratorIO <> patternGenerator.io.patternGeneratorIO
      msgSource := MsgSource.SB_MSG_WRAPPER
      when(mbTrainer.io.complete) {
        nextState := Mux(
          mbTrainer.io.err,
          LinkTrainingState.linkError,
          LinkTrainingState.linkInit,
        )
      }

    }
    is(LinkTrainingState.linkInit) {
      rdiBringup.io.sbTrainIO <> sbMsgWrapper.io.trainIO
      msgSource := MsgSource.SB_MSG_WRAPPER
      when(rdiBringup.io.active) {
        nextState := LinkTrainingState.active
      }
    }
    is(LinkTrainingState.active) {
      switch(activeSubState) {
        is(ActiveSubState.IDLE) {
          when(nextState =/= LinkTrainingState.active) {}
        }
      }

    }
    is(LinkTrainingState.linkError) {
      // TODO: What to do when I receive an error?
    }
  }

  private def sendSidebandReq(
      bitPat: BitPat,
      reqType: MessageRequestType.Type,
      repeat: Boolean,
      timeout: Int,
  ): Unit = {
    sbMsgWrapper.io.trainIO.msgReq.bits.msg := SBMessage_factory(
      bitPat,
      "PHY",
      true,
      "PHY",
    )
    sbMsgWrapper.io.trainIO.msgReq.bits.reqType := reqType
    sbMsgWrapper.io.trainIO.msgReq.valid := true.B
    sbMsgWrapper.io.trainIO.msgReq.bits.timeoutCycles := timeout.U
    sbMsgWrapper.io.trainIO.msgReq.bits.repeat := repeat.B
    msgSource := MsgSource.SB_MSG_WRAPPER
  }
}
