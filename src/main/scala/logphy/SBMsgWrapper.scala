package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import sideband.SidebandParams

class SBMsgWrapperTrainIO(
) extends Bundle {
  val msgReq = Flipped(Decoupled(new MessageRequest))
  val msgReqStatus = Decoupled(new MessageRequestStatus)
}

class SBMsgWriter(sbParams: SidebandParams) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Valid(UInt(128.W)))
    val result = Valid(MessageRequestStatusType())
    val txData = Decoupled(Bits(sbParams.sbNodeMsgWidth.W))
  })
  val inProgress = RegInit(false.B)
  val complete = RegInit(false.B)
  when(io.req.fire && !inProgress) {
    inProgress := true.B
    complete := false.B
  }
  io.txData.valid := inProgress
  io.txData.bits := io.req.bits
  when(inProgress && io.txData.fire) {

    /** continuously resend */
    complete := true.B
  }
  io.result.valid := complete || io.txData.fire
  io.result.bits := MessageRequestStatusType.SUCCESS
}

class SBMsgReader(sbParams: SidebandParams) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Valid(UInt(128.W)))
    val result = Valid(new MessageRequestStatus)
    val rxData = Flipped(Decoupled(Bits(sbParams.sbNodeMsgWidth.W)))
  })

  def messageIsEqual(m1: UInt, m2: UInt): Bool = {

    /** opcode */
    (m1(4, 0) === m2(4, 0)) &&
    /** subcode */
    (m1(21, 14) === m2(21, 14)) &&
    /** code */
    (m1(39, 32) === m2(39, 32))
  }

  val inProgress = RegInit(false.B)
  val complete = RegInit(false.B)
  when(io.req.fire && !inProgress) {
    inProgress := true.B
    complete := false.B
  }

  /** if receive message, move on */
  io.rxData.ready := inProgress
  val data = RegInit(0.U(64.W))
  val justReceivedMsg = Wire(Bool())
  justReceivedMsg := io.rxData.fire &&
    messageIsEqual(
      io.rxData.bits,
      io.req.bits,
    )

  when(inProgress && justReceivedMsg) {
    data := io.rxData.bits(127, 64)
    complete := true.B
    inProgress := false.B
  }

  io.result.valid := complete
  io.result.bits.status := MessageRequestStatusType.SUCCESS
  io.result.bits.data := data
}

class SBMsgWrapper(
    sbParams: SidebandParams,
) extends Module {
  val io = IO(new Bundle {
    val trainIO = new SBMsgWrapperTrainIO
    val laneIO = Flipped(new SidebandLaneIO(sbParams))
  })

  val sbMsgWriter = Module(new SBMsgWriter(sbParams))
  val sbMsgReader = Module(new SBMsgReader(sbParams))

  private object State extends ChiselEnum {
    val IDLE, EXCHANGE, RECEIVE_ONLY, SEND_ONLY, WAIT_ACK = Value
  }

  private val currentState = RegInit(State.IDLE)
  private val timeoutCounter = RegInit(0.U(64.W))

  private val nextState = WireInit(currentState)
  currentState := nextState

  when(currentState =/= nextState) {
    timeoutCounter := 0.U
    sbMsgReader.reset := true.B
    sbMsgWriter.reset := true.B
  }

  private val currentReq = RegInit(0.U((new MessageRequest).msg.getWidth.W))
  private val currentReqTimeoutMax = RegInit(0.U(64.W))
  private val currentStatus = RegInit(MessageRequestStatusType.ERR)

  private val dataOut = RegInit(0.U(64.W))
  io.trainIO.msgReqStatus.bits.data := dataOut
  io.trainIO.msgReqStatus.bits.status := currentStatus
  io.trainIO.msgReqStatus.valid := false.B
  io.trainIO.msgReq.nodeq()

  sbMsgReader.io.rxData <> io.laneIO.rxData
  sbMsgWriter.io.txData <> io.laneIO.txData
  sbMsgReader.io.req.bits := currentReq
  sbMsgWriter.io.req.bits := currentReq
  sbMsgReader.io.req.valid := false.B
  sbMsgWriter.io.req.valid := false.B
  private val requestToState = Seq(
    MessageRequestType.EXCHANGE -> State.EXCHANGE,
    MessageRequestType.SEND -> State.SEND_ONLY,
    MessageRequestType.RECEIVE -> State.RECEIVE_ONLY,
  )

  switch(currentState) {
    is(State.IDLE) {
      io.trainIO.msgReq.ready := true.B
      when(io.trainIO.msgReq.fire) {
        currentReq := io.trainIO.msgReq.bits.msg
        currentReqTimeoutMax := io.trainIO.msgReq.bits.timeoutCycles
        nextState := MuxLookup(io.trainIO.msgReq.bits.reqType, State.EXCHANGE)(
          requestToState,
        )
      }
    }
    is(State.EXCHANGE) {
      sbMsgReader.io.req.valid := true.B
      sbMsgWriter.io.req.valid := true.B

      when(sbMsgWriter.io.result.valid && sbMsgReader.io.result.valid) {
        dataOut := sbMsgReader.io.result.bits.data
        currentStatus := MessageRequestStatusType.SUCCESS
        nextState := State.WAIT_ACK
      }

      /** timeout logic */
      timeoutCounter := timeoutCounter + 1.U
      when(timeoutCounter === currentReqTimeoutMax) {
        nextState := State.WAIT_ACK
        currentStatus := MessageRequestStatusType.ERR
      }
    }
    is(State.RECEIVE_ONLY) {
      sbMsgReader.io.req.valid := true.B
      when(sbMsgReader.io.result.valid) {
        dataOut := sbMsgReader.io.result.bits.data
        currentStatus := MessageRequestStatusType.SUCCESS
        nextState := State.WAIT_ACK
      }
    }
    is(State.SEND_ONLY) {
      sbMsgWriter.io.req.valid := true.B
      when(sbMsgWriter.io.result.valid) {
        currentStatus := MessageRequestStatusType.SUCCESS
      }

      timeoutCounter := timeoutCounter + 1.U
      when(timeoutCounter === currentReqTimeoutMax) {
        nextState := State.WAIT_ACK
        currentStatus := MessageRequestStatusType.ERR
      }

    }
    is(State.WAIT_ACK) {
      io.trainIO.msgReqStatus.valid := true.B
      when(io.trainIO.msgReqStatus.fire) {
        nextState := State.IDLE
      }
    }
  }

}
