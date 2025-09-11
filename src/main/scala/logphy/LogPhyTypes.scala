package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegReadFn, RegWriteFn}
import sideband.SidebandParams
import interfaces._

object LinkTrainingState extends ChiselEnum {
  val reset, sbInit, mbInit, mbTrain, linkInit, active, linkError, retrain =
    Value
}

object MsgSource extends ChiselEnum {
  val PATTERN_GENERATOR, SB_MSG_WRAPPER = Value
}

/** Sideband Types */

class SBExchangeMsg extends Bundle {
  val exchangeMsg = UInt(128.W)
}

object MessageRequestStatusType extends ChiselEnum {
  val SUCCESS, ERR = Value
}

class SBReqMsg extends Bundle {
  val msg = UInt(128.W)
}

object MessageRequestType extends ChiselEnum {
  val EXCHANGE, RECEIVE, SEND = Value
}

class MessageRequest extends Bundle {
  val msg = UInt(128.W)
  val timeoutCycles = UInt(64.W)
  val reqType = MessageRequestType()
  val repeat = Bool()
}

class MessageRequestStatus extends Bundle {
  val status = MessageRequestStatusType()
  val data = UInt(64.W)
}

/** Param Enums */

object ClockModeParam extends ChiselEnum {
  val strobe = Value(0.U)
  val continuous = Value(1.U)
}

object TransmitPattern extends ChiselEnum {
  val LFSR = Value(0.U)
  val PER_LANE_ID = Value(1.U)
  val VALTRAIN = Value(2.U)
  val CLOCK = Value(3.U)
}

class SBIO(params: AfeParams) extends Bundle {

  val fifoParams = Input(new FifoParams())

  /** Data to transmit on the sideband.
    *
    * Output from the async FIFO.
    */
  val txData = Decoupled(Bits(params.sbSerializerRatio.W))
  val txValid = Decoupled(Bool())

  /** Data received on the sideband.
    *
    * Input to the async FIFO.
    */
  val rxData = Flipped(Decoupled(Bits(params.sbSerializerRatio.W)))
}

class MainbandLaneIO(
    afeParams: AfeParams,
) extends Bundle {

  /** Data to transmit on the mainband.
    *
    * Output from the async FIFO.
    *
    * @group data
    */
  val txData = Decoupled(
    Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W)),
  )

  /** Data received on the mainband.
    *
    * Input to the async FIFO.
    *
    * @group data
    */
  val rxData = Flipped(
    Decoupled(Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W))),
  )
}

class MainbandLaneDataValid(afeParams: AfeParams) extends Bundle {
  val data = Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W))
  val valid = Vec(afeParams.mbSerializerRatio, Bool())
}

class MainbandLaneIOWithValid(afeParams: AfeParams) extends Bundle {
  val tx = Decoupled(new MainbandLaneDataValid(afeParams))
  val rx = Flipped(Decoupled(new MainbandLaneDataValid(afeParams)))
}

class MainbandLaneIOWithFifoIO(
    afeParams: AfeParams,
) extends MainbandLaneIO(afeParams) {

  val fifoParams = Input(new FifoParams())

}

class MainbandIO(
    afeParams: AfeParams,
) extends Bundle {

  /** Data to transmit on the mainband.
    */
  val txData = Flipped(
    Decoupled(Bits((afeParams.mbLanes * afeParams.mbSerializerRatio).W)),
  )

  val rxData =
    Decoupled(Bits((afeParams.mbLanes * afeParams.mbSerializerRatio).W))
}

class SidebandLaneIO(
    sbParams: SidebandParams,
) extends Bundle {

  /** Data to transmit on the mainband.
    */
  val txData = Flipped(
    Decoupled(Bits(sbParams.sbNodeMsgWidth.W)),
  )

  val rxData =
    Decoupled(Bits(sbParams.sbNodeMsgWidth.W))
}

class RegisterRWIO[T <: Data](gen: T) extends Bundle {
  val write = Flipped(Decoupled(gen))
  val read = Output(gen)

  def regWrite: RegWriteFn = RegWriteFn((valid, data) => {
    write.valid := valid
    write.bits := data.asTypeOf(gen)
    write.ready
  })

  def regRead: RegReadFn = RegReadFn(read.asUInt)

  def getDataWidth = gen.getWidth

  def regField(desc: RegFieldDesc): RegField = {
    RegField(gen.getWidth, regRead, regWrite, desc)
  }
}

class RegisterRW[T <: Data](val init: T, name: String) {
  val reg: T = RegInit(init).suggestName(name)

  def io = new RegisterRWIO(chiselTypeOf(init))

  def connect(io: RegisterRWIO[T]): Unit = {
    io.write.deq()
    when(io.write.fire) {
      reg := io.write.bits
    }

    io.read := reg
  }
}
