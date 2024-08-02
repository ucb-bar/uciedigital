package edu.berkeley.cs.ucie.digital
package interfaces

import chisel3._
import chisel3.util._

class FifoParams extends Bundle {
  val clk = Clock()
  val reset = Bool()
}

/** The mainband pins exposed by a standard package UCIe module in one
  * direction.
  */
class MainbandIo(lanes: Int = 16) extends Bundle {
  val data = Bits(lanes.W)
  val valid = Bool()
  val track = Bool()
  val clkp = Clock()
  val clkn = Clock()
}

/** The sideband pins exposed by a standard package UCIe module in one
  * direction.
  */
class SidebandIo extends Bundle {
  val data = Bool()
  val clk = Bool()
}

/** The pins (mainband and sideband) exposed by a standard package UCIe module
  * in one direction.
  */
class UnidirectionalIo(lanes: Int = 16) extends Bundle {
  val mainband = new MainbandIo(lanes)
  val sideband = new SidebandIo()
}

/** The pins (mainband and sideband) exposed by a standard package UCIe module
  * in both directions.
  */
class StandardPackageIo(lanes: Int = 16) extends Bundle {
  val tx = Output(new UnidirectionalIo(lanes))
  val rx = Input(new UnidirectionalIo(lanes))
}

case class AfeParams(
    sbSerializerRatio: Int = 1,
    sbWidth: Int = 1,
    mbSerializerRatio: Int = 16,
    mbLanes: Int = 16,
    STANDALONE: Boolean = true
)

/** The sideband analog front-end (AFE) interface, from the perspective of the
  * logical PHY layer.
  *
  * All signals in this interface are synchronous to the sideband clock (fixed
  * at 800 MHz). As a result, the sideband's `serializerRatio` likely will be
  * different from the mainband's `serializerRatio`.
  */
class SidebandAfeIo(
    afeParams: AfeParams,
) extends Bundle {

  /** Data to transmit on the sideband.
    *
    * Output from the async FIFO.
    */
  val txData = Output(UInt(afeParams.sbWidth.W))
  val txClock = Output(Bool())

  /** Data received on the sideband.
    *
    * Input to the async FIFO.
    */
  val rxData = Input(UInt(afeParams.sbWidth.W))
  val rxClock = Input(Bool())

  /** Enable sideband receivers. */
  val rxEn = Output(Bool())

  /** Sideband PLL Lock.
    *
    * Indicates whether the sideband clock is stable.
    */
  val pllLock = Input(Bool())
}

/** The mainband analog front-end (AFE) interface, from the perspective of the
  * logical PHY layer.
  *
  * All signals in this interface are synchronous to the mainband AFE's digital
  * clock, which is produced by taking a high speed clock from a PLL and
  * dividing its frequency by `serializerRatio`.
  *
  * With half-rate clocking (1 data bit transmitted per UI; 1 UI = 0.5 clock
  * cycles), the PLL clock may be 2, 4, 6, 8, 12, or 16 GHz. With a serializer
  * ratio of 16, this results in a 0.125-1 GHz AFE digital clock.
  */
class MainbandAfeIo(
    afeParams: AfeParams,
) extends Bundle {

  val fifoParams = Input(new FifoParams())

  /** Data to transmit on the mainband. Output from the async FIFO.
    */
  val txData = Decoupled(
    Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W)),
  )

  /** Data received on the mainband. Input to the async FIFO.
    */
  val rxData = Flipped(
    Decoupled(Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W))),
  )

  val txFreqSel = Output(SpeedMode())

  /** Mainband receiver enable.
    */
  val rxEn = Output(Bool())

  /** Mainband PLL Lock. Indicates whether the mainband clock is stable.
    */
  val pllLock = Input(Bool())
}
