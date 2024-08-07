package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import interfaces._

class RdiDataMapperIO(rdiParams: RdiParams) extends Bundle {

  /** Adapter to Physical Layer data.
    *
    * Encompasses lp_irdy, lp_valid, and pl_trdy from the UCIe specification.
    */
  val lpData = Decoupled3(Bits((8 * rdiParams.width).W))

  /** Physical Layer to Adapter data.
    *
    * Encompasses `pl_valid` and `pl_data` from the UCIe specification. Note
    * that backpressure is not possible. Data should be sampled whenever valid
    * is asserted at a clock edge.
    */
  val plData = Flipped(Valid(Bits((8 * rdiParams.width).W)))
}

class RdiDataMapper(
    rdiParams: RdiParams,
    afeParams: AfeParams,
) extends Module {

  val io = IO(new Bundle {
    val rdi = Flipped(new RdiDataMapperIO(rdiParams))
    val mainbandLaneIO = Flipped(new MainbandIO(afeParams))
  })

  assert(afeParams.mbSerializerRatio * afeParams.mbLanes < rdiParams.width * 8)

  /** need to chunk RDI messages, and collect outgoing phy -> d2d */
  val afeBits = (afeParams.mbSerializerRatio * afeParams.mbLanes)
  val ratio =
    (rdiParams.width * 8) / afeBits

  /** collect outgoing phy -> d2d */
  val rxSliceCounter = RegInit(0.U(log2Ceil(ratio).W))
  val rxData =
    RegInit(
      VecInit(
        Seq.fill(ratio)(
          0.U(afeBits.W),
        ),
      ),
    )
  val hasRxData = RegInit(false.B)
  hasRxData := false.B
  when(io.mainbandLaneIO.rxData.fire) {

    /** chunk */
    rxData(rxSliceCounter) := io.mainbandLaneIO.rxData.bits
    rxSliceCounter := rxSliceCounter + 1.U
    when(rxSliceCounter === (ratio - 1).U) {
      hasRxData := true.B
      rxSliceCounter := 0.U
    }
  }
  io.rdi.plData.valid := hasRxData
  io.rdi.plData.bits := rxData.asUInt

  /** chunk RDI message to transmit */
  private val txWidthCoupler = Module(
    new DataWidthCoupler(
      DataWidthCouplerParams(
        inWidth = rdiParams.width * 8,
        outWidth = afeBits,
      ),
    ),
  )
  txWidthCoupler.io.out <> io.mainbandLaneIO.txData

  io.rdi.lpData.ready := txWidthCoupler.io.in.ready
  txWidthCoupler.io.in.valid := io.rdi.lpData.valid & io.rdi.lpData.irdy
  txWidthCoupler.io.in.bits := io.rdi.lpData.bits

  /** RDI has no backpressure mechanism */
  io.mainbandLaneIO.rxData.ready := true.B

}
