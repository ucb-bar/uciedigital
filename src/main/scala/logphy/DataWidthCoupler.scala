package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._

case class DataWidthCouplerParams(
    val inWidth: Int = 4,
    val outWidth: Int = 4,
)

class DataWidthCouplerIO(
    params: DataWidthCouplerParams,
) extends Bundle {
  val in = Flipped(Decoupled(UInt(params.inWidth.W)))
  val out = Decoupled(UInt(params.outWidth.W))
}

class DataWidthCoupler(params: DataWidthCouplerParams) extends Module {
  val io = IO(
    new DataWidthCouplerIO(params),
  )
  private object State extends ChiselEnum {
    val IDLE, CHUNK_OR_COLLECT = Value
  }

  private val currentState = RegInit(State.IDLE)
  io.out.noenq()
  io.in.nodeq()

  if (params.inWidth > params.outWidth) {
    val ratio = params.inWidth / params.outWidth
    assert(
      params.inWidth % params.outWidth == 0,
      "params.inWidth must be a multiple of params.outWidth",
    )

    /** need to chunk incoming message */

    val chunkCounter = RegInit(0.U(log2Ceil(ratio).W))
    val inData = RegInit(0.U(params.inWidth.W))
    switch(currentState) {
      is(State.IDLE) {
        io.in.ready := true.B
        when(io.in.fire) {
          inData := io.in.bits
          chunkCounter := 0.U
          currentState := State.CHUNK_OR_COLLECT
        }
      }
      is(State.CHUNK_OR_COLLECT) {
        io.out.bits := inData
          .asTypeOf(Vec(ratio, Bits(params.outWidth.W)))(
            chunkCounter,
          )
        io.out.valid := true.B
        when(io.out.fire) {
          chunkCounter := chunkCounter + 1.U
          when(chunkCounter === (ratio - 1).U) {
            currentState := State.IDLE
          }
        }
      }
    }
  } else {
    assert(
      params.outWidth % params.inWidth == 0,
      "params.outWidth must be a multiple of params.inWidth",
    )

    val ratio = params.outWidth / params.inWidth

    /** need to collect incoming message */

    val inSliceCounter = RegInit(0.U(log2Ceil(ratio).W))
    val inData =
      RegInit(
        VecInit(
          Seq.fill(ratio)(
            0.U(params.inWidth.W),
          ),
        ),
      )

    switch(currentState) {
      is(State.IDLE) {
        io.in.ready := true.B
        when(io.in.fire) {
          inData(inSliceCounter) := io.in.bits
          inSliceCounter := inSliceCounter + 1.U
        }
        when(inSliceCounter === (ratio - 1).U) {
          inSliceCounter := 0.U
          currentState := State.CHUNK_OR_COLLECT
        }
      }
      is(State.CHUNK_OR_COLLECT) {
        io.out.valid := true.B
        io.out.bits := inData.asUInt
        when(io.out.fire) {
          currentState := State.IDLE
        }
      }
    }

  }

}
