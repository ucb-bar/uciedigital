package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor

case class DataWidthCouplerParams(
    val inWidth: Int = 4,
    val outWidth: Int = 4,
)

class WidthCoupleData(
    params: DataWidthCouplerParams,
) extends Bundle {
  val data = UInt(params.outWidth.W)
}

class DataWithValid(
    params: DataWidthCouplerParams,
) extends WidthCoupleData(params) {
  assert(params.outWidth > params.inWidth)
  val valid = Vec(params.outWidth, Bool())
}

class DataWidthCouplerIO(
    params: DataWidthCouplerParams,
) extends Bundle {
  val in = Flipped(Decoupled(UInt(params.inWidth.W)))
  val out = Decoupled(new WidthCoupleData(params))
}

class DataWidthValidIO(
    params: DataWidthCouplerParams,
) extends DataWidthCouplerIO(params) {
  override val in = Flipped(Decoupled(UInt(params.inWidth.W)))
  override val out = Decoupled(new DataWithValid(params))
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
        io.out.bits.data := inData
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

class DataWidthValidFramer(params: DataWidthCouplerParams) extends Module {

  val io = IO(
    new DataWidthValidIO(params),
  )

  private object State extends ChiselEnum {
    val IDLE, COLLECT, FULL = Value
  }

  private val currentState = RegInit(State.IDLE)
  io.out.noenq()
  io.in.nodeq()

  assert(
    params.outWidth % params.inWidth == 0,
    "params.outWidth must be a multiple of params.inWidth",
  )

  assert(
    params.outWidth > params.inWidth,
    "params.outWidth must be greater than in width for valid framing",
  )

  val ratio = params.outWidth / params.inWidth

  /** need to collect incoming message */

  private class DataValid extends Bundle {
    val data = UInt(params.inWidth.W)
    val valid = Vec(params.inWidth, Bool())
  }

  val inSliceCounter = RegInit(0.U(log2Ceil(ratio).W))
  private val invalidData = (new DataValid()).Lit(
    _.valid -> Vec.Lit(Seq.fill(params.inWidth)(false.B): _*),
    _.data -> 0.U,
  )

  private val inDataWire = Wire(Vec(ratio, new DataValid()))
  private val inData =
    RegInit(
      VecInit(
        Seq.fill(ratio)(invalidData),
      ),
    )
  inDataWire := inData
  inData := inDataWire

  printf(cf"inSliceCounter: $inSliceCounter\n")
  printf(cf"currentState: $currentState\n")
  printf(cf"io.out.bits.valid: ${io.out.bits.valid}\n")

  io.out.bits.data := VecInit(inDataWire.map(_.data)).asUInt
  io.out.bits.valid := VecInit(inDataWire.map(_.valid))
    .asTypeOf(io.out.bits.valid)
  switch(currentState) {
    is(State.IDLE) {
      io.in.ready := true.B
      when(io.in.fire) {
        val newData = Wire(new DataValid())
        newData.data := io.in.bits
        newData.valid := Vec.Lit(
          Seq.fill(params.inWidth)(true.B): _*,
        )

        inDataWire(0) := newData
        io.out.valid := true.B
        when(!io.out.fire) {
          inSliceCounter := 1.U
          currentState := State.COLLECT
        }.otherwise {
          inData := VecInit(Seq.fill(ratio)(invalidData))
        }
      }
    }
    is(State.COLLECT) {
      io.out.valid := true.B
      io.in.ready := true.B
      when(io.in.fire) {
        val newData = Wire(new DataValid())
        newData.data := io.in.bits
        newData.valid := Vec.Lit(
          Seq.fill(params.inWidth)(true.B): _*,
        )

        inDataWire(inSliceCounter) := newData
        inSliceCounter := inSliceCounter + 1.U
        when(inSliceCounter === (ratio - 1).U) {
          inSliceCounter := 0.U
          currentState := State.FULL
        }
      }
      when(io.out.fire) {
        inData := VecInit(Seq.fill(ratio)(invalidData))
        currentState := State.IDLE
      }
      printf(cf"inDataWire: ${inDataWire}\n")
    }
    is(State.FULL) {
      io.out.valid := true.B
      when(io.out.fire) {
        inData := VecInit(Seq.fill(ratio)(invalidData))
        currentState := State.IDLE
      }
    }
  }

}
