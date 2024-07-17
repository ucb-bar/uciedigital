package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import interfaces.AfeParams

class ErrorCounter(afeParams: AfeParams) extends Module {
  val width = afeParams.mbLanes * afeParams.mbSerializerRatio

  val io = IO(new Bundle {
    val req = Flipped(Valid(new Bundle {
      val pattern = TransmitPattern()
      val input = Input(UInt(width.W))
    }))
    val errorCount = Output(UInt(log2Ceil(width + 1).W))
  })

  val lfsr = Module(
    new UCIeScrambler(afeParams = afeParams, numLanes = afeParams.mbLanes),
  )

  lfsr.io.valid := io.req.valid && io.req.bits.pattern === TransmitPattern.LFSR
  lfsr.io.data_in := VecInit(
    Seq.fill(afeParams.mbLanes)(0.U(afeParams.mbSerializerRatio.W)),
  )

  val expected = WireInit(0.U(width.W))

  /** Assign expected value */
  switch(io.req.bits.pattern) {
    is(TransmitPattern.CLOCK) {
      assert(!io.req.valid, "Cannot do error count with sideband clock pattern")
    }
    is(TransmitPattern.LFSR) {
      val ratioBytes = afeParams.mbSerializerRatio / 8
      val patternBytes = VecInit(
        Seq.fill(afeParams.mbLanes * ratioBytes)(0.U(8.W)),
      )
      for (i <- 0 until ratioBytes) {
        for (j <- 0 until afeParams.mbLanes) {
          patternBytes(i * afeParams.mbLanes + j) := lfsr.io
            .data_out(j)
            .asTypeOf(VecInit(Seq.fill(ratioBytes)(0.U(8.W))))(i)
        }
      }
      expected := patternBytes.asUInt
    }
    is(TransmitPattern.PER_LANE_ID) {
      val perLaneId = VecInit(Seq.fill(afeParams.mbLanes)(0.U(16.W)))
      for (i <- 0 until afeParams.mbLanes) {
        perLaneId(i) := Cat("b1010".U(4.W), i.U(8.W), "b1010".U(4.W))
      }
      val ratio16 = afeParams.mbSerializerRatio / 16
      val patternVec = VecInit.tabulate(afeParams.mbLanes, ratio16) { (_, _) =>
        0.U(16.W)
      }
      for (i <- 0 until afeParams.mbLanes) {
        for (j <- 0 until ratio16) {
          patternVec(i)(j) := perLaneId(i)
        }
      }
      expected := patternVec.asUInt
    }
    is(TransmitPattern.VALTRAIN) {
      val valtrain = VecInit(
        Seq.fill(afeParams.mbLanes * afeParams.mbSerializerRatio / 8)(
          "b1111_0000".U(8.W),
        ),
      )
      expected := valtrain.asUInt
    }
  }

  /** count errors */
  val diffVec = Wire(Vec(width, UInt(1.W)))
  diffVec := (expected ^ io.req.bits.input).asTypeOf(diffVec)
  io.errorCount := diffVec.reduceTree(_ +& _)

}
