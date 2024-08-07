package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import interfaces.AfeParams

/** TODO: need to do per-lane, not just aggregate */
class ErrorCounter(afeParams: AfeParams) extends Module {

  val io = IO(new Bundle {
    val req = Flipped(Valid(new Bundle {
      val pattern = TransmitPattern()
      val input = Input(
        Vec(afeParams.mbLanes, UInt(afeParams.mbSerializerRatio.W)),
      )
    }))
    val errorCount = Output(
      Vec(afeParams.mbLanes, UInt(log2Ceil(afeParams.mbSerializerRatio + 1).W)),
    )
  })

  val lfsr = Module(
    new UCIeScrambler(afeParams = afeParams, numLanes = afeParams.mbLanes),
  )

  lfsr.io.valid := io.req.valid && io.req.bits.pattern === TransmitPattern.LFSR
  lfsr.io.data_in := VecInit(
    Seq.fill(afeParams.mbLanes)(0.U(afeParams.mbSerializerRatio.W)),
  )

  val expected = WireInit(
    VecInit(
      Seq.fill(afeParams.mbLanes)(0.U(afeParams.mbSerializerRatio.W)),
    ),
  )

  /** Assign expected value */
  switch(io.req.bits.pattern) {
    is(TransmitPattern.CLOCK) {
      assert(!io.req.valid, "Cannot do error count with sideband clock pattern")
    }
    is(TransmitPattern.LFSR) {
      expected := lfsr.io.data_out
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
      expected := patternVec.asTypeOf(expected)
    }
    is(TransmitPattern.VALTRAIN) {
      val valtrain = VecInit(
        Seq.fill(afeParams.mbLanes * afeParams.mbSerializerRatio / 8)(
          "b1111_0000".U(8.W),
        ),
      )
      expected := valtrain.asTypeOf(expected)
    }
  }

  /** count errors */
  val diffVec = Wire(
    Vec(afeParams.mbLanes, Vec(afeParams.mbSerializerRatio, UInt(1.W))),
  )
  for (i <- 0 until afeParams.mbLanes) {
    diffVec(i) := (expected(i) ^ io.req.bits.input(i)).asTypeOf(diffVec(i))
    io.errorCount(i) := diffVec(i).reduceTree(_ +& _)
  }

}
