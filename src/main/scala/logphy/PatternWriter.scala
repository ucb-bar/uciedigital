package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import sideband.SidebandParams
import interfaces._

class PatternWriter(
    sbParams: SidebandParams,
    afeParams: AfeParams,
    maxPatternCount: Int,
) extends Module {
  val maxPatternCountWidth = log2Ceil(maxPatternCount + 1)
  val io = IO(new Bundle {
    val request = Flipped(Valid(new Bundle {
      val pattern = TransmitPattern()
      val sideband = Bool()
      val patternCountMax = UInt(maxPatternCountWidth.W)
    }))
    val resp = Output(new Bundle {
      val complete = Bool()
      val inProgress = Bool()
    })
    val sbTxData = Decoupled(Bits(sbParams.sbNodeMsgWidth.W))
    val mbTxData =
      Decoupled(Bits((afeParams.mbLanes * afeParams.mbSerializerRatio).W))
  })

  private val writeInProgress = RegInit(false.B)
  io.resp.inProgress := writeInProgress
  when(io.request.valid && !writeInProgress) {
    writeInProgress := true.B
  }

  val patternToTransmit = WireInit(
    0.U(
      (afeParams.mbLanes * afeParams.mbSerializerRatio)
        .max(sbParams.sbNodeMsgWidth)
        .W,
    ),
  )

  val patternWrittenCount = RegInit(0.U(maxPatternCountWidth.W))
  io.resp.complete := patternWrittenCount >= io.request.bits.patternCountMax
  val patternWritten = WireInit(false.B)

  io.sbTxData.noenq()
  io.mbTxData.noenq()
  when(!io.request.bits.sideband) {
    io.mbTxData.valid := writeInProgress
    io.mbTxData.bits := patternToTransmit
    when(io.mbTxData.fire) {
      patternWrittenCount := patternWrittenCount + (afeParams.mbLanes * afeParams.mbSerializerRatio).U
      patternWritten := true.B
    }
  }.otherwise {
    io.sbTxData.valid := writeInProgress
    io.sbTxData.bits := patternToTransmit
    when(io.sbTxData.fire) {
      patternWrittenCount := patternWrittenCount + sbParams.sbNodeMsgWidth.U
      patternWritten := true.B
    }

  }

  val lfsrPatternGenerator =
    Module(
      new UCIeScrambler(afeParams, numLanes = afeParams.mbLanes),
    )
  lfsrPatternGenerator.io.data_in := VecInit(Seq.fill(afeParams.mbLanes)(0.U))
  lfsrPatternGenerator.io.valid := false.B

  when(writeInProgress) {
    switch(io.request.bits.pattern) {

      /** Patterns may be different lengths, etc. so may be best to handle
        * separately, for now
        */

      is(TransmitPattern.CLOCK) {

        /** SB clock gating is implemented in the serializer, so just send 128
          * bits of regular clock data TODO: currently not long enough to use in
          * MB, if ever used in MB need to make longer
          */
        patternToTransmit := "haaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa".U
      }
      is(TransmitPattern.LFSR) {
        assert(io.request.bits.sideband === false.B)
        val ratioBytes = afeParams.mbSerializerRatio / 8
        val patternBytes = VecInit(
          Seq.fill(afeParams.mbLanes * ratioBytes)(0.U(8.W)),
        )
        for (i <- 0 until ratioBytes) {
          for (j <- 0 until afeParams.mbLanes) {
            patternBytes(i * afeParams.mbLanes + j) := lfsrPatternGenerator.io
              .data_out(j)
              .asTypeOf(VecInit(Seq.fill(ratioBytes)(0.U(8.W))))(i)
          }
        }
        patternToTransmit := patternBytes.asUInt
        when(patternWritten) {
          lfsrPatternGenerator.io.valid := true.B
        }
      }
      is(TransmitPattern.VALTRAIN) {
        val valtrain = VecInit(
          Seq.fill(afeParams.mbLanes * afeParams.mbSerializerRatio / 8)(
            "b1111_0000".U(8.W),
          ),
        )
        patternToTransmit := valtrain.asUInt
      }
      is(TransmitPattern.PER_LANE_ID) {
        val perLaneId = VecInit(Seq.fill(afeParams.mbLanes)(0.U(16.W)))
        for (i <- 0 until afeParams.mbLanes) {
          perLaneId(i) := Cat("b1010".U(4.W), i.U(8.W), "b1010".U(4.W))
        }
        val ratio16 = afeParams.mbSerializerRatio / 16
        val patternVec = VecInit.tabulate(afeParams.mbLanes, ratio16) {
          (_, _) => 0.U(16.W)
        }
        for (i <- 0 until afeParams.mbLanes) {
          for (j <- 0 until ratio16) {
            patternVec(i)(j) := perLaneId(i)
          }
        }
        patternToTransmit := patternVec.asUInt
      }

    }

  }

}
