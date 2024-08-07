package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import interfaces.AfeParams
import sideband.SidebandParams

class PatternReader(
    sbParams: SidebandParams,
    afeParams: AfeParams,
    maxPatternCount: Int,
) extends Module {
  private val maxPatternWidth = log2Ceil(maxPatternCount + 1)
  val io = IO(new Bundle {
    val request = Flipped(Valid(new Bundle {
      val pattern = TransmitPattern()
      val patternCountMax = UInt(maxPatternWidth.W)
    }))
    val resp = new Bundle {
      val complete = Output(Bool())
      val inProgress = Output(Bool())
      val errorCount = Output(
        Vec(afeParams.mbLanes, UInt(maxPatternWidth.W)),
      )
    }
    val sbRxData = Flipped(Decoupled(Bits(sbParams.sbNodeMsgWidth.W)))
    val mbRxData = Flipped(
      Decoupled(Vec(afeParams.mbLanes, UInt(afeParams.mbSerializerRatio.W))),
    )
  })

  private val readInProgress = RegInit(false.B)
  private val patternDetectedCount = RegInit(0.U(maxPatternWidth.W))
  io.resp.inProgress := readInProgress
  io.resp.complete := patternDetectedCount >= io.request.bits.patternCountMax

  when(io.request.valid && !readInProgress) {
    readInProgress := true.B
  }

  /** increment error count */
  private val errorCount = RegInit(
    VecInit(Seq.fill(afeParams.mbLanes)(0.U(maxPatternWidth.W))),
  )
  io.resp.errorCount := errorCount
  private val errorCounter = Module(
    new ErrorCounter(afeParams),
  )
  errorCounter.io.req.valid := false.B
  errorCounter.io.req.bits := DontCare
  when(readInProgress) {
    when(errorCounter.io.req.valid) {
      for (i <- 0 until afeParams.mbLanes) {
        errorCount(i) := errorCount(i) + errorCounter.io.errorCount(i)
      }
    }
  }

  io.mbRxData.nodeq()
  io.sbRxData.nodeq()
  val sideband = io.request.bits.pattern === TransmitPattern.CLOCK
  when(readInProgress) {
    when(sideband) {
      io.sbRxData.ready := true.B
      when(io.sbRxData.fire) {
        val patternToDetect = WireInit(
          ("h" + "aaaa" * (sbParams.sbNodeMsgWidth / 16)).U(
            sbParams.sbNodeMsgWidth.W,
          ),
        )

        /** just do an exact comparison */
        when(io.sbRxData.bits === patternToDetect) {
          patternDetectedCount := patternDetectedCount + sbParams.sbNodeMsgWidth.U
        }
      }
    }.otherwise {
      io.mbRxData.ready := true.B
      when(io.mbRxData.fire) {
        errorCounter.io.req.bits.input := io.mbRxData.bits
        errorCounter.io.req.bits.pattern := io.request.bits.pattern
        errorCounter.io.req.valid := true.B
        patternDetectedCount := patternDetectedCount + (afeParams.mbLanes * afeParams.mbSerializerRatio).U
      }
    }
  }
}
