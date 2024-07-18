package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import sideband.SidebandParams
import interfaces._

class PatternGeneratorIO(maxPatternCount: Int) extends Bundle {
  val maxPatternCountWidth = log2Ceil(maxPatternCount + 1)
  val transmitReq = Flipped(Decoupled(new Bundle {
    val pattern = TransmitPattern()
    val timeoutCycles = UInt(maxPatternCountWidth.W)
    val sideband = Bool()
    val patternCountMax = UInt(maxPatternCountWidth.W)
    val patternDetectedCountMax = UInt(maxPatternCountWidth.W)
  }))
  val transmitPatternStatus = Decoupled(MessageRequestStatusType())

}

class PatternGenerator(
    afeParams: AfeParams,
    sbParams: SidebandParams,
    maxPatternCount: Int,
) extends Module {
  val maxPatternCountWidth = log2Ceil(maxPatternCount + 1)
  val io = IO(new Bundle {
    val patternGeneratorIO = new PatternGeneratorIO(maxPatternCount)

    val mainbandLaneIO = Flipped(new MainbandLaneIO(afeParams))
    val sidebandLaneIO = Flipped(new SidebandLaneIO(sbParams))
  })

  val patternWriter = Module(
    new PatternWriter(sbParams, afeParams, maxPatternCount),
  )
  val patternReader = Module(
    new PatternReader(sbParams, afeParams, maxPatternCount),
  )

  patternWriter.io.sbTxData <> io.sidebandLaneIO.txData
  patternWriter.io.mbTxData <> io.mainbandLaneIO.txData
  patternReader.io.sbRxData <> io.sidebandLaneIO.rxData
  patternReader.io.mbRxData <> io.mainbandLaneIO.rxData

  private val inProgress = WireInit(
    patternWriter.io.resp.inProgress || patternReader.io.resp.inProgress,
  )
  private val inputsValid = RegInit(false.B)
  private val pattern = RegInit(TransmitPattern.CLOCK)
  private val sideband = RegInit(true.B)
  private val timeoutCycles = RegInit(0.U(maxPatternCountWidth.W))
  private val status = RegInit(MessageRequestStatusType.SUCCESS)
  private val statusValid = RegInit(false.B)
  private val patternCountMax = RegInit(0.U(maxPatternCountWidth.W))
  private val patternDetectedCountMax = RegInit(0.U(maxPatternCountWidth.W))

  patternWriter.io.request.bits.pattern := pattern
  patternWriter.io.request.bits.patternCountMax := patternCountMax
  patternWriter.io.request.bits.sideband := sideband
  patternReader.io.request.bits.pattern := pattern
  patternReader.io.request.bits.patternCountMax := patternDetectedCountMax
  patternReader.io.request.bits.sideband := sideband
  patternWriter.io.request.valid := inputsValid
  patternReader.io.request.valid := inputsValid

  io.patternGeneratorIO.transmitReq.ready := (inProgress === false.B)
  io.patternGeneratorIO.transmitPatternStatus.valid := statusValid
  io.patternGeneratorIO.transmitPatternStatus.bits := status

  when(io.patternGeneratorIO.transmitReq.fire) {
    pattern := io.patternGeneratorIO.transmitReq.bits.pattern
    sideband := io.patternGeneratorIO.transmitReq.bits.sideband
    timeoutCycles := io.patternGeneratorIO.transmitReq.bits.timeoutCycles
    patternCountMax := io.patternGeneratorIO.transmitReq.bits.patternCountMax
    patternDetectedCountMax := io.patternGeneratorIO.transmitReq.bits.patternDetectedCountMax
    inputsValid := true.B
    statusValid := false.B
  }

  when(io.patternGeneratorIO.transmitPatternStatus.fire) {
    statusValid := false.B
  }

  /** handle timeouts and completion */
  when(inProgress) {
    timeoutCycles := timeoutCycles - 1.U
    when(timeoutCycles === 0.U) {
      status := MessageRequestStatusType.ERR
      statusValid := true.B
      patternWriter.reset := true.B
      patternReader.reset := true.B
      inputsValid := false.B
    }.elsewhen(
      patternWriter.io.resp.complete && patternReader.io.resp.complete,
    ) {
      statusValid := true.B
      status := MessageRequestStatusType.SUCCESS
      patternWriter.reset := true.B
      patternReader.reset := true.B
      inputsValid := false.B
    }
  }

}
