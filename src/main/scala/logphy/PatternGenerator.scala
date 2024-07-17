package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import sideband.SidebandParams
import interfaces._

class PatternGeneratorIO extends Bundle {
  val transmitReq = Flipped(Decoupled(new Bundle {
    val pattern = TransmitPattern()
    val timeoutCycles = UInt(32.W)
    val sideband = Bool()
    val patternCountMax = UInt(32.W)
    val patternDetectedCountMax = UInt(32.W)
  }))
  val transmitPatternStatus = Decoupled(MessageRequestStatusType())

}

/** TODO: incorporate reader and writer */
class PatternGenerator(
    afeParams: AfeParams,
    sbParams: SidebandParams,
    maxPatternCount: Int,
) extends Module {
  val io = IO(new Bundle {
    val patternGeneratorIO = new PatternGeneratorIO()

    val mainbandLaneIO = Flipped(new MainbandLaneIO(afeParams))
    val sidebandLaneIO = Flipped(new SidebandLaneIO(sbParams))
  })

  val patternWriter = Module(new PatternWriter(sbParams, afeParams))
  val patternReader = Module(
    new PatternReader(sbParams, afeParams, maxPatternCount),
  )

  private val inProgress = WireInit(
    patternWriter.io.resp.inProgress || patternReader.io.resp.inProgress,
  )
  private val pattern = RegInit(TransmitPattern.CLOCK)
  private val sideband = RegInit(true.B)
  private val timeoutCycles = RegInit(0.U(32.W))
  private val status = RegInit(MessageRequestStatusType.SUCCESS)
  private val statusValid = RegInit(false.B)
  private val patternCountMax = RegInit(0.U(32.W))
  private val patternDetectedCountMax = RegInit(0.U(32.W))

  io.patternGeneratorIO.transmitReq.ready := (inProgress === false.B)
  io.patternGeneratorIO.transmitPatternStatus.valid := statusValid
  io.patternGeneratorIO.transmitPatternStatus.bits := status

  when(io.patternGeneratorIO.transmitReq.fire) {
    pattern := io.patternGeneratorIO.transmitReq.bits.pattern
    sideband := io.patternGeneratorIO.transmitReq.bits.sideband
    timeoutCycles := io.patternGeneratorIO.transmitReq.bits.timeoutCycles
    patternCountMax := io.patternGeneratorIO.transmitReq.bits.patternCountMax
    patternDetectedCountMax := io.patternGeneratorIO.transmitReq.bits.patternDetectedCountMax
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
    }.elsewhen(
      patternWriter.io.resp.complete && patternReader.io.resp.complete,
    ) {
      statusValid := true.B
      status := MessageRequestStatusType.SUCCESS
      patternWriter.reset := true.B
      patternReader.reset := true.B
    }
  }

}
