package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import sideband.SidebandParams
import interfaces._

class PatternGeneratorIO(afeParams: AfeParams, maxPatternCount: Int)
    extends Bundle {
  val maxPatternCountWidth = log2Ceil(maxPatternCount + 1)
  val transmitReq = Flipped(Decoupled(new Bundle {
    val pattern = TransmitPattern()
    val timeoutCycles = UInt(32.W)
    val patternCountMax = UInt(maxPatternCountWidth.W)
    val patternDetectedCountMax = UInt(maxPatternCountWidth.W)
  }))
  val resp = Decoupled(new Bundle {
    val status = MessageRequestStatusType()
    val errorCount = Output(
      Vec(afeParams.mbLanes, UInt(maxPatternCountWidth.W)),
    )
  })

}

class PatternGenerator(
    afeParams: AfeParams,
    sbParams: SidebandParams,
    maxPatternCount: Int,
) extends Module {
  val maxPatternCountWidth = log2Ceil(maxPatternCount + 1)
  val io = IO(new Bundle {
    val patternGeneratorIO = new PatternGeneratorIO(afeParams, maxPatternCount)

    val mainbandIO = Flipped(new MainbandIO(afeParams))
    val sidebandLaneIO = Flipped(new SidebandLaneIO(sbParams))
  })

  val patternWriter = Module(
    new PatternWriter(sbParams, afeParams, maxPatternCount),
  )
  val patternReader = Module(
    new PatternReader(sbParams, afeParams, maxPatternCount),
  )

  patternWriter.io.sbTxData <> io.sidebandLaneIO.txData
  patternWriter.io.mbTxData.map(
    LanesToOne(_, afeParams.mbLanes, afeParams.mbSerializerRatio),
  ) <> io.mainbandIO.txData
  patternReader.io.sbRxData <> io.sidebandLaneIO.rxData
  patternReader.io.mbRxData <> io.mainbandIO.rxData.map(
    OneToLanes(_, afeParams.mbLanes, afeParams.mbSerializerRatio),
  )

  private val inProgress = WireInit(
    patternWriter.io.resp.inProgress || patternReader.io.resp.inProgress,
  )

  private val inputsValid = RegInit(false.B)
  private val pattern = RegInit(TransmitPattern.CLOCK)
  private val timeoutCycles = RegInit(0.U(32.W))
  private val status = RegInit(MessageRequestStatusType.SUCCESS)
  private val errorCount = RegInit(
    VecInit(Seq.fill(afeParams.mbLanes)(0.U(maxPatternCount.W))),
  )
  private val statusValid = RegInit(false.B)
  private val patternCountMax = RegInit(0.U(maxPatternCountWidth.W))
  private val patternDetectedCountMax = RegInit(0.U(maxPatternCountWidth.W))

  patternWriter.io.request.bits.pattern := pattern
  patternWriter.io.request.bits.patternCountMax := patternCountMax
  patternReader.io.request.bits.pattern := pattern
  patternReader.io.request.bits.patternCountMax := patternDetectedCountMax
  patternWriter.io.request.valid := inputsValid
  patternReader.io.request.valid := inputsValid

  io.patternGeneratorIO.transmitReq.ready := (inProgress === false.B)
  io.patternGeneratorIO.resp.valid := statusValid
  io.patternGeneratorIO.resp.bits.status := status
  io.patternGeneratorIO.resp.bits.errorCount := errorCount

  when(io.patternGeneratorIO.transmitReq.fire) {
    pattern := io.patternGeneratorIO.transmitReq.bits.pattern
    timeoutCycles := io.patternGeneratorIO.transmitReq.bits.timeoutCycles
    patternCountMax := io.patternGeneratorIO.transmitReq.bits.patternCountMax
    patternDetectedCountMax := io.patternGeneratorIO.transmitReq.bits.patternDetectedCountMax
    inputsValid := true.B
    statusValid := false.B
  }

  when(io.patternGeneratorIO.resp.fire) {
    statusValid := false.B
  }

  /** handle timeouts and completion */
  when(inProgress) {
    timeoutCycles := timeoutCycles - 1.U
    val timeout = timeoutCycles === 0.U
    val complete =
      patternWriter.io.resp.complete && patternReader.io.resp.complete

    when(timeout || complete) {
      status := Mux(
        timeout,
        MessageRequestStatusType.ERR,
        MessageRequestStatusType.SUCCESS,
      )
      statusValid := true.B
      patternWriter.reset := true.B
      patternReader.reset := true.B
      inputsValid := false.B
      errorCount := patternReader.io.resp.errorCount
    }
  }

}
