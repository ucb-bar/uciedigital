package edu.berkeley.cs.ucie.digital

import chisel3._
import chisel3.util._
import chisel3.experimental.FlatIO
import interfaces._

import chisel3.stage.ChiselStage

// LatencyPipe from rocket-chip
// https://github.com/chipsalliance/rocket-chip/blob/master/src/main/scala/util/LatencyPipe.scala
class LatencyPipe[T <: Data](typ: T, latency: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(typ))
    val out = Decoupled(typ)
  })

  def doN[S](n: Int, func: S => S, in: S): S =
    (0 until n).foldLeft(in)((last, _) => func(last))

  io.out <> doN(latency, (last: DecoupledIO[T]) => Queue(last, 1, true), io.in)
}

object LatencyPipe {
  def apply[T <: Data](in: DecoupledIO[T], latency: Int): DecoupledIO[T] = {
    val pipe = Module(new LatencyPipe(chiselTypeOf(in.bits), latency))
    pipe.io.in <> in
    pipe.io.out
  }
}

class D2DDummyLoopback(latency: Int = 8) extends Module {
  // 64 bit wide data bus, 8 bit wide side channel bus
  val fdiParams = FdiParams(width = 8, dllpWidth = 8, sbWidth = 8)

  val io = FlatIO(Flipped(new Fdi(fdiParams))) // implicit module clock = lclk

  when(io.lpData.valid) {
    /* For now, the protocol layer must assert lpData.valid and lpData.irdy
     * together */
    chisel3.assert(
      io.lpData.irdy,
      "lpData.valid was asserted without lpData.irdy",
    )
  }

  // Restructure lpData as Decoupled[UInt]
  val lpDataDecoupled = Wire(DecoupledIO(chiselTypeOf(io.lpData.bits)))
  lpDataDecoupled.bits := io.lpData.bits
  lpDataDecoupled.valid := io.lpData.valid
  io.lpData.ready := lpDataDecoupled.ready

  // Loopback lpData to plData with some fixed latency
  val pipe = Module(new LatencyPipe(chiselTypeOf(io.lpData.bits), latency))
  pipe.io.in <> lpDataDecoupled
  pipe.io.out.ready := true.B // immediately fetch the data after <latency> cycles
  io.plData.valid := pipe.io.out.valid
  io.plData.bits := pipe.io.out.bits

  // Tieoffs
  io.plProtocol := Protocol.streaming
  io.plProtocolFlitFormat := FlitFormat.raw
  io.plProtocolValid := true.B // TODO: need to model this signal changing from false to true upon training completion
  io.plInbandPres := false.B // TODO: this depends on the FSM state
  io.plCerror := false.B
  io.plFlitCancel := false.B
  io.plRxActiveReq := false.B // TODO: this signal indicates when the protocol layer can receive bits
  io.plClkReq := true.B // we don't support dynamic clock gating
  io.plRetimerCrd := false.B // this is an optional signal anyways
  io.plStream.protoStack := ProtoStack.stack0
  io.plStream.protoType := ProtoStreamType.Stream
  io.plNfError := false.B // TODO: we want to be able to inject errors from the adapter to protocol layer
  io.plDllp.valid := false.B // we don't use the DLLP interface due to no PCIe support
  io.plDllp.bits := 0.U
  io.plDllpOfc := false.B
  io.plPhyInL1 := false.B // TODO: need to use these if the D2D adapter handles multiple power states
  io.plPhyInL2 := false.B
  io.plConfigCredit := false.B // TODO: need to handle sideband packets
  io.plConfig.valid := false.B
  io.plConfig.bits := 0.U
  io.plStallReq := false.B // TODO: need to model D2D adapter requesting protocol layer to flush flits
  io.plWakeAck := RegNext(io.lpWakeReq)
  io.plLinkWidth := PhyWidth.width64 // TODO: this is a PHY-level parameter
  io.plSpeedMode := SpeedMode.speed4 // TODO: this is a runtime PHY knob
  io.plPhyInRecenter := false.B // TODO: this is part of the FDI state machine
  io.plTrainError := false.B // TODO: need to model fatal errors of the PHY
  io.plError := false.B // TODO: need to model framing errors
  io.plStateStatus := PhyState.active // TODO: this is part of the FDI state machine
}

@annotation.nowarn("cat=deprecation&origin=chisel3.stage.ChiselStage")
object D2DDummyLoopbackMain extends App {
  new ChiselStage()
    .emitVerilog(new D2DDummyLoopback(), args = Array("--target-dir", "vlog"))
}
