package edu.berkeley.cs.ucie.digital

import chisel3._
import chisel3.util._

import interfaces._

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

/**
  * This class creates a loopback for testing the Protocol layer
  * It consists of the FDI interface and a latency pipe to emulate
  * the latency imparted by the D2D adapter.
  */
class ProtoFDILoopback(val fdiParams: FdiParams, val latency: Int = 8) extends Module {
    val io = IO(Flipped(new Fdi(fdiParams)))

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

    // Signals from Protocol layer to D2D adapter
    /*
    io.lpRetimerCrd
    io.lpCorruptCrc
    io.lpDlio.lp
    io.lpDlio.lpOfc
    io.lpStream
    io.lpStateReq
    io.lpLinkError
    io.lpRxActiveStatus
    io.lpStallAck
    io.lpClkAck
    io.lpWakeReq
    io.lpConfig
    io.lpConfigCredit
    */

    // Signals from D2D adapter to Protocol layer
    io.plRetimerCrd
    io.plDllp
    io.plDllpOfc
    io.plStream
    io.plFlitCancel
    io.plStateStatus
    io.plInbandPres
    io.plError
    io.plCerror
    io.plNfError
    io.plTrainError
    io.plRxActiveReq
    io.plProtocol
    io.plProtocolFlitFormat
    io.plProtocolValid
    io.plStallReq
    io.plPhyInRecenter
    io.plPhyInL1
    io.plPhyInL2
    io.plSpeedMode
    io.plLinkWidth
    io.plClkReq
    io.plWakeAck
    io.plConfig
    io.plConfigCredit
}