package edu.berkeley.cs.ucie.digital
package e2e

import chisel3._
import chisel3.util._
import interfaces._
import org.chipsalliance.cde.config.{Field, Parameters, Config}
import chisel3.experimental.hierarchy.{
  Definition,
  Instance,
  instantiable,
  public,
}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink.{TLTestRAM}
import protocol._

// @instantiable
class AfeLoopback(val afeParams: AfeParams) extends Module {
  val io = IO(new Bundle {
    // val finished = Output(Bool())
    // val mbAfe = Flipped(new MainbandAfeIo(afeParams))
    val mbAfe_tx = Input(new MainbandIo(afeParams.mbLanes))
    val mbAfe_rx = Output(new MainbandIo(afeParams.mbLanes))
    val sbAfe_tx = Input(new SidebandIo)
    val sbAfe_rx = Output(new SidebandIo)
  })

  val latency = 100
  /* val delayerMb = Module(new Pipe(chiselTypeOf(io.mbAfe.txData.bits),
   * latency)) */
  val delayerMbTx = Module(new Pipe(chiselTypeOf(io.mbAfe_tx.data), latency))
  val delayerSb = Module(new Pipe(chiselTypeOf(io.sbAfe_tx.data), latency))
  val delayerSb_clock = Module(
    new Pipe(chiselTypeOf(io.sbAfe_tx.clk), latency),
  )
  val delayerMbTx_clockn = Module(
    new Pipe(chiselTypeOf(io.mbAfe_tx.clkn.asBool), latency),
  )
  val delayerMbTx_clockp = Module(
    new Pipe(chiselTypeOf(io.mbAfe_tx.clkp.asBool), latency),
  )

  /** TODO: ansa fix delayed signals -- not delayed */
  delayerMbTx.io.enq.valid := io.mbAfe_tx.valid
  delayerMbTx.io.enq.bits := io.mbAfe_tx.data
  io.mbAfe_rx.data := delayerMbTx.io.deq.bits
  io.mbAfe_rx.valid := delayerMbTx.io.deq.valid
  delayerMbTx_clockn.io.enq.valid := true.B
  delayerMbTx_clockn.io.enq.bits := io.mbAfe_tx.clkn.asBool
  delayerMbTx_clockp.io.enq.valid := true.B
  delayerMbTx_clockp.io.enq.bits := io.mbAfe_tx.clkp.asBool

  io.mbAfe_rx.clkn := io.mbAfe_tx.clkn
  io.mbAfe_rx.clkp := io.mbAfe_tx.clkp
  io.mbAfe_rx.track := false.B

  delayerSb.io.enq.valid := true.B // io.sbAfe.txData.valid
  delayerSb.io.enq.bits := io.sbAfe_tx.data
  delayerSb_clock.io.enq.valid := true.B
  delayerSb_clock.io.enq.bits := io.sbAfe_tx.clk

  io.sbAfe_rx.data := delayerSb.io.deq.bits
  val delayNegEdge = withClock((!clock.asBool).asClock)(RegInit(false.B))
  delayNegEdge := delayerSb_clock.io.deq.bits.asBool && delayerSb_clock.io.deq.valid
  io.sbAfe_rx.clk := Mux(
    delayNegEdge,
    clock.asBool,
    false.B,
  )
}
