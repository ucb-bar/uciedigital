package edu.berkeley.cs.ucie.digital
package logphy

import chisel3.util._
import chisel3._
import chisel3.util.random._
import edu.berkeley.cs.ucie.digital.interfaces.AfeParams

class Scrambler(
    afeParams: AfeParams,
    seed: BigInt,
) extends Module {
  val io = IO(new Bundle {
    val data_in = Input(UInt(afeParams.mbSerializerRatio.W))
    val valid = Input(Bool())
    val seed = Input(UInt(23.W))
    val data_out = Output(UInt(afeParams.mbSerializerRatio.W))
  })
  val LFSR = Module(
    new ConsistentFibonacciLFSR(
      23,
      Seq(23, 21, 16, 8, 5, 2),
      Some(seed),
      XOR,
      afeParams.mbSerializerRatio,
      false,
    ),
  )
  LFSR.io.increment := io.valid
  LFSR.io.seed.bits := VecInit(io.seed.asBools)
  LFSR.io.seed.valid := (reset.asBool)
  val LFSR_flipped = Wire(UInt(16.W))
  LFSR_flipped := Reverse(LFSR.io.out.asUInt(22, 23 - 16))

  io.data_out := LFSR_flipped ^ io.data_in
}

class UCIeScrambler(
    afeParams: AfeParams,
    numLanes: Int,
) extends Module {
  val io = IO(new Bundle {
    val data_in = Input(Vec(numLanes, UInt(afeParams.mbSerializerRatio.W)))
    val valid = Input(Bool())
    val data_out = Output(Vec(numLanes, UInt(afeParams.mbSerializerRatio.W)))
  })
  val UCIe_seeds = List(
    /** seeds have to be reversed so that LSB ends up in rightmost position */
    "00111101111111011011100", // "1dbfbc",
    "11011101111000000110000", // "0607bb",
    "00000110111000110111100", // "1ec760",
    "11011011000000110001100", // "18c0db",
    "01001000111100001000000", // "010f12",
    "10010011111100111001100", // "19cfc9",
    "01110011111011100100000", // "0277ce",
    "11100000000111011101100", // "1bb807",
  )
  val seeds = (for (i <- 0 until numLanes) yield UCIe_seeds(i % 8)).toList
  val scramblers =
    seeds.map(seed => Module(new Scrambler(afeParams, BigInt(seed, 2))))
  for (i <- 0 until scramblers.length) {
    scramblers(i).io.data_in := io.data_in(i)
    scramblers(i).io.valid := io.valid
    scramblers(i).reset := reset
    scramblers(i).clock := clock
    scramblers(i).io.seed := ("b" + seeds(i)).U(23.W)
    io.data_out(i) := scramblers(i).io.data_out
  }
}
