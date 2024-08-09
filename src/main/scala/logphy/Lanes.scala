package edu.berkeley.cs.ucie.digital
package logphy

import interfaces._
import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams}
import logphy.Scrambler

object LanesToOne {
  def apply(
      laneData: Vec[UInt],
      numLanes: Int,
      serializerRatio: Int,
  ): Bits = {
    val ratioBytes = serializerRatio / 8
    val rxDataVec = Wire(
      Vec(ratioBytes, Vec(numLanes, UInt(8.W))),
    )
    for (i <- 0 until numLanes) {
      for (j <- 0 until ratioBytes) {
        rxDataVec(j)(i) := laneData(i).asTypeOf(
          VecInit(Seq.fill(ratioBytes)(0.U(8.W))),
        )(j)
      }
    }
    rxDataVec.asUInt
  }
}

object OneToLanes {
  def apply(
      bits: Bits,
      numLanes: Int,
      serializerRatio: Int,
  ): Vec[UInt] = {
    val ratioBytes = serializerRatio / 8
    val result = Wire(Vec(numLanes, UInt(serializerRatio.W)))
    val txDataVec = Wire(Vec(numLanes, Vec(ratioBytes, UInt(8.W))))
    val txDataBytes = Wire(Vec(numLanes * ratioBytes, UInt(8.W)))
    txDataBytes := bits.asTypeOf(txDataBytes)
    for (i <- 0 until numLanes) {
      for (j <- 0 until ratioBytes) {
        txDataVec(i)(j) := txDataBytes(numLanes * j + i)
      }
    }
    for (i <- 0 until numLanes) {
      result(i) := txDataVec(i).asUInt
    }
    result
  }
}

class LaneIO(afeParams: AfeParams) extends Bundle {
  val scramble = Input(Bool())
  val mainbandLaneIO = new MainbandLaneIO(afeParams)
  val mainbandIO = new MainbandIO(afeParams)
}

abstract class LanesModule(afeParams: AfeParams) extends Module {
  val io = IO(new LaneIO(afeParams))

  val rxScrambler =
    Module(
      new UCIeScrambler(afeParams = afeParams, numLanes = afeParams.mbLanes),
    )

  val txScrambler =
    Module(
      new UCIeScrambler(afeParams = afeParams, numLanes = afeParams.mbLanes),
    )

  val rxScramblerInput = Wire(chiselTypeOf(io.mainbandLaneIO.rxData))
  val txScramblerOutput = Wire(chiselTypeOf(io.mainbandLaneIO.txData))

  val scrambledTx = Wire(chiselTypeOf(io.mainbandLaneIO.txData.bits))
  val descrambledRx = Wire(chiselTypeOf(io.mainbandLaneIO.rxData.bits))
  val rxDataInput = Wire(chiselTypeOf(io.mainbandLaneIO.rxData.bits))

  rxScrambler.io.data_in := rxScramblerInput.bits
  rxScrambler.io.valid := rxScramblerInput.fire
  descrambledRx := rxScrambler.io.data_out

  rxDataInput := Mux(
    io.scramble,
    rxScrambler.io.data_out,
    rxScramblerInput.bits,
  )

  io.mainbandIO.rxData.bits := LanesToOne(
    rxDataInput,
    afeParams.mbLanes,
    afeParams.mbSerializerRatio,
  )
  io.mainbandIO.rxData.valid := rxScramblerInput.valid
  rxScramblerInput.ready := true.B

  scrambledTx := txScrambler.io.data_out
  txScrambler.io.data_in := OneToLanes(
    io.mainbandIO.txData.bits,
    afeParams.mbLanes,
    afeParams.mbSerializerRatio,
  )
  txScrambler.io.valid := io.mainbandIO.txData.fire

  txScramblerOutput.valid := io.mainbandIO.txData.valid
  io.mainbandIO.txData.ready := txScramblerOutput.ready
  txScramblerOutput.bits := Mux(
    io.scramble,
    scrambledTx,
    txScrambler.io.data_in,
  )

}

class LanesNoFifo(
    afeParams: AfeParams,
) extends LanesModule(afeParams) {

  assert(
    afeParams.mbSerializerRatio > 8 && afeParams.mbSerializerRatio % 8 == 0,
  )

  rxScramblerInput <> io.mainbandLaneIO.rxData
  io.mainbandLaneIO.txData <> txScramblerOutput

}

class Lanes(
    afeParams: AfeParams,
    queueParams: AsyncQueueParams,
) extends LanesModule(afeParams) {

  val asyncQueueIO = IO(Input(new FifoParams))

  val txMBFifo =
    Module(
      new AsyncQueue(
        Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W)),
        queueParams,
      ),
    )
  val rxMBFifo =
    Module(
      new AsyncQueue(
        Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W)),
        queueParams,
      ),
    )

  rxMBFifo.io.enq <> io.mainbandLaneIO.rxData
  rxMBFifo.io.deq_clock := clock
  rxMBFifo.io.deq_reset := reset
  rxMBFifo.io.enq_clock := asyncQueueIO.clk
  rxMBFifo.io.enq_reset := asyncQueueIO.reset
  txMBFifo.io.deq <> io.mainbandLaneIO.txData
  txMBFifo.io.enq_clock := clock
  txMBFifo.io.enq_reset := reset
  txMBFifo.io.deq_clock := asyncQueueIO.clk
  txMBFifo.io.deq_reset := asyncQueueIO.reset

  assert(
    afeParams.mbSerializerRatio > 8 && afeParams.mbSerializerRatio % 8 == 0,
  )

  rxScramblerInput <> rxMBFifo.io.deq
  txMBFifo.io.enq <> txScramblerOutput

}

class SimLanes(
    afeParams: AfeParams,
    queueParams: AsyncQueueParams,
) extends LanesModule(afeParams) {

  val txMBFifo =
    Module(
      new Queue(
        Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W)),
        queueParams.depth,
      ),
    )
  val rxMBFifo =
    Module(
      new Queue(
        Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W)),
        queueParams.depth,
      ),
    )

  rxMBFifo.io.enq <> io.mainbandLaneIO.rxData
  txMBFifo.io.deq <> io.mainbandLaneIO.txData

  assert(
    afeParams.mbSerializerRatio > 8 && afeParams.mbSerializerRatio % 8 == 0,
  )

  rxScramblerInput <> rxMBFifo.io.deq
  txMBFifo.io.enq <> txScramblerOutput
}
