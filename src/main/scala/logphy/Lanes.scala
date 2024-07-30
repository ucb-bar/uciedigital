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

class Lanes(
    afeParams: AfeParams,
    queueParams: AsyncQueueParams,
) extends Module {

  val io = IO(new Bundle() {
    val scramble = Input(Bool())
    val mainbandLaneIO = new MainbandLaneIO(afeParams)
    val mainbandIO = new MainbandIO(afeParams)
  })

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
  val rxScrambler =
    Module(
      new UCIeScrambler(afeParams = afeParams, numLanes = afeParams.mbLanes),
    )

  val txScrambler =
    Module(
      new UCIeScrambler(afeParams = afeParams, numLanes = afeParams.mbLanes),
    )

  rxMBFifo.io.enq <> io.mainbandLaneIO.rxData
  rxMBFifo.io.deq_clock := clock
  rxMBFifo.io.deq_reset := reset
  rxMBFifo.io.enq_clock := io.mainbandLaneIO.fifoParams.clk
  rxMBFifo.io.enq_reset := io.mainbandLaneIO.fifoParams.reset
  txMBFifo.io.deq <> io.mainbandLaneIO.txData
  txMBFifo.io.enq_clock := clock
  txMBFifo.io.enq_reset := reset
  txMBFifo.io.deq_clock := io.mainbandLaneIO.fifoParams.clk
  txMBFifo.io.deq_reset := io.mainbandLaneIO.fifoParams.reset

  assert(
    afeParams.mbSerializerRatio > 8 && afeParams.mbSerializerRatio % 8 == 0,
  )

  val ratioBytes = afeParams.mbSerializerRatio / 8

  val scrambledTx = Wire(chiselTypeOf(txMBFifo.io.enq.bits))
  val descrambledRx = Wire(chiselTypeOf(rxMBFifo.io.deq.bits))

  val rxDataInput = Wire(chiselTypeOf(rxMBFifo.io.deq.bits))
  rxDataInput := Mux(io.scramble, descrambledRx, rxMBFifo.io.deq.bits)

  /** Data Scrambling / De-scrambling */

  rxScrambler.io.data_in := rxMBFifo.io.deq.bits
  rxScrambler.io.valid := rxMBFifo.io.deq.fire
  descrambledRx := rxScrambler.io.data_out
  txScrambler.io.data_in := OneToLanes(
    io.mainbandIO.txData.bits,
    afeParams.mbLanes,
    afeParams.mbSerializerRatio,
  )
  txScrambler.io.valid := io.mainbandIO.txData.fire
  scrambledTx := txScrambler.io.data_out

  /** Queue data into FIFOs */

  txMBFifo.io.enq.valid := io.mainbandIO.txData.valid
  io.mainbandIO.txData.ready := txMBFifo.io.enq.ready
  txMBFifo.io.enq.bits := Mux(
    io.scramble,
    scrambledTx,
    txScrambler.io.data_in,
  )

  io.mainbandIO.rxData.valid := rxMBFifo.io.deq.valid
  io.mainbandIO.rxData.bits := LanesToOne(
    rxDataInput,
    afeParams.mbLanes,
    afeParams.mbSerializerRatio,
  )
  rxMBFifo.io.deq.ready := true.B
}

class MainbandSimIO(afeParams: AfeParams) extends Bundle {
  val txData = Decoupled(
    Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W)),
  )
  val rxData = Flipped(
    Decoupled(Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W))),
  )
}

class SimLanes(
    afeParams: AfeParams,
    queueParams: AsyncQueueParams,
) extends Module {

  val io = IO(new Bundle() {
    val scramble = Input(Bool())
    val mainbandIo = new MainbandSimIO(afeParams)
    val mainbandLaneIO = new MainbandIO(afeParams)
  })

  val rxScrambler =
    Module(
      new UCIeScrambler(afeParams = afeParams, numLanes = afeParams.mbLanes),
    )

  val txScrambler =
    Module(
      new UCIeScrambler(afeParams = afeParams, numLanes = afeParams.mbLanes),
    )

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

  rxMBFifo.io.enq <> io.mainbandIo.rxData
  txMBFifo.io.deq <> io.mainbandIo.txData

  txMBFifo.io.enq.valid := io.mainbandLaneIO.txData.valid
  io.mainbandLaneIO.rxData.valid := rxMBFifo.io.deq.valid
  assert(
    afeParams.mbSerializerRatio > 8 && afeParams.mbSerializerRatio % 8 == 0,
  )

  val ratioBytes = afeParams.mbSerializerRatio / 8
  val txDataVec = Wire(
    Vec(afeParams.mbLanes, Vec(ratioBytes, UInt(8.W))),
  )
  val rxDataVec = Wire(
    Vec(ratioBytes, Vec(afeParams.mbLanes, UInt(8.W))),
  )
  val txDataBytes = Wire(
    Vec(afeParams.mbLanes * ratioBytes, UInt(8.W)),
  )
  txDataBytes := io.mainbandLaneIO.txData.asTypeOf(txDataBytes)

  for (i <- 0 until afeParams.mbLanes) {
    for (j <- 0 until ratioBytes) {
      txDataVec(i)(j) := txDataBytes(afeParams.mbLanes * j + i)
    }
  }

  val scrambledTx = Wire(chiselTypeOf(txMBFifo.io.enq.bits))
  val descrambledRx = Wire(chiselTypeOf(rxMBFifo.io.deq.bits))

  val rxDataInput = Wire(chiselTypeOf(rxMBFifo.io.deq.bits))
  rxDataInput := Mux(io.scramble, descrambledRx, rxMBFifo.io.deq.bits)

  for (i <- 0 until afeParams.mbLanes) {
    for (j <- 0 until ratioBytes) {
      rxDataVec(j)(i) := rxDataInput(i).asTypeOf(
        VecInit(Seq.fill(ratioBytes)(0.U(8.W))),
      )(j)
    }
  }

  /** Data Scrambling / De-scrambling */

  rxScrambler.io.data_in := rxMBFifo.io.deq.bits
  rxScrambler.io.valid := rxMBFifo.io.deq.fire
  descrambledRx := rxScrambler.io.data_out

  for (i <- 0 until afeParams.mbLanes) {
    txScrambler.io.data_in(i) := txDataVec(i).asUInt
  }
  txScrambler.io.valid := io.mainbandLaneIO.txData.fire
  scrambledTx := txScrambler.io.data_out

  /** Queue data into FIFOs */

  txMBFifo.io.enq.valid := io.mainbandLaneIO.txData.valid
  io.mainbandLaneIO.txData.ready := txMBFifo.io.enq.ready
  txMBFifo.io.enq.bits := Mux(
    io.scramble,
    scrambledTx,
    txScrambler.io.data_in,
  )

  io.mainbandLaneIO.rxData.valid := rxMBFifo.io.deq.valid
  io.mainbandLaneIO.rxData.bits := rxDataVec.asUInt
  rxMBFifo.io.deq.ready := true.B
}
