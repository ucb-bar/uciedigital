package edu.berkeley.cs.ucie.digital
package afe

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import interfaces._
import chisel3.experimental.DataMirror
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams}

// This module receives data from logphy and sends to analog
class TxMainbandSerializer(afeParams: AfeParams) extends Module {
  val io = IO(new Bundle {
    val txInData = Flipped(
      Decoupled(Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W))),
    )
    val txMbIo = Output(new MainbandIo())
  })

  val lanes = afeParams.mbLanes
  val width = afeParams.mbSerializerRatio
  val hasData = Wire(Bool())
  hasData := io.txInData.valid

  // receive data

  val txMbShiftRegs = Seq.fill(lanes)(RegInit(0.U(width.W)))
  val sending = RegInit(false.B)
  val (txMbUICounter, uiCountDone) = Counter(sending, width)

  io.txInData.ready := !sending
  when(io.txInData.fire) {
    sending := true.B
  }.elsewhen(uiCountDone) {
    sending := false.B
  }

  val shift = RegInit(false.B)

  io.txMbIo.clkn := (!clock.asBool).asClock
  io.txMbIo.clkp := clock

  // Assign each async fifo individually
  io.txInData.bits.zipWithIndex.foreach { case (laneData, i) =>
    // Valid framing, up for first 4 ui, down for last 4 ui
    when(io.txInData.fire) {
      txMbShiftRegs(i) := laneData
    }.otherwise {
      txMbShiftRegs(i) := txMbShiftRegs(i) >> 1.U
    }
  }

  io.txMbIo.data := VecInit(txMbShiftRegs.map(_(0))).asUInt
  io.txMbIo.valid := sending
  io.txMbIo.track := false.B
}

// This module accepts data from analog and send to adapter
class RxMainbandDeserializer(
    afeParams: AfeParams,
) extends Module {
  val io = IO(new Bundle {
    val rxMbIo = Input(new MainbandIo())
    val rxOutData =
      Decoupled(Vec(afeParams.mbLanes, Bits(afeParams.mbSerializerRatio.W)))
  })

  private val width = afeParams.mbSerializerRatio
  private val lanes = afeParams.mbLanes

  val rxMbShiftRegs = Seq.fill(lanes)(RegInit(0.U(width.W)))
  val (_, done_sending) = Counter(io.rxMbIo.valid, width)
  val out_valid = RegNext(done_sending)

  rxMbShiftRegs.zipWithIndex.foreach { case (rxMbShiftReg, i) =>
    when(io.rxMbIo.valid) {
      rxMbShiftReg := (rxMbShiftReg << 1.U) | io.rxMbIo.data(i)
    }
    io.rxOutData.bits(i) := Reverse(rxMbShiftReg)
  }
  io.rxOutData.valid := out_valid
}

class MbAfe(afeParams: AfeParams) extends Module {
  val io = IO(new Bundle {
    val mbAfeIo = Flipped(new MainbandAfeIo(AfeParams()))
    val mbTxData = Output(new MainbandIo(afeParams.mbLanes))
    val mbRxData = Input(new MainbandIo(afeParams.mbLanes))
  })

  val txMainband = Module(new TxMainbandSerializer(afeParams))
  val rxMainband =
    withClockAndReset(io.mbRxData.clkp, reset.asAsyncReset)(
      Module(new RxMainbandDeserializer(afeParams)),
    )

  /** Connect to LogPhy AFE IO */
  io.mbAfeIo.fifoParams.clk := io.mbRxData.clkp
  io.mbAfeIo.fifoParams.reset := reset.asBool
  io.mbAfeIo.txData <> txMainband.io.txInData
  io.mbAfeIo.rxData <> rxMainband.io.rxOutData
  io.mbAfeIo.pllLock := true.B

  /** Connect to Mainband IO */
  io.mbTxData <> txMainband.io.txMbIo
  io.mbRxData <> rxMainband.io.rxMbIo
}
