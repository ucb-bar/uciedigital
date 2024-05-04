package edu.berkeley.cs.ucie.digital
package sideband

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncQueue, AsyncResetReg}
import interfaces._
import utils.ClockMux2

//TODO: 1) L317-318 needs to be revisited
//      2) SidebandLinkDeserializer needs to have CDC crossings

case class SidebandParams(
    // val NC_width: Int = 32, // This is merged into the FDI Params
    val sbNodeMsgWidth: Int =
      128, // Internal SB msg widths in individual layers
    val maxCrd: Int = 32,
)

// SidebandLinkNode connects the SB messaging to the AFE, so it does not carry credit
class SidebandLinkNode(val sbParams: SidebandParams, val fdiParams: FdiParams)
    extends Module {
  val io = IO(new SidebandLinkIO(sbParams, fdiParams))

  val tx_ser = Module(new SidebandLinkSerializer(sbParams, fdiParams))
  val rx_des = Module(new SidebandLinkDeserializer(sbParams, fdiParams))
  val rx_queue = Module(new SidebandPriorityQueue(sbParams))

  // Connect outer signals
  io.outer.tx <> tx_ser.io.out
  io.outer.rx.clock <> rx_des.io.in.remote_clock
  io.outer.rx.bits <> rx_des.io.in.bits

  // Connect rx queue and deserializer
  rx_queue.io.enq <> rx_des.io.out

  // Connect inner signals
  io.inner.layer_to_node.ready <> tx_ser.io.in.ready

  tx_ser.io.in.bits <> Cat(
    io.inner.layer_to_node.bits(127, 59),
    0.U(1.W),
    io.inner.layer_to_node.bits(57, 0),
  )
  tx_ser.io.in.valid <> io.inner.layer_to_node.valid

  when(io.rxMode === RXTXMode.PACKET) {
    io.inner.node_to_layer <> rx_queue.io.deq
  }.otherwise {
    rx_queue.io.enq.noenq()
    rx_queue.io.deq.nodeq()
    io.inner.node_to_layer <> rx_des.io.out
  }
}

// SidebandNode is the inter/intra layer SB messaging and uses credit flow
class SidebandNode(val sbParams: SidebandParams, val fdiParams: FdiParams)
    extends Module {
  val io = IO(new SidebandNodeIO(sbParams, fdiParams))

  val tx_ser = Module(new SidebandSerializer(sbParams, fdiParams))
  val rx_queue = Module(new SidebandPriorityQueue(sbParams))
  val rx_des = Module(new SidebandDeserializer(sbParams, fdiParams))

  // Connect outer signals
  io.outer.tx <> tx_ser.io.out
  io.outer.rx.valid <> rx_des.io.in.valid
  io.outer.rx.bits <> rx_des.io.in.bits
  io.outer.rx.credit := rx_queue.io.deq.fire && (!SBM.isComplete(
    rx_queue.io.deq.bits.asUInt,
  ))

  // Connect rx queue and deserializer
  rx_queue.io.enq <> rx_des.io.out

  // Connect inner signals
  io.inner.layer_to_node.ready <> tx_ser.io.in.ready

  tx_ser.io.in.bits <> io.inner.layer_to_node.bits
  tx_ser.io.in.valid <> io.inner.layer_to_node.valid

  io.inner.node_to_layer <> rx_queue.io.deq
}

class SidebandPriorityQueue(val sbParams: SidebandParams) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(UInt(sbParams.sbNodeMsgWidth.W)))
    val deq = Decoupled(UInt(sbParams.sbNodeMsgWidth.W))
  })

  val p0_queue = Module(
    new Queue(UInt(sbParams.sbNodeMsgWidth.W), 4),
  ) // highest priority queue, for access completion packets
  val p1_queue = Module(
    new Queue(UInt(sbParams.sbNodeMsgWidth.W), sbParams.maxCrd),
  ) // second highest priority queue, for message packets
  val p2_queue = Module(
    new Queue(UInt(sbParams.sbNodeMsgWidth.W), sbParams.maxCrd),
  ) // lowest priority queue, for access request packets

  val enq_arb = Module(new SidebandEnqArbiter(sbParams))
  val deq_arb = Module(new SidebandDeqArbiter(sbParams))

  enq_arb.io.in <> io.enq
  enq_arb.io.out(0) <> p0_queue.io.enq
  enq_arb.io.out(1) <> p1_queue.io.enq
  enq_arb.io.out(2) <> p2_queue.io.enq

  deq_arb.io.in(0) <> p0_queue.io.deq
  deq_arb.io.in(1) <> p1_queue.io.deq
  deq_arb.io.in(2) <> p2_queue.io.deq
  deq_arb.io.out <> io.deq
}

class SidebandEnqArbiter(val sbParams: SidebandParams) extends Module {
  val io = IO(new Bundle {
    val out = Vec(3, Decoupled(UInt(sbParams.sbNodeMsgWidth.W)))
    val in = Flipped(Decoupled(UInt(sbParams.sbNodeMsgWidth.W)))
  })
  val lIndex: Array[Int] = Array(0, 1, 2)
  lIndex.foreach(i => io.out(i).bits := io.in.bits)
  // fake decoupled interface, out(*).ready is always true
  // lIndex.foreach(i => io.out(i).ready := true.B)
  // fake decoupled interface, in.ready is always true
  io.in.ready := true.B
  io.out(0).valid := io.in.valid && SBM.isComplete(io.in.bits)
  io.out(1).valid := io.in.valid && SBM.isMessage(io.in.bits)
  io.out(2).valid := io.in.valid && SBM.isRequest(io.in.bits)
}

class SidebandDeqArbiter(val sbParams: SidebandParams) extends Module {
  val io = IO(new Bundle {
    val out = Decoupled(UInt(sbParams.sbNodeMsgWidth.W))
    val in = Vec(3, Flipped(Decoupled(UInt(sbParams.sbNodeMsgWidth.W))))
  })
  val lIndex: Array[Int] = Array(0, 1, 2)
  io.out.valid := io.in(0).valid || io.in(1).valid || io.in(2).valid
  // traffic arbitration
  when(io.in(0).valid) {
    lIndex.foreach(i =>
      io.in(i).ready := (if (i == 0) io.out.ready else false.B),
    )
    io.out.bits := io.in(0).bits
  }.elsewhen(io.in(1).valid) {
    lIndex.foreach(i =>
      io.in(i).ready := (if (i == 1) io.out.ready else false.B),
    )
    io.out.bits := io.in(1).bits
  }.elsewhen(io.in(2).valid) {
    lIndex.foreach(i =>
      io.in(i).ready := (if (i == 2) io.out.ready else false.B),
    )
    io.out.bits := io.in(2).bits
  }.otherwise {
    lIndex.foreach(i => io.in(i).ready := (false.B))
    io.out.bits := io.in(0).bits
  }
}

class SidebandSerializer(val sbParams: SidebandParams, val fdiParams: FdiParams)
    extends Module {
  val sb_w = fdiParams.sbWidth
  val msg_w = sbParams.sbNodeMsgWidth
  val cdt_max = sbParams.maxCrd
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(msg_w.W)))
    val out = new Bundle {
      val bits = Output(UInt(sb_w.W))
      val valid = Output(Bool())
      val credit = Input(Bool())
    }
  })

  val dataBits = msg_w
  val dataBeats = (dataBits - 1) / sb_w + 1
  val data = Reg(UInt(dataBits.W))

  val sending = RegInit(false.B)
  val (sendCount, sendDone) = Counter(io.out.valid, dataBeats)

  val current_credit = RegInit(cdt_max.U((log2Ceil(cdt_max) + 1).W))
  val isComplete = RegInit(false.B)

  io.in.ready := (!sending && (SBM.isComplete(
    io.in.bits.asUInt,
  ) || (current_credit > 0.U))) // set ready if credit>0 or completion packet
  io.out.valid := (((current_credit > 0.U) || isComplete) && sending) // Don't need to check credit for access completion packets
  io.out.bits := data(sb_w - 1, 0)

  when(io.in.fire) {
    data := io.in.bits.asUInt
    sending := true.B
    isComplete := SBM.isComplete(io.in.bits.asUInt)
  }

  when(io.out.valid) { data := data >> sb_w.U }

  when(sendDone) {
    sending := false.B
    when(isComplete === false.B) { current_credit := current_credit - 1.U }
  }

  when(io.out.credit) { current_credit := current_credit + 1.U }
}

class SidebandDeserializer(
    val sbParams: SidebandParams,
    val fdiParams: FdiParams,
) extends Module {
  val sb_w = fdiParams.sbWidth
  val msg_w = sbParams.sbNodeMsgWidth
  val io = IO(new Bundle {
    val in = new Bundle {
      val bits = Input(UInt(sb_w.W))
      val valid = Input(Bool())
    }
    val out = Decoupled(UInt(msg_w.W))
  })

  val dataBits = msg_w
  val dataBeats = (dataBits - 1) / sb_w + 1
  val data = Reg(Vec(dataBeats, UInt(sb_w.W)))

  val receiving = RegInit(true.B)
  val (recvCount, recvDone) = Counter(io.in.valid, dataBeats)

  io.out.valid := !receiving
  io.out.bits := data.asUInt

  when(io.in.valid) {
    data(recvCount) := io.in.bits
  }

  when(recvDone) { receiving := false.B }

  when(io.out.fire) { receiving := true.B }
}

class SidebandLinkSerializer(
    val sbParams: SidebandParams,
    val fdiParams: FdiParams,
) extends Module {
  val sb_w = 1
  val msg_w = sbParams.sbNodeMsgWidth
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(msg_w.W)))
    val out = new Bundle {
      val bits = Output(UInt(sb_w.W))
      val clock = Output(Bool())
    }
    val counter = Output(UInt(log2Ceil(32).W))
  })

  val dataBits = msg_w
  val dataBeats = (dataBits - 1) / sb_w + 1
  val data = Reg(UInt(dataBits.W))

  val counter_en = Wire(Bool())
  val counter_next = Wire(UInt(log2Ceil(32).W))
  val counter = RegEnable(counter_next, 0.U(log2Ceil(32).W), counter_en)
  counter_next := Mux(counter === 31.U, 31.U, counter + 1.U)

  val sending = RegInit(false.B)
  val done = RegInit(false.B)
  val waited = RegInit(true.B)
  val (sendCount, sendDone) = Counter(sending, dataBeats)

  val isComplete = RegInit(false.B)

  io.in.ready := (waited) // wait for 32 cycles between SB messages
  io.out.clock := Mux(sending, clock.asUInt, false.B)
  io.out.bits := data(sb_w - 1, 0)

  counter_en := false.B

  io.counter := counter

  when(io.in.fire) {
    data := io.in.bits.asUInt
    sending := true.B
    waited := false.B
    counter_next := 0.U
  }

  when(sending) { data := data >> sb_w.U }

  when(sendDone) {
    sending := false.B
    done := true.B
  }

  when(done) {
    counter_en := true.B
    waited := counter === 31.U
  }
}

class SidebandLinkDeserializer(
    val sbParams: SidebandParams,
    val fdiParams: FdiParams,
) extends Module {
  val sb_w = 1
  val msg_w = sbParams.sbNodeMsgWidth
  val io = IO(new Bundle {
    val in = new Bundle {
      val bits = Input(UInt(sb_w.W))
      val remote_clock = Input(Bool())
    }
    val out = Decoupled(UInt(msg_w.W))
  })

  val remote_clock = io.in.remote_clock.asClock

  val dataBits = msg_w
  val dataBeats = (dataBits - 1) / sb_w + 1

  val asyncFifo = Module(
    new AsyncQueue(
      UInt(msg_w.W),
    ),
  )

  asyncFifo.io.deq <> io.out
  asyncFifo.io.deq_clock := clock
  asyncFifo.io.deq_reset := reset
  asyncFifo.io.enq_clock := remote_clock
  asyncFifo.io.enq_reset := reset

  // val clockMux2 = Module(new ClockMux2)
  // clockMux2.io.clocksIn(0) := remote_clock
  // clockMux2.io.clocksIn(1) := clock
  // clockMux2.io.sel := reset.asBool

  withClockAndReset(remote_clock, reset.asAsyncReset) {
    val data = Reg(Vec(dataBeats, UInt(sb_w.W)))

    // val (recvCount, recvDone) = Counter(true.B, dataBeats)
    val recvCount = RegInit(0.U(log2Ceil(dataBeats + 1).W))
    val recvDone = WireInit(recvCount === (dataBeats - 1).U)
    val receiving = RegInit(true.B)

    when(recvCount === (dataBeats - 1).U) {
      recvCount := 0.U
    }.elsewhen(receiving) {
      recvCount := recvCount + 1.U
    }

    // val recvCount_delay = RegInit(0.U(log2Ceil(dataBeats).W))
    // recvCount_delay := recvCount

    data(recvCount) := io.in.bits
    when(recvDone) { receiving := false.B }
    when(asyncFifo.io.enq.fire) { receiving := true.B }
    asyncFifo.io.enq.valid := !receiving

    asyncFifo.io.enq.bits := data.asUInt
  }
}
