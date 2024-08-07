package edu.berkeley.cs.ucie.digital
package afe

import chisel3._
import chisel3.util._



class AsyncFifoCustom(depth: Int, width: Int) extends Module {
    val io = IO(new Bundle {
        val deq_clock = Input(Clock())
        val deq = Flipped(Flipped(Decoupled(UInt(width.W))))
        val deq_reset = Input(Bool())
        val enq_reset = Input(Bool())
        val enq_clock = Input(Clock())
        val enq = Flipped(Decoupled(UInt(width.W)))
    })
    val fifo_inst = Module (new AsyncFifoCustomCore(depth, width))
    
    fifo_inst.io.rst := ~io.enq_reset
    fifo_inst.io.clk_w := io.enq_clock
    fifo_inst.io.valid_w := io.enq.valid 
    io.enq.ready := fifo_inst.io.ready_w
    fifo_inst.io.data_w := io.enq.bits
    
    // fifo_inst.io.clk_r := io.deq_clock
    // fifo_inst.io.ready_r := io.deq.ready 
    // io.deq.bits := fifo_inst.io.data_r
    // io.deq.valid := fifo_inst.io.valid_r 
    io.deq.valid := fifo_inst.io.valid_r 
    fifo_inst.io.clk_r := io.deq_clock 
    fifo_inst.io.ready_r := io.deq.ready 
    io.deq.bits := fifo_inst.io.data_r
}

class AsyncFifoCustomCore (depth: Int, width: Int) extends BlackBox(
    Map("DEPTH" -> depth, "WIDTH" -> width)
) with HasBlackBoxResource {
    val io = IO(new Bundle {
        val rst = Input(Bool())
        val clk_w = Input(Clock())
        val valid_w = Input(Bool())
        val ready_w = Output(Bool())
        val data_w = Input(Bits(width.W))
        val clk_r = Input(Clock())
        val valid_r = Output(Bool())
        val ready_r = Input(Bool())
        val data_r = Output(Bits(width.W))
    })
    addResource("/vsrc/AsyncFifoCustomCore.sv")
}