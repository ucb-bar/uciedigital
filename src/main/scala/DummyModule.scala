package edu.berkeley.cs.ucie.digital

import chisel3._

class DummyModule extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Output(Bool())
  })


  io.b := ~io.a
}
