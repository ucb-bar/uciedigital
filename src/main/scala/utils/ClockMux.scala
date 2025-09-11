package edu.berkeley.cs.ucie.digital
package utils

import chisel3._
import chisel3.util._

class ClockMux2 extends BlackBox with HasBlackBoxResource {

  val io = IO(new Bundle {
    val clocksIn = Input(Vec(2, Clock()))
    val sel = Input(Bool())
    val clockOut = Output(Clock())
  })

  addResource("/vsrc/ClockSelector.v")
}
