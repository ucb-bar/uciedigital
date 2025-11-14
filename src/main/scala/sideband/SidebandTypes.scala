package edu.berkeley.cs.ucie.digital
package sideband

import chisel3._
import chisel3.util._

object SBRxTxMode extends ChiselEnum {
  val RAW, PACKET = Value
}