package edu.berkeley.cs.ucie.digital
package tilelink

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import scala.math._

import protocol._

// TileLink parameters for connecting the TL nodes to chipyard diplomacy

// object TLMsgType extends ChiselEnum {
//     val TLA = Value(0x0.U(3.W))
//     val TLB = Value(0x1.U(3.W))
//     val TLC = Value(0x2.U(3.W))
//     val TLD = Value(0x3.U(3.W))
//     val TLE = Value(0x4.U(3.W))
// }

case class TileLinkParams(
    val address: BigInt,
    val addressRange: BigInt,
    val configAddress: BigInt,
    val inwardQueueDepth: Int,
    val outwardQueueDepth: Int,
    val dataWidth_arg: Int = 256,
) {
  val CONFIG_ADDRESS = configAddress
  val ADDRESS = address
  val ADDR_RANGE = addressRange
  val BEAT_BYTES = dataWidth_arg/8 // 32 // 256 bits/8
  val CONFIG_BEAT_BYTES = 32
  val opcodeWidth = 3
  val paramWidth = 3
  val sourceIDWidth = 8
  val sinkIDWidth = 8
  val addressWidth = 64
  val dataWidth = dataWidth_arg
  val maskWidth = dataWidth/8
  val sizeWidth = log2Ceil(dataWidth/8) // 5 bits if 256
  val deniedWidth = 1
  val corruptWidth = 1
  val reservedH2Width = 5 // width of the reserved bits in header 2
}

/**
  * Class to pack A and D channels into one single bundle
  * Total 383 bits
  */
class TLBundleAUnionD(val tlParams: TileLinkParams) extends Bundle {
  val opcode = Input(UInt(tlParams.opcodeWidth.W))
  val param = Input(UInt(tlParams.paramWidth.W))
  val size = Input(UInt(tlParams.sizeWidth.W))
  val source = Input(UInt(tlParams.sourceIDWidth.W))
  val sink = Input(UInt(tlParams.sinkIDWidth.W))
  val address = Input(UInt(tlParams.addressWidth.W))
  val mask = Input(UInt(tlParams.maskWidth.W))
  val data = Input(UInt(tlParams.dataWidth.W))
  val msgType = Input(UCIProtoMsgTypes())
  // val denied = Input(UInt(tlParams.deniedWidth.W))
  //val corrupt = Input(UInt(tlParams.corruptWidth.W))
}
