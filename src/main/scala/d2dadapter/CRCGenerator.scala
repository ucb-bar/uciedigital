package edu.berkeley.cs.ucie.digital.d2dadapter

import chisel3._
import chisel3.util._

/** Generates the CRC of data using the polynomial x^16 + x^15 + x^2 + 1. The
  * message must be formatted according to the UCIe1.1 spec, and the message
  * will be consumed one byte per clock cycle, starting from the MSB to the LSB.
  * The calculated 2 byte CRC will be available on the crc ReadyValid3IO
  * interface in the raw, un-reversed format as indicated in the UCIe1.1 spec
  * (i.e. CRC[15] is the MSB of the crc interface's data bits).
  * @param width
  *   The message size in bits (must be whole number of bytes). UCIe1.1 spec
  *   will use 1024-bit messages, with shorter messages padded with 0s in MSB
  *   side.
  * @param bytes_per_cycle
  *   The number of bytes of the message to consume per clock cycle. This can be
  *   adjusted depending on processing requirements of consumer.
  * @groupdesc Signals
  *   The input and output controls of the CRCGenerator module
  */

class CRCGenerator(width: Int, bytes_per_cycle: Int) extends Module {
  assert(
    bytes_per_cycle == 1 ||
      bytes_per_cycle == 2 ||
      bytes_per_cycle == 4 ||
      bytes_per_cycle == 8 ||
      bytes_per_cycle == 16 ||
      bytes_per_cycle == 32 ||
      bytes_per_cycle == 64 ||
      bytes_per_cycle == 128,
  )
  assert(width % 8 == 0)
  assert((width / 8) >= bytes_per_cycle)
  assert((width / 8) % bytes_per_cycle == 0)
  val io = IO(new Bundle {

    /** ReadyValidIO interface to allow consumer to transfer message bits to the
      * CRCGenerator. Message bits are latched when message ready and message
      * valid are high for the same clock cycle, and then the CRCGenerator is
      * locked into the calculation loop.
      *
      * The CRCGenerator will not accept new messages during the calculation
      * loop, nor reflect any changes that occur to the message data bits after
      * the message was latched on the ready/valid transfer.
      *
      * The CRCGenerator will only accept a new message after the CRC data has
      * been transfered through the CRC ReadyValid3IO interface.
      * @group Signals
      */
    val message = Flipped(Decoupled(Bits(width.W)))

    /** ReadyValidIO interface to allow CRCGenerator to transfer crc bits to the
      * consumer. CRC will be cleared to 0 and invalid on reset.
      *
      * Once message bits have been transfered, the CRCGenerator will enter the
      * calculation loop, consuming the message data one byte each clock cycle
      * until all bytes have been consumed. The CRC data bits on the interface
      * willl then become valid with the crc valid signal set high.
      *
      * The CRC data will remain valid and the CRCGenerator will remain in an
      * idle state until the consumer initiates a transfer of the CRC data by
      * signaling CRC ready high. After this transfer, the CRC data will become
      * invalid and the CRC Generator will signal message ready to accept a new
      * message.
      * @group Signals
      */
    val crc = Decoupled(Bits(16.W))
  })

  // CRC data
  val crc_calc = RegInit(Bits(16.W), 0.U)
  val crc_poly = RegInit(Bits((width).W), 0x18005.U << width - 17)

  // Output control signals
  val message_ready = RegInit(Bool(), true.B)
  val crc_valid = RegInit(Bool(), false.B)

  // CRC calculating variables
  // Store number of bytes_per_cycle in width bits
  val step = RegInit(0.U(log2Ceil((width / 8) / bytes_per_cycle + 1).W))
  // Latches full message when message_ready and data_val
  val message_bits = RegInit(Bits(width.W), 0.U)

  // Propogate output data register
  io.crc.bits := crc_calc

  // Propogate crc valid and message ready signal
  io.crc.valid := crc_valid
  io.message.ready := message_ready

  // Latch data on message_ready and data_val
  when(io.message.fire) {
    // Reset logic
    crc_calc := 0.U
    crc_valid := false.B

    step := ((width / 8) / bytes_per_cycle).U // Set step to number of cycles needed for word
    message_bits := io.message.bits
    message_ready := false.B
  }

  def nextCRC(message: UInt) = {
    (0 until (bytes_per_cycle * 8)).foldLeft(message) {
      case (message, bit_num) => {
        Mux(message(width - 1) === 1.U, (message ^ crc_poly) << 1, message << 1)
      }
    }
  }

  // Computation not finished when step is not 0
  when(step > 0.U) {
    val update = nextCRC(message_bits)
    message_bits := update
    crc_calc := update(width - 1, width - 16)
    step := step - 1.U
    crc_valid := step === 1.U // If next step will be 0, CRC is now valid
  }

  /* Idle with crc output valid until consumer initiates transfer of CRC with
   * io.crc.ready */
  when(io.crc.fire) {
    crc_valid := false.B
    message_ready := true.B
  }
}
