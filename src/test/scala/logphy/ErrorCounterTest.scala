package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import interfaces.AfeParams

class ErrorCounterTest extends AnyFlatSpec with ChiselScalatestTester {
  val afeParams = AfeParams()
  behavior of "error counter"
  it should "correctly count errors for lfsr pattern" in {
    test(new ErrorCounter(afeParams)) { c =>
      c.io.req.valid.poke(true.B)
      c.io.req.bits.pattern.poke(TransmitPattern.LFSR)

      /** No errors */
      c.io.req.bits.input.poke(
        "hb877_cf0f_c0c7_07bf_b877_cf0f_c0c7_07bf_07ce_c912_db60_bbbc_07ce_c912_db60_bbbc".U,
      )
      c.io.errorCount.expect(0.U)
      c.clock.step()

      /** 4 errors */
      c.io.req.bits.input.poke(
        "h85d6_5241_12be_ad28_85d7_5241_13be_ad28_9b02_9901_9816_861d_9b02_9900_981e_861d".U,
      )
      c.io.errorCount.expect(4.U)
      c.clock.step()

      /** 7 errors */
      c.io.req.bits.input.poke(
        "hac7c_d3b4_60e4_8428_ac7c_d0b0_66e4_8428_6bad_c683_4596_d3b8_6bfd_c683_4596_d3b8".U,
      )
      c.io.errorCount.expect(7.U)
      c.clock.step()

      c.io.req.valid.poke(false.B)
      for (_ <- 0 until 10) {
        c.clock.step()
      }

      /** 0 errors */
      c.io.req.bits.input.poke(
        "hb317_a4b0_142b_3f8c_b317_a4b0_142b_3f8c_0a16_1c83_9fc1_5e54_0a16_1c83_9fc1_5e54".U,
      )
      c.io.req.valid.poke(true.B)
      c.io.errorCount.expect(0.U)
    }
  }
  it should "correctly count errors for valtrain pattern" in {
    test(new ErrorCounter(afeParams)) { c =>
      val width = afeParams.mbSerializerRatio * afeParams.mbLanes
      c.io.req.valid.poke(true.B)
      c.io.req.bits.pattern.poke(TransmitPattern.VALTRAIN)

      /** No errors */
      c.io.req.bits.input.poke(
        ("b" + "1111_0000" * (width / 8)).U,
      )
      c.io.errorCount.expect(0.U)
      c.clock.step()

      /** 4 errors */
      c.io.req.bits.input.poke(
        ("b" + "1011_0100" + "1111_0000" * (width / 8 - 2) + "0111_0001").U,
      )
      c.io.errorCount.expect(4.U)
      c.clock.step()

    }
  }
  it should "correctly count errors for per-lane ID pattern" in {
    test(new ErrorCounter(afeParams)) { c =>
      c.io.req.valid.poke(true.B)
      c.io.req.bits.pattern.poke(TransmitPattern.PER_LANE_ID)

      /** No errors */
      c.io.req.bits.input.poke(
        ("b" +
          "1010_0000_1111_1010" +
          "1010_0000_1110_1010" +
          "1010_0000_1101_1010" +
          "1010_0000_1100_1010" +
          "1010_0000_1011_1010" +
          "1010_0000_1010_1010" +
          "1010_0000_1001_1010" +
          "1010_0000_1000_1010" +
          "1010_0000_0111_1010" +
          "1010_0000_0110_1010" +
          "1010_0000_0101_1010" +
          "1010_0000_0100_1010" +
          "1010_0000_0011_1010" +
          "1010_0000_0010_1010" +
          "1010_0000_0001_1010" +
          "1010_0000_0000_1010").U,
      )
      c.io.errorCount.expect(0.U)
      c.clock.step()

      c.io.req.bits.input.poke(
        ("b" +
          "1010_0000_1111_1010" +
          "1010_0000_1110_1010" +
          "1010_0010_1101_1010" +
          "1010_0000_1100_1010" +
          "1010_0000_0011_1010" +
          "1010_0000_1010_1010" +
          "1010_0000_1001_1010" +
          "1010_0000_1000_1010" +
          "1010_0000_0111_1010" +
          "1010_0000_0110_1010" +
          "1010_0000_0100_1010" +
          "1010_0000_0100_1010" +
          "1010_0000_0011_1010" +
          "1010_0000_0010_1010" +
          "1010_0000_0001_1010" +
          "0010_0000_0000_1010").U,
      )
      c.io.errorCount.expect(4.U)
      c.clock.step()
    }
  }
}
