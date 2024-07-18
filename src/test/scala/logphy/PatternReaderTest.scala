package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import sideband.SidebandParams
import interfaces._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
class PatternReaderTest extends AnyFlatSpec with ChiselScalatestTester {
  val afeParams = AfeParams()
  val sbParams = SidebandParams()
  behavior of "pattern reader"
  it should "detect SB clock pattern" in {
    test(new PatternReader(sbParams, afeParams, 1024)) { c =>
      initPorts(c)
      createRequest(c, TransmitPattern.CLOCK, sbParams.sbNodeMsgWidth * 4, true)
      c.io.mbRxData.ready.expect(false.B)

      val testVector =
        Seq.fill(4)("haaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa".U)

      testVector.foreach(vec => {
        c.io.resp.complete.expect(false.B)
        c.io.resp.inProgress.expect(true.B)
        c.io.mbRxData.ready.expect(false.B)
        c.io.sbRxData.enqueueNow(vec)
      })

      c.io.mbRxData.ready.expect(false.B)
      c.io.resp.complete.expect(true.B)
    }
  }
  it should "detect MB LFSR pattern no errors" in {
    test(new PatternReader(sbParams, afeParams, maxPatternCount = 2048)) { c =>
      val width = afeParams.mbLanes * afeParams.mbSerializerRatio
      val testVector =
        Seq(
          "hb877_cf0f_c0c7_07bf_b877_cf0f_c0c7_07bf_07ce_c912_db60_bbbc_07ce_c912_db60_bbbc"
            .U(width.W),
          "h85d7_5241_13be_ad28_85d7_5241_13be_ad28_9b02_9901_981e_861d_9b02_9901_981e_861d"
            .U(width.W),
          "hac7c_d0b0_60e4_8428_ac7c_d0b0_60e4_8428_6bad_c683_4596_d3b8_6bad_c683_4596_d3b8"
            .U(width.W),
          "hb317_a4b0_142b_3f8c_b317_a4b0_142b_3f8c_0a16_1c83_9fc1_5e54_0a16_1c83_9fc1_5e54"
            .U(width.W),
        )
      makeMBTest(c, TransmitPattern.LFSR, testVector, 0)
    }
  }
  it should "detect MB LFSR pattern error count" in {
    test(new PatternReader(sbParams, afeParams, maxPatternCount = 2048)) { c =>
      val width = afeParams.mbLanes * afeParams.mbSerializerRatio
      var testVector = Seq(
        "hb877_cf0f_c0c7_07bf_b877_cf0f_c0c7_07bf_07ce_c912_df60_bbbc_07ce_c912_db60_bbbc"
          .U(width.W),
        "h85d7_5241_13be_ad28_85d7_5241_13be_ad28_9b02_9901_981e_861d_9b12_9911_981e_861d"
          .U(width.W),
        "hac7c_d0b0_60e4_8428_ac7c_d3b0_60e4_f428_6bad_c683_4596_d3f8_6bad_c683_4596_d3b8"
          .U(width.W),
        "hb317_a7b1_142b_3f8c_f317_a4b0_142b_3f8c_fa16_1c83_9fc1_5e54_0a16_1c83_9fc1_5e54"
          .U(width.W),
      )
      makeMBTest(c, TransmitPattern.LFSR, testVector, 17)

      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)

      testVector = Seq(
        "hb877_cf0f_c0c7_07bf_b877_cf00_c0c7_07bf_07ce_c912_db60_bbbc_07ce_c912_db60_bbbc"
          .U(width.W),
        "h85d7_5241_13be_ad28_85d7_5241_13be_ad28_9b02_0901_981e_861d_9b02_9901_981e_861d"
          .U(width.W),
        "hac7c_d0b0_60e4_8428_ac7c_d0b0_60e4_8428_6bad_c682_4596_d3b8_6bad_c683_4596_d3b8"
          .U(width.W),
        "hb317_a4b0_142b_3f8c_b317_a4b0_142b_3f8c_0a16_1c83_9fc1_5e54_0a16_1c83_9fc1_5e54"
          .U(width.W),
      )
      makeMBTest(c, TransmitPattern.LFSR, testVector, 7)

      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)

      testVector = Seq(
        "hb877_cf0f_c0c7_07bf_b877_cf0f_c0c7_07bf_07ce_c912_db60_bbbc_00ce_c912_db60_bbbc"
          .U(width.W),
        "h85d7_5241_13be_ad28_85d7_5241_13be_ad28_9b32_9901_981e_861d_9b02_9911_981e_861d"
          .U(width.W),
        "hac7c_d0b0_60e4_8428_ac7c_d0b0_60e4_8428_60ad_c683_4596_d3b8_6bad_c683_4596_d3b8"
          .U(width.W),
        "hb317_a4b0_142b_3f8c_b317_a4b0_142b_3f8c_0a06_1c83_9fc1_5e5f_0a16_1c83_9fc1_5e54"
          .U(width.W),
      )
      makeMBTest(c, TransmitPattern.LFSR, testVector, 13)
    }
  }
  it should "detect MB valtrain pattern" in {
    test(new PatternReader(sbParams, afeParams, maxPatternCount = 2048)) { c =>
      val width = afeParams.mbLanes * afeParams.mbSerializerRatio
      var testVector = Seq(
        BigInt("11110000" * (width / 8), 2).U,
        BigInt("11110000" * (width / 8), 2).U,
        BigInt("11110000" * (width / 8), 2).U,
        BigInt("11110000" * (width / 8), 2).U,
      )
      makeMBTest(c, TransmitPattern.VALTRAIN, testVector, 0)

      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)

      testVector = Seq(
        toggleBits(BigInt("11110000" * (width / 8), 2), 0, 31, 249, 2).U,
        toggleBits(BigInt("11110000" * (width / 8), 2), 1, 8, 9).U,
        toggleBits(BigInt("11110000" * (width / 8), 2), 0, 2, 49, 9).U,
        toggleBits(BigInt("11110000" * (width / 8), 2), 1).U,
      )
      makeMBTest(c, TransmitPattern.VALTRAIN, testVector, 12)

      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)

      testVector = Seq(
        toggleBits(BigInt("11110000" * (width / 8), 2), 3, 241).U,
        toggleBits(BigInt("11110000" * (width / 8), 2), 9).U,
        toggleBits(BigInt("11110000" * (width / 8), 2), 49, 0).U,
        toggleBits(BigInt("11110000" * (width / 8), 2)).U,
      )
      makeMBTest(c, TransmitPattern.VALTRAIN, testVector, 5)
    }
  }
  it should "detect MB per-lane ID pattern" in {
    test(new PatternReader(sbParams, afeParams, maxPatternCount = 2048)) { c =>
      val pattern = BigInt(
        "1010000011111010" +
          "1010000011101010" +
          "1010000011011010" +
          "1010000011001010" +
          "1010000010111010" +
          "1010000010101010" +
          "1010000010011010" +
          "1010000010001010" +
          "1010000001111010" +
          "1010000001101010" +
          "1010000001011010" +
          "1010000001001010" +
          "1010000000111010" +
          "1010000000101010" +
          "1010000000011010" +
          "1010000000001010",
        2,
      )

      var testVector = Seq(
        pattern.U,
        pattern.U,
        pattern.U,
        pattern.U,
      )
      makeMBTest(c, TransmitPattern.PER_LANE_ID, testVector, 0)

      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)

      testVector = Seq(
        toggleBits(pattern, 0).U,
        toggleBits(pattern, 240, 8, 9).U,
        toggleBits(pattern, 0, 2, 49, 9).U,
        toggleBits(pattern, 1, 200).U,
        toggleBits(pattern, 1).U,
      )
      makeMBTest(c, TransmitPattern.PER_LANE_ID, testVector, 11)

      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)

      testVector = Seq(
        toggleBits(pattern, 0).U,
        toggleBits(pattern, 1, 240, 9).U,
      )
      makeMBTest(c, TransmitPattern.PER_LANE_ID, testVector, 4)
    }
  }

  private def toggleBits(
      input: BigInt,
      bits: Int*,
  ): BigInt = {
    var result = input
    for (bit <- bits) {
      result ^= BigInt(1) << bit
    }
    BigInt(result.toString(2), 2)
  }

  private def makeMBTest(
      c: PatternReader,
      transmitPattern: TransmitPattern.Type,
      testVector: Seq[UInt],
      errorCountExpected: Int,
  ): Unit = {
    initPorts(c)
    val width = afeParams.mbLanes * afeParams.mbSerializerRatio
    createRequest(
      c,
      transmitPattern,
      patternCountMax = width * testVector.length,
      sideband = false,
    )
    c.io.sbRxData.ready.expect(false.B)

    testVector.foreach(vec => {
      c.io.resp.complete.expect(false.B)
      c.io.resp.inProgress.expect(true.B)
      c.io.sbRxData.ready.expect(false.B)
      c.io.mbRxData.enqueueNow(vec)
    })

    c.io.sbRxData.ready.expect(false.B)
    c.io.resp.complete.expect(true.B)
    c.io.resp.errorCount.expect(errorCountExpected.U)
  }
  private def createRequest(
      c: PatternReader,
      transmitPattern: TransmitPattern.Type,
      patternCountMax: Int,
      sideband: Boolean,
  ): Unit = {
    c.io.request.bits.pattern.poke(transmitPattern)
    c.io.request.bits.patternCountMax.poke(patternCountMax)
    c.io.request.bits.sideband.poke(sideband.B)
    c.io.request.valid.poke(true.B)
    c.io.resp.complete.expect(false.B)
    c.io.resp.errorCount.expect(0.U)
    c.io.resp.inProgress.expect(false.B)
    c.clock.step()
  }

  private def initPorts(c: PatternReader): Unit = {
    c.io.sbRxData.initSource()
    c.io.sbRxData.setSourceClock(c.clock)
    c.io.mbRxData.initSource()
    c.io.mbRxData.setSourceClock(c.clock)
  }

}
