package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import sideband.SidebandParams
import interfaces._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PatternWriterTest extends AnyFlatSpec with ChiselScalatestTester {
  val afeParams = AfeParams()
  val sbParams = SidebandParams()
  behavior of "sideband pattern writer"
  it should "send SB clock pattern" in {
    test(new PatternWriter(sbParams, afeParams)) { c =>
      initPorts(c)
      createRequest(
        c,
        TransmitPattern.CLOCK,
        sideband = true,
        patternCountMax = 512,
      )

      c.clock.step()
      for (_ <- 0 until 512 / sbParams.sbNodeMsgWidth) {
        c.io.mbTxData.expectInvalid()
        c.io.resp.complete.expect(false.B)
        c.io.resp.inProgress.expect(true.B)
        c.io.sbTxData.expectDequeueNow(
          ("h" + "aaaa" * (sbParams.sbNodeMsgWidth / 16)).U,
        )
      }

      c.io.resp.complete.expect(true.B)

    }
  }

  it should "send MB LFSR pattern" in {
    test(new PatternWriter(sbParams, afeParams)) { c =>
      initPorts(c)
      createRequest(
        c,
        TransmitPattern.LFSR,
        false,
        afeParams.mbSerializerRatio * afeParams.mbLanes * 4,
      )

      c.clock.step()
      val mbWidth = afeParams.mbSerializerRatio * afeParams.mbLanes
      val lfsrValues =
        Seq(
          "hb877_cf0f_c0c7_07bf_b877_cf0f_c0c7_07bf_07ce_c912_db60_bbbc_07ce_c912_db60_bbbc"
            .U(mbWidth.W),
          "h85d7_5241_13be_ad28_85d7_5241_13be_ad28_9b02_9901_981e_861d_9b02_9901_981e_861d"
            .U(mbWidth.W),
          "hac7c_d0b0_60e4_8428_ac7c_d0b0_60e4_8428_6bad_c683_4596_d3b8_6bad_c683_4596_d3b8"
            .U(mbWidth.W),
          "hb317_a4b0_142b_3f8c_b317_a4b0_142b_3f8c_0a16_1c83_9fc1_5e54_0a16_1c83_9fc1_5e54"
            .U(mbWidth.W),
        )
      for (i <- 0 until 4) {
        c.io.sbTxData.expectInvalid()
        c.io.resp.complete.expect(false.B)
        c.io.resp.inProgress.expect(true.B)
        c.io.mbTxData.expectDequeueNow(
          lfsrValues(i),
        )
      }

      c.io.resp.complete.expect(true.B)

    }
  }

  it should "send MB valtrain pattern" in {
    test(new PatternWriter(sbParams, afeParams)) { c =>
      initPorts(c)
      createRequest(
        c,
        TransmitPattern.VALTRAIN,
        sideband = false,
        patternCountMax = 512,
      )

      c.clock.step()
      val width = afeParams.mbSerializerRatio * afeParams.mbLanes
      for (_ <- 0 until 512 / width) {
        c.io.sbTxData.expectInvalid()
        c.io.resp.complete.expect(false.B)
        c.io.resp.inProgress.expect(true.B)
        c.io.mbTxData.expectDequeueNow(
          ("b" + "1111_0000" * (width / 8)).U,
        )
      }

      c.io.resp.complete.expect(true.B)

    }
  }

  it should "send MB per-lane ID pattern" in {
    test(new PatternWriter(sbParams, afeParams)) { c =>
      initPorts(c)
      createRequest(
        c,
        TransmitPattern.PER_LANE_ID,
        false,
        512,
      )

      c.clock.step()
      val width = afeParams.mbSerializerRatio * afeParams.mbLanes
      for (_ <- 0 until 512 / width) {
        c.io.sbTxData.expectInvalid()
        c.io.resp.complete.expect(false.B)
        c.io.resp.inProgress.expect(true.B)
        c.io.mbTxData.expectDequeueNow(
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
      }

      c.io.resp.complete.expect(true.B)

    }
  }

  private def createRequest(
      c: PatternWriter,
      transmitPattern: TransmitPattern.Type,
      sideband: Boolean,
      patternCountMax: Int,
  ): Unit = {
    c.io.request.valid.poke(true.B)
    c.io.request.bits.pattern.poke(transmitPattern)
    c.io.request.bits.sideband.poke(sideband.B)
    c.io.request.bits.patternCountMax.poke(patternCountMax.U)
    c.io.resp.complete.expect(false.B)
    c.io.resp.inProgress.expect(false.B)
    c.io.sbTxData.expectInvalid()
    c.io.mbTxData.expectInvalid()
  }

  private def initPorts(c: PatternWriter) = {
    c.io.mbTxData.initSink()
    c.io.mbTxData.setSinkClock(c.clock)
    c.io.sbTxData.initSink()
    c.io.sbTxData.setSinkClock(c.clock)
  }

}
