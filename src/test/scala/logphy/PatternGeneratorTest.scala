package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import sideband.SidebandParams
import interfaces._
import org.scalatest.flatspec.AnyFlatSpec

class PatternGeneratorTest extends AnyFlatSpec with ChiselScalatestTester {
  val afeParams = AfeParams()
  val sbParams = SidebandParams()
  behavior of "sideband pattern generator"
  it should "detect clock pattern no delay" in {
    test(
      new PatternGenerator(
        afeParams = afeParams,
        sbParams = sbParams,
        maxPatternCount = 1024,
      ),
    ) { c =>
      initPorts(c)
      testClockPatternSideband(c)
    }
  }
  it should "detect clock pattern no delay twice" in {
    test(
      new PatternGenerator(
        afeParams = afeParams,
        sbParams = sbParams,
        maxPatternCount = 1024,
      ),
    ) { c =>
      initPorts(c)
      testClockPatternSideband(c)
      testClockPatternSideband(c)
    }
  }
  behavior of "mainband pattern generator"
  it should "detect MB LFSR pattern" in {
    test(
      new PatternGenerator(
        afeParams = afeParams,
        sbParams = sbParams,
        maxPatternCount = 1024,
      ),
    ) { c =>
      initPorts(c)
      val width = afeParams.mbSerializerRatio * afeParams.mbLanes

      val rxReceived = Seq(
        "hb877_cf0f_c0c7_07bf_b877_cf0f_c0c7_07bf_07ce_c912_db60_bbbc_07ce_c912_db60_bbbc"
          .U(width.W),
        "h85d7_5241_13be_ad28_85d7_5241_13be_ad28_9b02_9901_981e_861d_9b02_9901_981e_861d"
          .U(width.W),
        "hac7c_d0b0_60e4_8428_ac7c_d0b0_60e4_8428_6bad_c683_4596_d3b8_6bad_c683_4596_d3b8"
          .U(width.W),
        "hb317_a4b0_142b_3f8c_b317_a4b0_142b_3f8c_0a16_1c83_9fc1_5e54_0a16_1c83_9fc1_5e54"
          .U(width.W),
      )

      val expectedTx = Seq(
        "hb877_cf0f_c0c7_07bf_b877_cf0f_c0c7_07bf_07ce_c912_db60_bbbc_07ce_c912_db60_bbbc"
          .U(width.W),
        "h85d7_5241_13be_ad28_85d7_5241_13be_ad28_9b02_9901_981e_861d_9b02_9901_981e_861d"
          .U(width.W),
        "hac7c_d0b0_60e4_8428_ac7c_d0b0_60e4_8428_6bad_c683_4596_d3b8_6bad_c683_4596_d3b8"
          .U(width.W),
        "hb317_a4b0_142b_3f8c_b317_a4b0_142b_3f8c_0a16_1c83_9fc1_5e54_0a16_1c83_9fc1_5e54"
          .U(width.W),
      )

      /** expected case with no errors */
      testMainband(
        c = c,
        transmitPattern = TransmitPattern.LFSR,
        patternCountMax = 4,
        patternDetectedCountMax = 4,
        timeoutCycles = 80,
        mainbandRx = rxReceived,
        mainbandTx = expectedTx,
        expectedResult = MessageRequestStatusType.SUCCESS,
        expectedErrorCount = 0,
      )

    }
  }

  it should "handle MB timeouts" in {
    test(
      new PatternGenerator(
        afeParams = afeParams,
        sbParams = sbParams,
        maxPatternCount = 1024,
      ),
    ) { c =>
      initPorts(c)
      assert(false, "TODO")
    }
  }

  private def initPorts(c: PatternGenerator) = {
    c.io.patternGeneratorIO.transmitReq
      .initSource()
      .setSourceClock(c.clock)
    c.io.patternGeneratorIO.resp
      .initSink()
      .setSinkClock(c.clock)
    c.io.sidebandLaneIO.rxData
      .initSource()
      .setSourceClock(c.clock)
    c.io.sidebandLaneIO.txData
      .initSink()
      .setSinkClock(c.clock)
    c.io.mainbandLaneIO.rxData
      .initSource()
      .setSourceClock(c.clock)
    c.io.mainbandLaneIO.txData
      .initSink()
      .setSinkClock(c.clock)
  }

  private def createRequest(
      c: PatternGenerator,
      transmitPattern: TransmitPattern.Type,
      patternCountMax: Int,
      patternDetectedCountMax: Int,
      timeoutCycles: Int,
      sideband: Boolean,
  ): Unit = {
    c.io.patternGeneratorIO.transmitReq.ready.expect(true)
    c.io.sidebandLaneIO.rxData.ready.expect(false)
    c.io.mainbandLaneIO.rxData.ready.expect(false)
    c.io.sidebandLaneIO.txData.expectInvalid()
    c.io.mainbandLaneIO.txData.expectInvalid()
    c.io.patternGeneratorIO.resp.expectInvalid()
    c.io.patternGeneratorIO.transmitReq.enqueueNow(
      chiselTypeOf(c.io.patternGeneratorIO.transmitReq.bits).Lit(
        _.pattern -> transmitPattern,
        _.patternCountMax -> patternCountMax.U,
        _.patternDetectedCountMax -> patternDetectedCountMax.U,
        _.timeoutCycles -> timeoutCycles.U,
        _.sideband -> sideband.B,
      ),
    )
  }

  private def testClockPatternSideband(c: PatternGenerator): Unit = {
    val length = 2

    createRequest(
      c,
      TransmitPattern.CLOCK,
      length * sbParams.sbNodeMsgWidth,
      length * sbParams.sbNodeMsgWidth,
      80,
      true,
    )

    val testVector =
      Seq.fill(2)("haaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa".U)

    fork {
      c.io.sidebandLaneIO.rxData.enqueueSeq(testVector)
    }.fork {
      c.io.sidebandLaneIO.txData.expectDequeueSeq(testVector)
    }.join()

    c.io.patternGeneratorIO.resp
      .expectDequeue(
        chiselTypeOf(c.io.patternGeneratorIO.resp.bits).Lit(
          _.status -> MessageRequestStatusType.SUCCESS,
          _.errorCount -> 0.U,
        ),
      )
  }

  private def testMainband(
      c: PatternGenerator,
      transmitPattern: TransmitPattern.Type,
      patternCountMax: Int,
      patternDetectedCountMax: Int,
      timeoutCycles: Int,
      mainbandRx: Seq[UInt],
      mainbandTx: Seq[UInt],
      expectedResult: MessageRequestStatusType.Type,
      expectedErrorCount: Int,
  ): Unit = {

    createRequest(
      c,
      transmitPattern,
      patternCountMax * afeParams.mbSerializerRatio * afeParams.mbLanes,
      patternDetectedCountMax * afeParams.mbSerializerRatio * afeParams.mbLanes,
      timeoutCycles,
      false,
    )

    fork {
      c.io.mainbandLaneIO.rxData.enqueueSeq(mainbandRx)
    }.fork {
      c.io.mainbandLaneIO.txData.expectDequeueSeq(mainbandTx)
    }.join()

    c.io.patternGeneratorIO.resp
      .expectDequeue(
        chiselTypeOf(c.io.patternGeneratorIO.resp.bits).Lit(
          _.status -> expectedResult,
          _.errorCount -> expectedErrorCount.U,
        ),
      )

  }
}
