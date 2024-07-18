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

  private def initPorts(c: PatternGenerator) = {
    c.io.patternGeneratorIO.transmitReq
      .initSource()
      .setSourceClock(c.clock)
    c.io.patternGeneratorIO.transmitPatternStatus
      .initSink()
      .setSinkClock(c.clock)
    c.io.sidebandLaneIO.rxData
      .initSource()
      .setSourceClock(c.clock)
    c.io.sidebandLaneIO.txData
      .initSink()
      .setSinkClock(c.clock)
  }

  private def testClockPatternSideband(c: PatternGenerator): Unit = {
    val length = 2

    c.io.patternGeneratorIO.transmitReq.ready.expect(true)
    c.io.sidebandLaneIO.rxData.ready.expect(false)
    c.io.mainbandLaneIO.rxData.ready.expect(false)
    c.io.sidebandLaneIO.txData.expectInvalid()
    c.io.mainbandLaneIO.txData.expectInvalid()
    c.io.patternGeneratorIO.transmitPatternStatus.expectInvalid()
    c.io.patternGeneratorIO.transmitReq.enqueueNow(
      chiselTypeOf(c.io.patternGeneratorIO.transmitReq.bits).Lit(
        _.pattern -> TransmitPattern.CLOCK,
        _.patternCountMax -> (length * sbParams.sbNodeMsgWidth).U,
        _.patternDetectedCountMax -> (length * sbParams.sbNodeMsgWidth).U,
        _.timeoutCycles -> 80.U,
        _.sideband -> true.B,
      ),
    )

    val testVector =
      Seq.fill(2)("haaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa".U)

    fork {
      c.io.sidebandLaneIO.rxData.enqueueSeq(testVector)
    }.fork {
      c.io.sidebandLaneIO.txData.expectDequeueSeq(testVector)
    }.join()

    c.io.patternGeneratorIO.transmitPatternStatus
      .expectDequeue(MessageRequestStatusType.SUCCESS)
  }
}
