package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor
import sideband.SidebandParams
import interfaces._
import org.scalatest.flatspec.AnyFlatSpec

class PatternGeneratorTest extends AnyFlatSpec with ChiselScalatestTester {
  val afeParams = AfeParams()
  val sbParams = SidebandParams()
  val lfsrVals = Seq(
    Seq(
      BigInt("bfbc", 16),
      BigInt("07bb", 16),
      BigInt("c760", 16),
      BigInt("c0db", 16),
      BigInt("0f12", 16),
      BigInt("cfc9", 16),
      BigInt("77ce", 16),
      BigInt("b807", 16),
      BigInt("bfbc", 16),
      BigInt("07bb", 16),
      BigInt("c760", 16),
      BigInt("c0db", 16),
      BigInt("0f12", 16),
      BigInt("cfc9", 16),
      BigInt("77ce", 16),
      BigInt("b807", 16),
    ),
    Seq(
      BigInt("281d", 16),
      BigInt("ad86", 16),
      BigInt("be1e", 16),
      BigInt("1398", 16),
      BigInt("4101", 16),
      BigInt("5299", 16),
      BigInt("d702", 16),
      BigInt("859b", 16),
      BigInt("281d", 16),
      BigInt("ad86", 16),
      BigInt("be1e", 16),
      BigInt("1398", 16),
      BigInt("4101", 16),
      BigInt("5299", 16),
      BigInt("d702", 16),
      BigInt("859b", 16),
    ),
    Seq(
      BigInt("28b8", 16),
      BigInt("84d3", 16),
      BigInt("e496", 16),
      BigInt("6045", 16),
      BigInt("b083", 16),
      BigInt("d0c6", 16),
      BigInt("7cad", 16),
      BigInt("ac6b", 16),
      BigInt("28b8", 16),
      BigInt("84d3", 16),
      BigInt("e496", 16),
      BigInt("6045", 16),
      BigInt("b083", 16),
      BigInt("d0c6", 16),
      BigInt("7cad", 16),
      BigInt("ac6b", 16),
    ),
    Seq(
      BigInt("8c54", 16),
      BigInt("3f5e", 16),
      BigInt("2bc1", 16),
      BigInt("149f", 16),
      BigInt("b083", 16),
      BigInt("a41c", 16),
      BigInt("1716", 16),
      BigInt("b30a", 16),
      BigInt("8c54", 16),
      BigInt("3f5e", 16),
      BigInt("2bc1", 16),
      BigInt("149f", 16),
      BigInt("b083", 16),
      BigInt("a41c", 16),
      BigInt("1716", 16),
      BigInt("b30a", 16),
    ),
  )

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
    val maxPatternCount = 1024
    test(
      new PatternGenerator(
        afeParams = afeParams,
        sbParams = sbParams,
        maxPatternCount = maxPatternCount,
      ),
    ) { c =>
      initPorts(c)
      val maxPatternCountWidth = log2Ceil(maxPatternCount + 1)
      val width = afeParams.mbSerializerRatio * afeParams.mbLanes

      val expectedTx = lfsrVals
        .map(
          TestUtils
            .lanesToOne(_, afeParams.mbLanes, afeParams.mbSerializerRatio),
        )
        .map(_.U(width.W))
      var rxReceived = lfsrVals
        .map(
          TestUtils
            .lanesToOne(_, afeParams.mbLanes, afeParams.mbSerializerRatio),
        )

      /** expected case with no errors */
      testMainband(
        c = c,
        transmitPattern = TransmitPattern.LFSR,
        patternCountMax = 4,
        patternDetectedCountMax = 4,
        timeoutCycles = 80,
        mainbandRx = rxReceived.map(f => f.U(width.W)),
        mainbandTx = expectedTx,
        expectedResult = MessageRequestStatusType.SUCCESS,
        expectedErrorCount = Vec.Lit(
          Seq.fill(afeParams.mbLanes)(0.U(maxPatternCountWidth.W)): _*,
        ),
      )

      val numTests = 4
      for (_ <- 0 until numTests) {
        val (rxRecv, err) =
          TestUtils.createExpErrVecs(lfsrVals, afeParams.mbSerializerRatio)
        testMainband(
          c = c,
          transmitPattern = TransmitPattern.LFSR,
          patternCountMax = 4,
          patternDetectedCountMax = 4,
          timeoutCycles = 80,
          mainbandTx = expectedTx,
          mainbandRx = rxRecv
            .map(f =>
              TestUtils
                .lanesToOne(
                  f.toSeq,
                  afeParams.mbLanes,
                  afeParams.mbSerializerRatio,
                ),
            )
            .map(_.U),
          expectedResult = MessageRequestStatusType.SUCCESS,
          expectedErrorCount = Vec.Lit(err.map(_.U(maxPatternCountWidth.W)): _*),
        )
      }
    }
  }
  it should "handle MB timeouts" in {
    val maxPatternCount = 1024
    test(
      new PatternGenerator(
        afeParams = afeParams,
        sbParams = sbParams,
        maxPatternCount = maxPatternCount,
      ),
    ) { c =>
      initPorts(c)
      val width = afeParams.mbSerializerRatio * afeParams.mbLanes
      val maxPatternCountWidth = log2Ceil(maxPatternCount + 1)

      val expectedTx = lfsrVals
        .map(
          TestUtils
            .lanesToOne(_, afeParams.mbLanes, afeParams.mbSerializerRatio),
        )
        .map(_.U(width.W))
      val rxReceived = lfsrVals
        .map(
          TestUtils
            .lanesToOne(_, afeParams.mbLanes, afeParams.mbSerializerRatio),
        )

      /** expected case with no errors */
      testMainband(
        c = c,
        transmitPattern = TransmitPattern.LFSR,
        patternCountMax = 4,
        patternDetectedCountMax = 4,
        timeoutCycles = 4,
        mainbandRx = rxReceived.map(f => f.U(width.W)),
        mainbandTx = expectedTx,
        expectedResult = MessageRequestStatusType.ERR,
        expectedErrorCount = Vec.Lit(
          Seq.fill(afeParams.mbLanes)(0.U(maxPatternCountWidth.W)): _*,
        ),
      )
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
    c.io.mainbandIO.rxData
      .initSource()
      .setSourceClock(c.clock)
    c.io.mainbandIO.txData
      .initSink()
      .setSinkClock(c.clock)
  }

  private def createRequest(
      c: PatternGenerator,
      transmitPattern: TransmitPattern.Type,
      patternCountMax: Int,
      patternDetectedCountMax: Int,
      timeoutCycles: Int,
  ): Unit = {
    c.io.patternGeneratorIO.transmitReq.ready.expect(true)
    c.io.sidebandLaneIO.rxData.ready.expect(false)
    c.io.mainbandIO.rxData.ready.expect(false)
    c.io.sidebandLaneIO.txData.expectInvalid()
    c.io.mainbandIO.txData.expectInvalid()
    c.io.patternGeneratorIO.resp.expectInvalid()
    c.io.patternGeneratorIO.transmitReq.enqueueNow(
      chiselTypeOf(c.io.patternGeneratorIO.transmitReq.bits).Lit(
        _.pattern -> transmitPattern,
        _.patternCountMax -> patternCountMax.U,
        _.patternDetectedCountMax -> patternDetectedCountMax.U,
        _.timeoutCycles -> timeoutCycles.U,
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
    )

    val testVector =
      Seq.fill(2)("haaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa".U)

    fork {
      c.io.sidebandLaneIO.rxData.enqueueSeq(testVector)
    }.fork {
      c.io.sidebandLaneIO.txData.expectDequeueSeq(testVector)
    }.join()

    c.io.patternGeneratorIO.resp.ready.poke(true.B)

    val resp = c.io.patternGeneratorIO.resp
    val statusExpected = MessageRequestStatusType.SUCCESS
    fork
      .withRegion(Monitor) {
        while (!resp.valid.peek().litToBoolean) {
          c.clock.step(1)
        }
        resp.valid.expect(true.B)
        resp.bits.expectPartial(
          chiselTypeOf(resp.bits).Lit(
            _.status -> statusExpected,
          ),
        )
      }
      .joinAndStep(c.clock)
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
      expectedErrorCount: Vec[UInt],
  ): Unit = {

    createRequest(
      c,
      transmitPattern,
      patternCountMax * afeParams.mbSerializerRatio * afeParams.mbLanes,
      patternDetectedCountMax * afeParams.mbSerializerRatio * afeParams.mbLanes,
      timeoutCycles,
    )

    fork {
      c.io.mainbandIO.rxData.enqueueSeq(mainbandRx)
    }.fork {
      c.io.mainbandIO.txData.expectDequeueSeq(mainbandTx)
    }.join()

    c.io.patternGeneratorIO.resp
      .expectDequeue(
        chiselTypeOf(c.io.patternGeneratorIO.resp.bits).Lit(
          _.status -> expectedResult,
          _.errorCount -> expectedErrorCount,
        ),
      )

  }
}

class PatternGeneratorLoopback(
    afeParams: AfeParams,
    sbParams: SidebandParams,
    maxPatternCount: Int,
) extends Module {

  val io = IO(new Bundle {
    val patternGeneratorIO = new PatternGeneratorIO(afeParams, maxPatternCount)
  })

  val patternGenerator = Module(
    new PatternGenerator(afeParams, sbParams, maxPatternCount),
  )

}
