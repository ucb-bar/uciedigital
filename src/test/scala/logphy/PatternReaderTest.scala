package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor
import sideband.SidebandParams
import interfaces._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

class PatternReaderTest extends AnyFlatSpec with ChiselScalatestTester {
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
  behavior of "pattern reader"
  it should "detect SB clock pattern" in {
    test(new PatternReader(sbParams, afeParams, 1024)) { c =>
      val maxPatternWidth = log2Ceil(1024 + 1)
      initPorts(c)
      createRequest(
        c,
        TransmitPattern.CLOCK,
        sbParams.sbNodeMsgWidth * 4,
        true,
        maxPatternWidth,
      )
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
      val maxPatternWidth = log2Ceil(2048 + 1)

      makeMBTest(
        c,
        TransmitPattern.LFSR,
        lfsrVals.map(f =>
          Vec.Lit(f.map(_.U(afeParams.mbSerializerRatio.W)): _*),
        ),
        errorCountExpected =
          Vec.Lit(Seq.fill(afeParams.mbLanes)(0.U(maxPatternWidth.W)): _*),
        maxErrorCountWidth = maxPatternWidth,
      )
    }
  }
  it should "detect MB LFSR pattern error count" in {
    test(new PatternReader(sbParams, afeParams, maxPatternCount = 2048)) { c =>
      val width = afeParams.mbLanes * afeParams.mbSerializerRatio
      val maxPatternCountWidth = log2Ceil(2048 + 1)
      var (testVecs, errVecs) = createExpErrVecs(lfsrVals)

      println(f"errVecs= $errVecs")
      makeMBTest(
        c,
        TransmitPattern.LFSR,
        testVecs.map(vec =>
          Vec.Lit(vec.toSeq.map(_.U(afeParams.mbSerializerRatio.W)): _*),
        ),
        Vec.Lit(errVecs.map(_.U(maxPatternCountWidth.W)): _*),
        maxPatternCountWidth,
      )

      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)

      var res = createExpErrVecs(lfsrVals)
      testVecs = res._1
      errVecs = res._2
      println(f"errVecs= $errVecs")

      makeMBTest(
        c,
        TransmitPattern.LFSR,
        testVecs.map(vec =>
          Vec.Lit(vec.toSeq.map(_.U(afeParams.mbSerializerRatio.W)): _*),
        ),
        Vec.Lit(errVecs.map(_.U(maxPatternCountWidth.W)): _*),
        maxPatternCountWidth,
      )

      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)

      res = createExpErrVecs(lfsrVals)
      testVecs = res._1
      errVecs = res._2
      println(f"errVecs= $errVecs")
      makeMBTest(
        c,
        TransmitPattern.LFSR,
        testVecs.map(vec =>
          Vec.Lit(vec.toSeq.map(_.U(afeParams.mbSerializerRatio.W)): _*),
        ),
        Vec.Lit(errVecs.map(_.U(maxPatternCountWidth.W)): _*),
        maxPatternCountWidth,
      )

    }
  }
  it should "detect MB valtrain pattern" in {
    test(new PatternReader(sbParams, afeParams, maxPatternCount = 2048)) { c =>
      var numVecs = 4
      val maxPatternCountWidth = log2Ceil(2048 + 1)
      var testVector = Seq.fill(numVecs)(
        Seq.fill(afeParams.mbLanes)(
          BigInt("11110000" * (afeParams.mbSerializerRatio / 8), 2),
        ),
      )
      var (testVecs, errVecs) = createExpErrVecs(testVector)

      println(f"errVecs= $errVecs")
      makeMBTest(
        c,
        TransmitPattern.VALTRAIN,
        testVecs.map(vec =>
          Vec.Lit(vec.toSeq.map(_.U(afeParams.mbSerializerRatio.W)): _*),
        ),
        Vec.Lit(errVecs.map(_.U(maxPatternCountWidth.W)): _*),
        maxPatternCountWidth,
      )

      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)

      numVecs = 10
      testVector = Seq.fill(numVecs)(
        Seq.fill(afeParams.mbLanes)(
          BigInt("11110000" * (afeParams.mbSerializerRatio / 8), 2),
        ),
      )

      val res = createExpErrVecs(testVector)
      testVecs = res._1
      errVecs = res._2

      println(f"errVecs= $errVecs")
      makeMBTest(
        c,
        TransmitPattern.VALTRAIN,
        testVecs.map(vec =>
          Vec.Lit(vec.toSeq.map(_.U(afeParams.mbSerializerRatio.W)): _*),
        ),
        Vec.Lit(errVecs.map(_.U(maxPatternCountWidth.W)): _*),
        maxPatternCountWidth,
      )

    }
  }

  private def createExpErrVecs(
      testVector: Seq[Seq[BigInt]],
  ): (Seq[mutable.Seq[BigInt]], Seq[Int]) = {
    val testErrSeq = testVector.map(vec =>
      TestUtils.makeRandomErrors(vec, afeParams.mbSerializerRatio),
    )
    val testVecs = testErrSeq.map(_._1)
    val errVecs =
      testErrSeq.map(_._2).reduce((x, y) => x.zip(y).map(f => f._1 + f._2))
    (testVecs, errVecs)
  }

  it should "detect MB per-lane ID pattern" in {
    test(new PatternReader(sbParams, afeParams, maxPatternCount = 2048)) { c =>
      val numVecs = 5
      val maxPatternCountWidth = log2Ceil(2048 + 1)
      val testVector = Seq.fill(numVecs)(
        Seq.tabulate(afeParams.mbLanes)(i => BigInt("A" + f"$i%02X" + "A", 16)),
      )

      val (testVecs, errVecs) = createExpErrVecs(testVector)

      println(f"errVecs= $errVecs")
      makeMBTest(
        c,
        TransmitPattern.PER_LANE_ID,
        testVecs.map(vec =>
          Vec.Lit(vec.toSeq.map(_.U(afeParams.mbSerializerRatio.W)): _*),
        ),
        Vec.Lit(errVecs.map(_.U(maxPatternCountWidth.W)): _*),
        maxPatternCountWidth,
      )

      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)

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
      testVector: Seq[Vec[UInt]],
      errorCountExpected: Vec[UInt],
      maxErrorCountWidth: Int,
  ): Unit = {
    initPorts(c)
    val width = afeParams.mbLanes * afeParams.mbSerializerRatio
    createRequest(
      c,
      transmitPattern,
      patternCountMax = width * testVector.length,
      sideband = false,
      maxErrorCountWidth,
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
    c.io.resp.errorCount.expect(errorCountExpected)
  }
  private def createRequest(
      c: PatternReader,
      transmitPattern: TransmitPattern.Type,
      patternCountMax: Int,
      sideband: Boolean,
      maxPatternWidth: Int,
  ): Unit = {
    c.io.request.bits.pattern.poke(transmitPattern)
    c.io.request.bits.patternCountMax.poke(patternCountMax)
    c.io.request.valid.poke(true.B)
    c.io.resp.complete.expect(false.B)
    c.io.resp.errorCount.expect(
      Vec.Lit(
        Seq.fill(afeParams.mbLanes)(0.U(maxPatternWidth.W)): _*,
      ),
    )
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
