package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor
import sideband.SidebandParams
import interfaces._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PatternWriterTest extends AnyFlatSpec with ChiselScalatestTester {
  val afeParams = AfeParams()
  val sbParams = SidebandParams()
  behavior of "sideband pattern writer"
  it should "send SB clock pattern" in {
    test(new PatternWriter(sbParams, afeParams, maxPatternCount = 2048)) { c =>
      initPorts(c)
      createRequest(
        c,
        TransmitPattern.CLOCK,
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
  it should "send MB LFSR pattern" in {
    test(new PatternWriter(sbParams, afeParams, maxPatternCount = 2048)) { c =>
      initPorts(c)
      createRequest(
        c,
        TransmitPattern.LFSR,
        afeParams.mbSerializerRatio * afeParams.mbLanes * 4,
      )

      val lfsrValVecs = lfsrVals.map(f =>
        Vec.Lit(f.map(_.U(afeParams.mbSerializerRatio.W)): _*),
      )

      c.clock.step()
      for (i <- 0 until 4) {
        c.io.sbTxData.expectInvalid()
        c.io.resp.complete.expect(false.B)
        c.io.resp.inProgress.expect(true.B)
        c.io.mbTxData.expectDequeueNow(
          lfsrValVecs(i),
        )
      }

      c.io.resp.complete.expect(true.B)

    }
  }

  it should "send MB valtrain pattern" in {
    val maxPatternCount = 2048
    test(
      new PatternWriter(sbParams, afeParams, maxPatternCount = maxPatternCount),
    ) { c =>
      initPorts(c)
      val patternCountMax = 512
      createRequest(
        c,
        TransmitPattern.VALTRAIN,
        patternCountMax = patternCountMax,
      )

      val numVecs =
        patternCountMax / (afeParams.mbLanes * afeParams.mbSerializerRatio)

      val valtrain = Seq.fill(numVecs)(
        Seq.fill(afeParams.mbLanes)(
          BigInt("11110000" * (afeParams.mbSerializerRatio / 8), 2),
        ),
      )

      c.clock.step()
      for (i <- 0 until numVecs) {
        c.io.sbTxData.expectInvalid()
        c.io.resp.complete.expect(false.B)
        c.io.resp.inProgress.expect(true.B)
        c.io.mbTxData.expectDequeueNow(
          Vec.Lit(valtrain(i).map(_.U(afeParams.mbSerializerRatio.W)): _*),
        )
      }

      c.io.resp.complete.expect(true.B)

    }
  }

  it should "send MB per-lane ID pattern" in {
    test(new PatternWriter(sbParams, afeParams, maxPatternCount = 2048)) { c =>
      initPorts(c)
      val numVecs = 5
      val perLaneVec = Seq.fill(numVecs)(
        Seq.tabulate(afeParams.mbLanes)(i => BigInt("A" + f"$i%02X" + "A", 16)),
      )

      createRequest(
        c,
        TransmitPattern.PER_LANE_ID,
        numVecs * afeParams.mbSerializerRatio * afeParams.mbLanes,
      )

      c.clock.step()
      for (i <- 0 until numVecs) {
        c.io.sbTxData.expectInvalid()
        c.io.resp.complete.expect(false.B)
        c.io.resp.inProgress.expect(true.B)
        c.io.mbTxData.expectDequeueNow(
          Vec.Lit(perLaneVec(i).map(_.U(afeParams.mbSerializerRatio.W)): _*),
        )
      }

      c.io.resp.complete.expect(true.B)

    }
  }

  private def createRequest(
      c: PatternWriter,
      transmitPattern: TransmitPattern.Type,
      patternCountMax: Int,
  ): Unit = {
    c.io.request.valid.poke(true.B)
    c.io.request.bits.pattern.poke(transmitPattern)
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
