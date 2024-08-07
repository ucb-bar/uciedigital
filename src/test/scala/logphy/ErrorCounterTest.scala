package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor
import org.scalatest.flatspec.AnyFlatSpec
import interfaces.AfeParams
import scala.util.Random

class ErrorCounterTest extends AnyFlatSpec with ChiselScalatestTester {
  val afeParams = AfeParams()
  val width = afeParams.mbSerializerRatio * afeParams.mbLanes * 4
  behavior of "error counter"
  it should "correctly count errors for lfsr pattern" in {
    test(new ErrorCounter(afeParams)) { c =>
      c.io.req.valid.poke(true.B)
      c.io.req.bits.pattern.poke(TransmitPattern.LFSR)

      var lfsrVals = Seq(
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
      )

      /** No errors */
      c.io.req.bits.input.poke(
        Vec.Lit(lfsrVals.map(_.U): _*),
      )

      val errWidth = log2Ceil(afeParams.mbSerializerRatio + 1)
      c.io.errorCount.expect(
        Vec.Lit(
          Seq.fill(afeParams.mbLanes)(
            0.U(errWidth.W),
          ): _*,
        ),
      )
      c.clock.step()

      val rand = new Random()
      val bitWidth = afeParams.mbSerializerRatio
      var numErrors = Seq.fill(16)(rand.nextInt(bitWidth))
      lfsrVals = Seq(
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
      )
      println(f"numErrors: $numErrors")

      c.io.req.bits.input.poke(
        Vec.Lit(
          TestUtils
            .makeRandomErrors(lfsrVals, numErrors, bitWidth)
            .toSeq
            .map(_.U(afeParams.mbSerializerRatio.W)): _*,
        ),
      )
      c.io.errorCount.expect(Vec.Lit(numErrors.map(_.U(errWidth.W)): _*))
      c.clock.step()

      numErrors = Seq.fill(16)(rand.nextInt(bitWidth))
      lfsrVals = Seq(
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
      )
      println(f"numErrors: $numErrors")

      c.io.req.bits.input.poke(
        Vec.Lit(
          TestUtils
            .makeRandomErrors(lfsrVals, numErrors, bitWidth)
            .toSeq
            .map(_.U(afeParams.mbSerializerRatio.W)): _*,
        ),
      )
      c.io.errorCount.expect(Vec.Lit(numErrors.map(_.U(errWidth.W)): _*))
      c.clock.step()

      c.io.req.valid.poke(false.B)
      for (_ <- 0 until 10) {
        c.clock.step()
      }

      lfsrVals = Seq(
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
      )

      c.io.req.bits.input.poke(
        Vec.Lit(lfsrVals.map(_.U(afeParams.mbSerializerRatio.W)): _*),
      )
      c.io.errorCount.expect(
        Vec.Lit(
          Seq.fill(afeParams.mbLanes)(0.U(errWidth.W)): _*,
        ),
      )
    }
  }
  it should "correctly count errors for valtrain pattern" in {
    test(new ErrorCounter(afeParams)) { c =>
      c.io.req.valid.poke(true.B)
      c.io.req.bits.pattern.poke(TransmitPattern.VALTRAIN)

      val valtrain = Seq.fill(afeParams.mbLanes)(
        BigInt("11110000" * (afeParams.mbSerializerRatio / 8), 2),
      )
      val errWidth = log2Ceil(afeParams.mbSerializerRatio + 1)
      val bitWidth = afeParams.mbSerializerRatio

      /** No errors */
      c.io.req.bits.input.poke(
        Vec.Lit(valtrain.map(_.U(bitWidth.W)): _*),
      )
      c.io.errorCount.expect(
        Vec.Lit(Seq.fill(afeParams.mbLanes)(0.U(errWidth.W)): _*),
      )
      c.clock.step()

      val rand = new Random()
      val numErrors = Seq.fill(16)(rand.nextInt(bitWidth))
      c.io.req.bits.input.poke(
        Vec.Lit(
          TestUtils
            .makeRandomErrors(valtrain, numErrors, bitWidth)
            .toSeq
            .map(_.U(bitWidth.W)): _*,
        ),
      )
      c.io.errorCount.expect(Vec.Lit(numErrors.map(_.U(errWidth.W)): _*))
      c.clock.step()

    }
  }
  it should "correctly count errors for per-lane ID pattern" in {
    test(new ErrorCounter(afeParams)) { c =>
      c.io.req.valid.poke(true.B)
      c.io.req.bits.pattern.poke(TransmitPattern.PER_LANE_ID)
      val errWidth = log2Ceil(afeParams.mbSerializerRatio + 1)
      val bitWidth = afeParams.mbSerializerRatio

      val perLaneId =
        Seq.tabulate(afeParams.mbLanes)(i => BigInt("A" + f"$i%02X" + "A", 16))

      /** No errors */
      c.io.req.bits.input.poke(
        Vec.Lit(perLaneId.map(_.U(bitWidth.W)): _*),
      )
      c.io.errorCount.expect(
        Vec.Lit(
          Seq.fill(afeParams.mbLanes)(0.U(errWidth.W)): _*,
        ),
      )
      c.clock.step()

      val rand = new Random()
      val numErrors = Seq.fill(16)(rand.nextInt(bitWidth))
      c.io.req.bits.input.poke(
        Vec.Lit(
          TestUtils
            .makeRandomErrors(perLaneId, numErrors, bitWidth)
            .toSeq
            .map(_.U(bitWidth.W)): _*,
        ),
      )
      c.io.errorCount.expect(
        Vec.Lit(
          numErrors.map(_.U(errWidth.W)): _*,
        ),
      )
      c.clock.step()
    }
  }
}
