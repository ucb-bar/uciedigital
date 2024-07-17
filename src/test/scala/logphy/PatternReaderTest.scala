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

      c.io.request.bits.pattern.poke(TransmitPattern.CLOCK)
      c.io.request.bits.patternCountMax.poke(sbParams.sbNodeMsgWidth * 4)
      c.io.request.bits.sideband.poke(true.B)
      c.io.request.valid.poke(true.B)
      c.io.resp.complete.expect(false.B)
      c.io.resp.errorCount.expect(0.U)
      c.io.resp.inProgress.expect(false.B)
      c.clock.step()

      val testVector =
        Seq.fill(4)("haaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa".U)

      testVector.foreach(vec => {
        c.io.resp.complete.expect(false.B)
        c.io.resp.inProgress.expect(true.B)
        c.io.sbRxData.enqueueNow(vec)
      })

      c.io.resp.complete.expect(true.B)
    }
  }

  private def initPorts(c: PatternReader): Unit = {
    c.io.sbRxData.initSource()
    c.io.sbRxData.setSourceClock(c.clock)
    c.io.mbRxData.initSource()
    c.io.mbRxData.setSourceClock(c.clock)
  }

  it should "detect MB LFSR pattern" in {}
  it should "detect MB LFSR pattern error count" in {}
  it should "detect MB valtrain pattern" in {}
  it should "detect MB valtrain pattern error count" in {}
  it should "detect MB per-lane ID pattern" in {}
  it should "detect MB per-lane ID pattern error count" in {}

}
