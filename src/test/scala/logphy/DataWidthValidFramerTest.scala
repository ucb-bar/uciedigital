package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DataWidthValidFramerTest extends AnyFlatSpec with ChiselScalatestTester {
  val params = DataWidthCouplerParams(
    inWidth = 4,
    outWidth = 16,
  )

  val ratio = params.outWidth / params.inWidth

  behavior of "data width valid framer"

  it should "enqueue and dequeue data" in {
    test(new DataWidthValidFramer(params)) { c =>
      initPorts(c)
      c.io.in.enqueueNow("ha".U)
      c.io.out.valid.expect(true.B)
      c.io.in.enqueueNow("hb".U)
      c.io.out.valid.expect(true.B)
      c.io.in.enqueueNow("hc".U)
      c.io.out.valid.expect(true.B)
      c.io.in.enqueueNow("hd".U)
      c.io.out.expectDequeueNow(
        (new DataWithValid(params)).Lit(
          _.data -> "hdcba".U,
          _.valid -> Vec.Lit(
            (Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(true.B)): _*,
          ),
        ),
      )
    }
  }

  it should "same cycle enqueue dequeue" in {
    test(new DataWidthValidFramer(params)) { c =>
      c.io.out.valid.expect(false.B)
      c.io.in.ready.expect(true.B)
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke("ha".U)

      c.clock.step()

      c.io.in.bits.poke("hb".U)
      c.io.in.valid.poke(true.B)
      c.io.out.ready.poke(true.B)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(
        (new DataWithValid(params)).Lit(
          _.data -> "h00ba".U,
          _.valid -> Vec.Lit(
            (Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B)): _*,
          ),
        ),
      )

      c.clock.step()

      c.io.in.bits.poke("hc".U)
      c.io.in.valid.poke(true.B)
      c.io.out.valid.expect(true.B)
      c.io.out.ready.poke(false.B)
      c.io.out.bits.expect(
        (new DataWithValid(params)).Lit(
          _.data -> "h000c".U,
          _.valid -> Vec.Lit(
            (Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B)): _*,
          ),
        ),
      )

      c.clock.step()
      c.io.in.bits.poke("hd".U)
      c.io.in.valid.poke(true.B)
      c.io.out.valid.expect(true.B)
      c.io.out.ready.poke(true.B)
      c.io.out.bits.expect(
        (new DataWithValid(params)).Lit(
          _.data -> "h00dc".U,
          _.valid -> Vec.Lit(
            (Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B)): _*,
          ),
        ),
      )
    }
  }

  it should "one element enqueue dequeue" in {
    test(new DataWidthValidFramer(params)) { c =>
      c.io.in.ready.expect(true.B)
      c.io.in.bits.poke("ha".U)
      c.io.in.valid.poke(true.B)
      c.io.out.valid.expect(true.B)
      c.io.out.ready.poke(true.B)
      c.io.out.bits.expect(
        (new DataWithValid(params)).Lit(
          _.data -> "h000a".U,
          _.valid -> Vec.Lit(
            (Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B)): _*,
          ),
        ),
      )

      c.clock.step()

      c.io.in.ready.expect(true.B)
      c.io.in.bits.poke("hb".U)
      c.io.in.valid.poke(true.B)
      c.io.out.valid.expect(true.B)
      c.io.out.ready.poke(true.B)
      c.io.out.bits.expect(
        (new DataWithValid(params)).Lit(
          _.data -> "h000b".U,
          _.valid -> Vec.Lit(
            (Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B)): _*,
          ),
        ),
      )

      c.clock.step()

      c.io.in.ready.expect(true.B)
      c.io.in.bits.poke("hc".U)
      c.io.in.valid.poke(true.B)
      c.io.out.valid.expect(true.B)
      c.io.out.ready.poke(false.B)
      c.io.out.bits.expect(
        (new DataWithValid(params)).Lit(
          _.data -> "h000c".U,
          _.valid -> Vec.Lit(
            (Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B)): _*,
          ),
        ),
      )

      c.clock.step()

      c.io.in.bits.poke("hd".U)
      c.io.in.valid.poke(true.B)
      c.io.out.ready.poke(true.B)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(
        (new DataWithValid(params)).Lit(
          _.data -> "h00dc".U,
          _.valid -> Vec.Lit(
            (Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(true.B) ++
              Seq.fill(ratio)(false.B) ++
              Seq.fill(ratio)(false.B)): _*,
          ),
        ),
      )

      c.clock.step()
    }
  }

  private def initPorts(c: DataWidthValidFramer) = {
    c.io.in.initSource().setSourceClock(c.clock)
    c.io.out.initSink().setSinkClock(c.clock)
  }
}
