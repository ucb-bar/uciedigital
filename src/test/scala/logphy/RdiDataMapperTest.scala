package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import interfaces._

class RdiDataMapperTest extends AnyFlatSpec with ChiselScalatestTester {
  val rdiParams = RdiParams(width = 128, sbWidth = 128)
  val afeParams = AfeParams()
  behavior of "rdi data mapper"
  it should "correctly output rx lane data" in {
    test(new RdiDataMapper(rdiParams, afeParams)) { c =>
      val data = Seq(
        "h1234_5678_9abc_def0_0fed_cba9_8765_4321_1111_2222_3333_4444_5555_6666_7777_8888".U,
        "h2222_2222_3333_4444_5555_6666_8888_8888_2234_5688_9abc_def0_0fed_cba9_8865_4322".U,
        "h1244_6678_9abc_def0_0fed_cba9_8766_4421_1111_2222_4444_4444_6666_6666_7777_8888".U,
        "h1111_3333_3333_4444_5555_6666_7777_8888_1334_5678_9aac_def0_0fed_caa9_8765_4331".U,
      )
      val dataUInt = ("h" +
        "1111_3333_3333_4444_5555_6666_7777_8888_1334_5678_9aac_def0_0fed_caa9_8765_4331" +
        "1244_6678_9abc_def0_0fed_cba9_8766_4421_1111_2222_4444_4444_6666_6666_7777_8888" +
        "2222_2222_3333_4444_5555_6666_8888_8888_2234_5688_9abc_def0_0fed_cba9_8865_4322" +
        "1234_5678_9abc_def0_0fed_cba9_8765_4321_1111_2222_3333_4444_5555_6666_7777_8888").U

      c.io.mainbandLaneIO.rxData.initSource()
      c.io.mainbandLaneIO.rxData.setSourceClock(c.clock)

      c.io.mainbandLaneIO.rxData.valid.poke(false)
      c.clock.step()
      for (i: Int <- 0 until 4) {
        for (_: Int <- 0 until 10) {
          c.io.rdi.plData.valid.expect(false)
          c.clock.step()
        }
        c.io.mainbandLaneIO.rxData.enqueueNow(data(i))
      }
      c.io.rdi.plData.valid.expect(true.B)
      c.io.rdi.plData.bits.expect(dataUInt)
      c.clock.step()
      for (_ <- 0 until 10) {
        c.io.rdi.plData.valid.expect(false)
        c.clock.step()
      }
    }
  }

  it should "correctly output tx lane data" in {
    test(new RdiDataMapper(rdiParams, afeParams)) { c =>
      val data = Seq(
        "h1234_5678_9abc_def0_0fed_cba9_8765_4321_1111_2222_3333_4444_5555_6666_7777_8888".U,
        "h2222_2222_3333_4444_5555_6666_8888_8888_2234_5688_9abc_def0_0fed_cba9_8865_4322".U,
        "h1244_6678_9abc_def0_0fed_cba9_8766_4421_1111_2222_4444_4444_6666_6666_7777_8888".U,
        "h1111_3333_3333_4444_5555_6666_7777_8888_1334_5678_9aac_def0_0fed_caa9_8765_4331".U,
      )
      val dataUInt = ("h" +
        "1111_3333_3333_4444_5555_6666_7777_8888_1334_5678_9aac_def0_0fed_caa9_8765_4331" +
        "1244_6678_9abc_def0_0fed_cba9_8766_4421_1111_2222_4444_4444_6666_6666_7777_8888" +
        "2222_2222_3333_4444_5555_6666_8888_8888_2234_5688_9abc_def0_0fed_cba9_8865_4322" +
        "1234_5678_9abc_def0_0fed_cba9_8765_4321_1111_2222_3333_4444_5555_6666_7777_8888").U

      c.io.mainbandLaneIO.txData.initSink()
      c.io.mainbandLaneIO.txData.setSinkClock(c.clock)

      c.io.mainbandLaneIO.txData.valid.expect(false.B)
      c.io.rdi.lpData.valid.poke(false.B)
      c.io.rdi.lpData.irdy.expect(false.B)
      c.io.rdi.lpData.ready.expect(true.B)
      c.clock.step()

      c.io.rdi.lpData.valid.poke(true.B)
      c.io.rdi.lpData.irdy.poke(true.B)
      c.io.rdi.lpData.bits.poke(dataUInt)
      c.io.rdi.lpData.ready.expect(true.B)
      c.clock.step()

      c.io.rdi.lpData.valid.poke(false.B)

      for (i: Int <- 0 until 4) {
        c.io.mainbandLaneIO.txData.ready.poke(false)
        for (_: Int <- 0 until 10) {
          c.io.rdi.lpData.ready.expect(false)
          c.clock.step()
        }
        c.io.mainbandLaneIO.txData.expectDequeueNow(data(i))
      }

      for (_ <- 0 until 10) {
        c.io.mainbandLaneIO.txData.valid.expect(false)
        c.io.rdi.lpData.ready.expect(true)
        c.clock.step()
      }
    }
  }
}
