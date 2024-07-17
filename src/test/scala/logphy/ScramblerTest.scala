package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chiseltest._
import edu.berkeley.cs.ucie.digital.interfaces.AfeParams
import org.scalatest.funspec.AnyFunSpec

class ScramblerTest extends AnyFunSpec with ChiselScalatestTester {

  describe("Scrambler") {
    it("dummy test") {
      test(new UCIeScrambler(afeParams = AfeParams(), numLanes = 16)) { c =>
        c.io.valid.poke(true.B)
        for (i <- 0 until 16) {
          c.io.data_in(i).poke(0.U)
        }
        for (_ <- 0 until 4) {
          println()
          c.io.valid.poke(true.B)
          for (i <- 0 until 16) {
            println(c.io.data_out(i).peek().litValue.toString(16))
          }
          c.clock.step()
        }
      }
    }

    it("4 lane scrambler test") {
      test(new UCIeScrambler(afeParams = AfeParams(), numLanes = 4)) { c =>
        c.reset.poke(true.B)
        c.clock.step()
        c.clock.step()
        c.reset.poke(false.B)
        c.clock.step()
        c.io.valid.poke(true.B)

        c.io.data_in(0).poke(1.U(16.W))
        c.io.data_in(1).poke(1012.U(16.W))
        c.io.data_in(2).poke(823.U(16.W))
        c.io.data_in(3).poke(134.U(16.W))

        c.io.data_out(0).expect(49085.U(16.W))
        c.io.data_out(1).expect(1103.U(16.W))
        c.io.data_out(2).expect(50263.U(16.W))
        c.io.data_out(3).expect(49245.U(16.W))
        c.clock.step()
        c.io.data_in(0).poke(203.U(16.W))
        c.io.data_in(1).poke(176.U(16.W))
        c.io.data_in(2).poke(21.U(16.W))
        c.io.data_in(3).poke(5847.U(16.W))
        c.io.data_out(0).expect(65321.U(16.W))
        c.io.data_out(1).expect(56489.U(16.W))
        c.io.data_out(2).expect(11245.U(16.W))
        c.io.data_out(3).expect(57654.U(16.W))
      }
    }
  }
}
