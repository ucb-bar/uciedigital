package edu.berkeley.cs.ucie.digital

import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class D2DTest extends AnyFreeSpec with ChiselScalatestTester {
  "D2D Dummy Loopback module should elaborate" in {
    test(new D2DDummyLoopback()) { c =>
      c.clock.step()
    }
  }
}
