package edu.berkeley.cs.ucie.digital
package logphy

import chiseltest._
import freechips.rocketchip.util.AsyncQueueParams
import sideband._
import interfaces._
import org.scalatest.flatspec.AnyFlatSpec

class LogPhyTest extends AnyFlatSpec with ChiselScalatestTester {
  val linkTrainingParams = LinkTrainingParams()
  val afeParams = AfeParams()
  val rdiParams = RdiParams()
  val fdiParams = FdiParams()
  val sbParams = SidebandParams()
  val laneAsyncQueueParams = AsyncQueueParams()

  /** This test only exists to check for FIRRTL compilation errors. Cannot do an
    * actual top-level test because Chiseltest does not support multiple clock
    * domains.
    */
  behavior of "logical phy"
  ignore should "" in {
    test(
      new LogicalPhy(
        linkTrainingParams = linkTrainingParams,
        afeParams = afeParams,
        rdiParams = rdiParams,
        fdiParams = fdiParams,
        sbParams = sbParams,
        laneAsyncQueueParams = laneAsyncQueueParams,
      ),
    ) { c => }
  }
}
