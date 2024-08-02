package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chiseltest._
import interfaces.{AfeParams, RdiParams, SpeedMode}
import sideband.{SBMessage_factory, SBM, SidebandParams}
import org.scalatest.flatspec.AnyFlatSpec

class LinkTrainingFSMTest extends AnyFlatSpec with ChiselScalatestTester {
  val linkTrainingParams = LinkTrainingParams(pllWaitTime = 20)
  val sbParams = SidebandParams()
  val afeParams = AfeParams()
  val rdiParams = RdiParams(128, 128)

  behavior of "Link Training FSM"
  it should "correctly transition between states -- basic simulation" in {
    test(
      new LinkTrainingFSM(
        linkTrainingParams = linkTrainingParams,
        sbParams = sbParams,
        afeParams = afeParams,
      ),
    ) { c =>
      initPorts(c)
      testTransitionOutOfReset(c)
      println("Exited Reset")
      initSB(c)
      println("Successfully initialized sideband")
      initMB(c)
      println("Successfully initialized mainband")
      trainMB(c)
      println("Successfully trained mainband")

      c.io.currentState.expect(LinkTrainingState.linkInit)

    }
  }

  private def initMB(c: LinkTrainingFSM): Unit = {
    c.io.currentState.expect(LinkTrainingState.mbInit)
    c.io.sidebandFSMIO.packetTxData.expectDequeue(
      new MBInitFSMTest().formParamsReqMsg(true, linkTrainingParams),
    )
    c.io.sidebandFSMIO.rxData.enqueueNow(
      new MBInitFSMTest().formParamsReqMsg(true, linkTrainingParams),
    )
    c.clock.step(4)
    c.io.sidebandFSMIO.rxData.enqueueNow(
      new MBInitFSMTest().formParamsReqMsg(false, linkTrainingParams),
    )
    c.io.sidebandFSMIO.packetTxData.expectDequeueNow(
      new MBInitFSMTest().formParamsReqMsg(false, linkTrainingParams),
    )
    c.clock.step(3)
  }

  private def initSB(c: LinkTrainingFSM): Unit = {
    c.io.currentState.expect(LinkTrainingState.sbInit)
    val txData = Seq.fill(3)("h_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa".U)
    val rxData = Seq.fill(1)("h_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa_aaaa".U)
    fork {
      c.io.sidebandFSMIO.rxData.enqueueSeq(rxData)
    }.fork {
      c.io.sidebandFSMIO.patternTxData.expectDequeueSeq(txData)
    }.join()

    c.clock.step(5)
    println("Exchanged clock patterns")
    c.io.sidebandFSMIO.rxData.enqueueNow(
      SBMessage_factory(SBM.SBINIT_OUT_OF_RESET, "PHY", true, "PHY", 0, 0).U,
    )
    c.io.sidebandFSMIO.packetTxData
      .expectDequeueNow(
        SBMessage_factory(SBM.SBINIT_OUT_OF_RESET, "PHY", true, "PHY", 0, 0).U,
      )
    c.io.sidebandFSMIO.rxData.enqueue(
      SBMessage_factory(SBM.SBINIT_OUT_OF_RESET, "PHY", true, "PHY", 0, 0).U,
    )
    c.io.sidebandFSMIO.rxData.enqueue(
      SBMessage_factory(SBM.SBINIT_OUT_OF_RESET, "PHY", true, "PHY", 0, 0).U,
    )
    println("Exchanged out of reset sb packets")
    c.clock.step(2)
    c.io.sidebandFSMIO.rxData
      .enqueueNow(
        SBMessage_factory(SBM.SBINIT_DONE_REQ, "PHY", true, "PHY", 0, 0).U,
      )
    c.io.sidebandFSMIO.packetTxData
      .expectDequeueNow(
        SBMessage_factory(SBM.SBINIT_DONE_REQ, "PHY", true, "PHY", 0, 0).U,
      )
    c.clock.step(3)
    c.io.sidebandFSMIO.rxData
      .enqueueNow(
        SBMessage_factory(SBM.SBINIT_DONE_RESP, "PHY", true, "PHY", 0, 0).U,
      )
    c.io.sidebandFSMIO.packetTxData
      .expectDequeueNow(
        SBMessage_factory(
          SBM.SBINIT_DONE_RESP,
          "PHY",
          true,
          "PHY",
          data = 0,
          msgInfo = 0,
        ).U,
      )
    c.clock.step(3)
  }

  private def trainMB(c: LinkTrainingFSM): Unit = {
    c.io.currentState.expect(LinkTrainingState.mbTrain)

  }

  private def testTransitionOutOfReset(c: LinkTrainingFSM): Unit = {
    c.io.currentState.expect(LinkTrainingState.reset)
    c.io.mainbandFSMIO.txFreqSel.expect(SpeedMode.speed4)
    c.clock.step()
    c.io.mainbandFSMIO.pllLock.poke(true)
    c.io.sidebandFSMIO.pllLock.poke(true)

    for (_ <- 0 until 30) {
      c.io.currentState.expect(LinkTrainingState.reset)
      c.io.mainbandFSMIO.txFreqSel.expect(SpeedMode.speed4)
      c.clock.step()
      c.io.mainbandFSMIO.pllLock.poke(false)
      c.io.sidebandFSMIO.pllLock.poke(false)
    }

    c.io.mainbandFSMIO.pllLock.poke(true)
    c.io.sidebandFSMIO.pllLock.poke(true)
    c.clock.step()
  }

  private def initPorts(c: LinkTrainingFSM) = {
    c.io.sidebandFSMIO.rxData.initSource().setSourceClock(c.clock)
    c.io.sidebandFSMIO.patternTxData.initSink().setSinkClock(c.clock)
    c.io.sidebandFSMIO.packetTxData.initSink().setSinkClock(c.clock)
  }
}
