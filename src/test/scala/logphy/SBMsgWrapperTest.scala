package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import sideband._

import scala.util.Random

class SBMsgWrapperTest extends AnyFlatSpec with ChiselScalatestTester {
  val sbParams = SidebandParams()
  behavior of "sb message wrapper"
  it should "correctly exchange SB out of reset message" in {
    test(new SBMsgWrapper(sbParams)) { c =>
      initPorts(c)
      testSBInitOutOfReset(c)
    }
  }

  it should "correctly exchange SB out of reset message twice" in {
    test(new SBMsgWrapper(sbParams)) { c =>
      initPorts(c)
      testSBInitOutOfReset(c)
      testSBInitOutOfReset(c)
    }
  }

  it should "correctly complete parameter exchange" in {
    test(new SBMsgWrapper(sbParams)) { c =>
      /** TODO */
    }
  }

  it should "correctly receive only" in {
    test(new SBMsgWrapper(sbParams)) { c =>
      initPorts(c)
      testSBInitReceiveOnly(c)
      testSBInitReceiveOnly(c)
    }

  }
  private def testSBInitReceiveOnly(c: SBMsgWrapper): Unit = {
    c.io.laneIO.rxData.ready.expect(false)
    c.io.laneIO.txData.expectInvalid()
    c.io.trainIO.msgReq.ready.expect(true)
    c.io.trainIO.msgReqStatus.expectInvalid()

    c.clock.step()
    val rand = new Random()
    val data = BigInt(64, rand)
    val sbMsg = SBMessage_factory(
      SBM.SBINIT_OUT_OF_RESET,
      "PHY",
      true,
      "PHY",
      data = data,
      msgInfo = 0,
    )
    c.io.trainIO.msgReq.enqueueNow(
      (new MessageRequest).Lit(
        _.msg -> sbMsg.U,
        _.reqType -> MessageRequestType.RECEIVE,
        _.timeoutCycles -> 80.U,
      ),
    )

    c.io.trainIO.msgReq.ready.expect(false)
    for (_ <- 0 until 4) {
      c.clock.step()
      c.io.laneIO.txData.expectInvalid()
    }
    c.io.laneIO.rxData.enqueueNow(sbMsg.U)
    for (_ <- 0 until 4) {
      c.clock.step()
      c.io.trainIO.msgReqStatus.valid.expect(true.B)
      c.io.laneIO.txData.expectInvalid()
    }
    c.io.trainIO.msgReqStatus.expectDequeue(
      (new MessageRequestStatus).Lit(
        _.status ->
          MessageRequestStatusType.SUCCESS,
        _.data -> data.U,
      ),
    )
  }
  private def testSBInitOutOfReset(c: SBMsgWrapper): Unit = {
    c.io.laneIO.rxData.ready.expect(false)
    c.io.laneIO.txData.expectInvalid()
    c.io.trainIO.msgReq.ready.expect(true)
    c.io.trainIO.msgReqStatus.expectInvalid()

    c.clock.step()
    val sbMsg = SBMessage_factory(
      SBM.SBINIT_OUT_OF_RESET,
      "PHY",
      true,
      "PHY",
      data = 0,
      msgInfo = 0,
    )
    c.io.trainIO.msgReq.enqueueNow(
      (new MessageRequest).Lit(
        _.msg -> sbMsg.U,
        _.reqType -> MessageRequestType.EXCHANGE,
        _.timeoutCycles -> 80.U,
      ),
    )

    c.io.trainIO.msgReq.ready.expect(false)
    for (i <- 0 until 4) {
      c.io.laneIO.txData.expectDequeue(sbMsg.U)
    }
    c.io.laneIO.rxData.enqueueNow(sbMsg.U)
    c.io.trainIO.msgReqStatus.expectDequeue(
      (new MessageRequestStatus).Lit(
        _.status ->
          MessageRequestStatusType.SUCCESS,
        _.data -> 0.U,
      ),
    )
  }

  private def initPorts(c: SBMsgWrapper): Unit = {
    c.io.trainIO.msgReq.initSource()
    c.io.trainIO.msgReq.setSourceClock(c.clock)
    c.io.trainIO.msgReqStatus.initSink()
    c.io.trainIO.msgReqStatus.setSinkClock(c.clock)
    c.io.laneIO.txData.initSink()
    c.io.laneIO.txData.setSinkClock(c.clock)
    c.io.laneIO.rxData.initSource()
    c.io.laneIO.rxData.setSourceClock(c.clock)
  }

}
