package edu.berkeley.cs.ucie.digital
package logphy

import chisel3._
import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor
import chiseltest._
import freechips.rocketchip.util.AsyncQueueParams
import org.scalatest.flatspec.AnyFlatSpec
import interfaces._

import scala.util.Random

class LogPhyLaneTest extends AnyFlatSpec with ChiselScalatestTester {
  val afeParams = AfeParams()
  val afeParams32 = AfeParams(mbSerializerRatio = 32)
  val queueParams = new AsyncQueueParams()
  behavior of "log phy TX lanes no scramble"
  it should "correctly map TX bytes to their lanes" in {
    test(new SimLanes(afeParams, queueParams)) { c =>
      initPorts(c, false)

      c.io.mainbandIO.txData.enqueueNow(
        "h1234_5678_9abc_def0_0fed_cba9_8765_4321_1111_2222_3333_4444_5555_6666_7777_8888".U,
      )
      c.io.mainbandLaneIO.txData
        .expectDequeueNow(
          Vec.Lit(
            "h2188".U,
            "h4388".U,
            "h6577".U,
            "h8777".U,
            "ha966".U,
            "hcb66".U,
            "hed55".U,
            "h0f55".U,
            "hf044".U,
            "hde44".U,
            "hbc33".U,
            "h9a33".U,
            "h7822".U,
            "h5622".U,
            "h3411".U,
            "h1211".U,
          ),
        )
    }
  }

  behavior of "log phy RX lanes no scramble"
  it should "correctly map RX bytes to their lanes" in {
    test(new SimLanes(afeParams, queueParams)) { c =>
      initPorts(c, scramble = false)

      c.io.mainbandLaneIO.rxData
        .enqueueNow(
          Vec.Lit(
            "h1211".U,
            "h3411".U,
            "h5622".U,
            "h7822".U,
            "h9a33".U,
            "hbc33".U,
            "hde44".U,
            "hf044".U,
            "h0f55".U,
            "hed55".U,
            "hcb66".U,
            "ha966".U,
            "h8777".U,
            "h6577".U,
            "h4388".U,
            "h2188".U,
          ),
        )
      c.io.mainbandIO.rxData.expectDequeueNow(
        "h2143_6587_a9cb_ed0f_f0de_bc9a_7856_3412_8888_7777_6666_5555_4444_3333_2222_1111".U,
      )

    }
  }

  behavior of "log phy TX lanes scramble"
  it should "correctly map TX bytes to their lanes" in {
    test(new SimLanes(afeParams, queueParams)) { c =>
      initPorts(c, scramble = true)

      c.io.mainbandIO.txData.enqueueNow(
        "h1234_5678_9abc_def0_0fed_cba9_8765_4321_1111_2222_3333_4444_5555_6666_7777_8888".U,
      )

      c.clock.step()

      c.io.mainbandLaneIO.txData
        .expectDequeueNow(
          Vec.Lit(
            "b1001111000110100".U, // "h2188".U  ^ "1011_1111_1011_1100".U,
            "b100010000110011".U, // "h4388".U  ^ "b0000_0111_1011_1011".U,
            "b1010001000010111".U, // "h6577".U  ^ "b1100011101100000".U
            "b100011110101100".U, // "h8777".U   ^ "0b1100000011011011".U
            "b1010011001110100".U, // "ha966".U   ^ "0b0000111100010010".U
            "b10010101111".U, // "hcb66".U    ^ "0b1100111111001001".U
            "b1001101010011011".U, // "hed55".U   ^ "0b0111011111001110".U
            "b1011011101010010".U, // "h0f55".U    ^ "0b1011100000000111".U

            "b100111111111000".U, // "hf044".U  ^ "b1011_1111_1011_1100"
            "b1101100111111111".U, // "hde44".U ^ "b0000_0111_1011_1011"
            "b111101101010011".U, // "hbc33".U,   ^ "b1100011101100000".U
            "b101101011101000".U, // "h9a33".U ^ "0b1100000011011011".U
            "b111011100110000".U, // "h7822".U, ^ "0b0000111100010010".U
            "b1001100111101011".U, // "h5622".U,  ^ "0b1100111111001001".U
            "b100001111011111".U, // "h3411".U, ^ "0b0111011111001110".U
            "b1010101000010110".U, // "h1211".U, ^ "0b1011100000000111".U
          ),
        )

      println()
      println()
      println()

      c.io.mainbandIO.txData.enqueueNow(
        "h1234_5678_9abc_def0_0fed_cba9_8765_4321_1111_2222_3333_4444_5555_6666_7777_8888".U,
      )

      c.clock.step()

      c.io.mainbandLaneIO.txData
        .expectDequeueNow(
          Vec.Lit(
            "b100110010101".U, // "h2188".U   ^ "0b0010100000011101".U
            "b1110111000001110".U, // "h4388".U  ^ "0b1010110110000110".U
            "b1101101101101001".U, // "h6577".U   ^ "1011_1110_0001_1110".U
            "b1001010011101111".U, // "h8777".U   ^ "0001_0011_1001_1000".U
            "b1110100001100111".U, // "ha966".U   ^ "0100_0001_0000_0001".U
            "b1001100111111111".U, // "hcb66".U    ^ "0101001010011001".U
            "b11101001010111".U, // "hed55".U   ^ "1101011100000010".U
            "b1000101011001110".U, // "h0f55".U    ^ "1000010110011011".U

            "b1101100001011001".U, // "hf044".U  ^ "0b0010100000011101"
            "b111001111000010".U, // "hde44".U ^ "0b1010110110000110"
            "b1000101101".U, // "hbc33".U,   ^ "1011_1110_0001_1110".U
            "b1000100110101011".U, // "h9a33".U ^ "0001_0011_1001_1000".U
            "b11100100100011".U, // "h7822".U, ^ "0100_0001_0000_0001".U
            "b10010111011".U, // "h5622".U,  ^ "0101001010011001".U
            "b1110001100010011".U, // "h3411".U, ^ "1101011100000010".U
            "b1001011110001010".U, // "h1211".U, ^ "1000010110011011".U
          ),
        )

    }
  }

  it should "tx data matches rx data" in {
    test(new LanesLoopBack(afeParams, queueParams)) { c =>
      c.io.mainbandLaneIO.txData.initSource()
      c.io.mainbandLaneIO.txData.setSourceClock(c.clock)
      c.io.mainbandLaneIO.rxData.initSink()
      c.io.mainbandLaneIO.rxData.setSinkClock(c.clock)
      c.io.scramble.poke(true.B)

      val rand = new Random()
      for (i <- 0 until 20) {
        println("i: ", i)
        val dataEnqueued =
          BigInt(afeParams.mbLanes * afeParams.mbSerializerRatio, rand)
            .U((afeParams.mbLanes * afeParams.mbSerializerRatio).W)
        c.io.mainbandLaneIO.txData.enqueueNow(
          dataEnqueued,
        )
        c.clock.step()
        c.io.mainbandLaneIO.rxData.expectDequeueNow(
          dataEnqueued,
        )
        c.clock.step()

      }

    }
  }

  behavior of "log phy with serializer ratio 32"
  it should "correctly map TX bytes to their lanes" in {
    test(new SimLanes(afeParams32, queueParams)) { c =>
      initPorts(c, false)

      c.io.mainbandIO.txData.enqueueNow(
        ("h1234_5678_9abc_def0_0fed_cba9_8765_4321" +
          "1111_2222_3333_4444_5555_6666_7777_8888" +
          "1212_2323_3434_4545_5656_6767_7878_8989" +
          "aaaa_bbbb_cccc_dddd_eeee_ffff_0000_1111").U,
      )
      c.io.mainbandLaneIO.txData
        .expectDequeueNow(
          Vec.Lit(
            "h21888911".U,
            "h43888911".U,
            "h65777800".U,
            "h87777800".U,
            "ha96667ff".U,
            "hcb6667ff".U,
            "hed5556ee".U,
            "h0f5556ee".U,
            "hf04445dd".U,
            "hde4445dd".U,
            "hbc3334cc".U,
            "h9a3334cc".U,
            "h782223bb".U,
            "h562223bb".U,
            "h341112aa".U,
            "h121112aa".U,
          ),
        )
    }
  }
  it should "tx data matches rx data" in {
    test(new LanesLoopBack(afeParams32, queueParams)) { c =>
      c.io.mainbandLaneIO.txData.initSource()
      c.io.mainbandLaneIO.txData.setSourceClock(c.clock)
      c.io.mainbandLaneIO.rxData.initSink()
      c.io.mainbandLaneIO.rxData.setSinkClock(c.clock)
      c.io.scramble.poke(true.B)

      val rand = new Random()
      for (i <- 0 until 20) {
        println("i: ", i)
        val dataEnqueued =
          BigInt(afeParams32.mbLanes * afeParams32.mbSerializerRatio, rand)
            .U((afeParams32.mbLanes * afeParams32.mbSerializerRatio).W)
        c.io.mainbandLaneIO.txData.enqueueNow(
          dataEnqueued,
        )
        c.clock.step()
        c.io.mainbandLaneIO.rxData.expectDequeueNow(
          dataEnqueued,
        )
        c.clock.step()

      }

    }
  }

  private def initPorts(c: SimLanes, scramble: Boolean): Unit = {
    c.io.mainbandIO.txData.initSource()
    c.io.mainbandIO.txData.setSourceClock(c.clock)
    c.io.mainbandIO.rxData.initSink()
    c.io.mainbandIO.rxData.setSinkClock(c.clock)
    c.io.mainbandLaneIO.txData.initSink()
    c.io.mainbandLaneIO.txData.setSinkClock(c.clock)
    c.io.mainbandLaneIO.rxData.initSource()
    c.io.mainbandLaneIO.rxData.setSourceClock(c.clock)
    c.io.scramble.poke(scramble.B)
  }

}

class LanesLoopBack(
    afeParams: AfeParams,
    queueParams: AsyncQueueParams,
) extends Module {
  val io = IO(new Bundle {
    val scramble = Input(Bool())
    val mainbandLaneIO = new MainbandIO(afeParams)
  })
  val lanes = Module(new SimLanes(afeParams, queueParams))
  lanes.io.scramble := io.scramble
  lanes.io.mainbandIO <> io.mainbandLaneIO
  lanes.io.mainbandLaneIO.txData <> lanes.io.mainbandLaneIO.rxData
  when(io.mainbandLaneIO.rxData.fire) {
    printf("rxDataBits: %x\n", io.mainbandLaneIO.rxData.bits)
  }
  when(io.mainbandLaneIO.txData.fire) {
    printf("txDataBits: %x\n", io.mainbandLaneIO.txData.bits)
  }
}
