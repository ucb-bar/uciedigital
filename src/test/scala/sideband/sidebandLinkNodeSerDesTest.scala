package edu.berkeley.cs.ucie.digital
package sideband

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import interfaces._

class LinkSerDesTest extends AnyFlatSpec with ChiselScalatestTester {
  val fdiParams = new FdiParams(width = 8, dllpWidth = 8, sbWidth = 32)
  val simple_fdiParams = new FdiParams(width = 8, dllpWidth = 8, sbWidth = 1)
  behavior of "linkserdes"
  it should "instantiate ser test" in {
    test(new SidebandLinkSerializer(new SidebandParams(), fdiParams)) { c =>
      c.clock.step()
      println("welp")
    }
  }

  it should "instantiate des test" in {
    test(new SidebandLinkDeserializer(new SidebandParams(), fdiParams)) { c =>
      c.clock.step()
      println("welp")
    }
  }

  it should "instantiate sidebandlinknode" in {
    test(new SidebandLinkNode(new SidebandParams(), fdiParams)) { c =>
      c.clock.step()
      println("welp")
    }
  }

  it should "simple ser" in {
    test(new SidebandSerializer(new SidebandParams(), simple_fdiParams)) { c =>
      // prepare random data generator
      println("Test started")
      val seed: Int = 0
      val rand = new scala.util.Random(seed)

      // init
      c.io.in.valid.poke(false.B)
      c.io.out.credit.poke(false.B)
      c.clock.step()

      // Send data to serializer
      println("Send data")
      val data = BigInt(c.msg_w, rand).U
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke(data)
      c.io.out.valid.expect(false.B)
      c.clock.step()

      // Check serialized data
      c.io.in.valid.poke(false.B)
      for (i <- 0 until (c.msg_w / c.sb_w)) {
        val serialized_data = data((i + 1) * c.sb_w - 1, i * c.sb_w)
        c.io.in.ready.expect(false.B)
        c.io.out.valid.expect(true.B)
        c.io.out.bits.expect(serialized_data)
        c.clock.step()
      }

      // make sure nothing is there
      c.io.in.ready.expect(true.B)
      c.io.out.valid.expect(false.B)
    }
  }

  it should "simple link ser" in {
    test(new SidebandLinkSerializer(new SidebandParams(), simple_fdiParams)) {
      c =>
        // init
        println("Test started")
        val seed: Int = 0
        val rand = new scala.util.Random(seed)

        // init
        c.io.in.valid.poke(false.B)
        c.clock.step()

        // Send data to serializer
        println("Send data")
        val data = BigInt(c.msg_w, rand).U
        c.io.in.valid.poke(true.B)
        c.io.in.bits.poke(data)
        c.clock.step()

        // Check serialized data
        c.io.in.valid.poke(false.B)
        c.clock.step()
        for (i <- 0 until (c.msg_w / c.sb_w)) {
          val serialized_data = data((i + 1) * c.sb_w - 1, i * c.sb_w)
          c.io.in.ready.expect(false.B)
          c.io.out.bits.expect(serialized_data)
          c.clock.step()
        }

        // make sure it does not take anything for 32 cycles
        for (i <- 0 until 32) {
          println(i)
          c.io.in.ready.expect(false.B)
          c.clock.step()
        }

        // make sure it takes something
        c.io.in.ready.expect(true.B)
    }
  }

  it should "simple linknodepair" in {
    test(new linkNodePair(new SidebandParams(), simple_fdiParams)) { c =>
      // init
      c.io.A.layer_to_node.valid.poke(false.B)
      c.io.A.node_to_layer.ready.poke(false.B)
      c.io.B.layer_to_node.valid.poke(false.B)
      c.io.B.node_to_layer.ready.poke(false.B)
      // reset
      c.reset.poke(true.B)
      c.clock.step()

      c.reset.poke(false.B)
      c.io.B.node_to_layer.valid.expect(false.B)

      // send something from A to B
      val data = 3.U(128.W)
      c.io.A.layer_to_node.valid.poke(true.B)
      c.io.A.layer_to_node.bits.poke(data)

      // wait until B receives it
      println("entering loop")
      while (c.io.B.node_to_layer.valid.peek().litToBoolean == false) {
        println("waiting")
        c.clock.step()
      }

      c.io.B.node_to_layer.valid.expect(true.B)
      c.io.B.node_to_layer.bits.expect(data)
      c.io.B.node_to_layer.ready.poke(true.B)

      c.clock.step()
      // send another thing from A to B
      val data2 = 2.U(128.W)
      c.io.A.layer_to_node.valid.poke(true.B)
      c.io.A.layer_to_node.bits.poke(data2)
      println("entering loop")
      while (c.io.B.node_to_layer.valid.peek().litToBoolean == false) {
        println("waiting")
        c.clock.step()
      }
      c.io.B.node_to_layer.valid.expect(true.B)
      c.io.B.node_to_layer.bits.expect(data2)
    }
  }

  class linkNodePair(sbParams: SidebandParams, fdiParams: FdiParams)
      extends Module {
    val io = IO(new Bundle {
      val A = new Bundle {
        val layer_to_node = Flipped(Decoupled(UInt(sbParams.sbNodeMsgWidth.W)))
        val node_to_layer = Decoupled(UInt(sbParams.sbNodeMsgWidth.W))
      }
      val B = new Bundle {
        val layer_to_node = Flipped(Decoupled(UInt(sbParams.sbNodeMsgWidth.W)))
        val node_to_layer = Decoupled(UInt(sbParams.sbNodeMsgWidth.W))
      }
    })
    val nodeA = Module(new SidebandLinkNode(sbParams, fdiParams))
    val nodeB = Module(new SidebandLinkNode(sbParams, fdiParams))
    nodeA.io.inner <> io.A
    nodeA.io.rxMode := RXTXMode.PACKET
    nodeB.io.inner <> io.B
    nodeA.io.outer.tx <> nodeB.io.outer.rx
    nodeB.io.outer.tx <> nodeA.io.outer.rx
    nodeB.io.rxMode := RXTXMode.PACKET
  }

}
