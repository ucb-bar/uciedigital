package edu.berkeley.cs.ucie.digital
package sideband

import chisel3._

import interfaces._

class D2DSidebandChannel(
    val myID: BigInt = BigInt(1),
    val sbParams: SidebandParams,
    val fdiParams: FdiParams,
) extends Module {
  val io = IO(new D2DSidebandChannelIO(sbParams, fdiParams))

  // Instantiate submodule
  val upper_node = Module(new SidebandNode(sbParams, fdiParams))
  val switcher = Module(new sidebandSwitcher(myID, sbParams))
  val lower_node = Module(new SidebandNode(sbParams, fdiParams))

  // Connect outer signals
  io.to_upper_layer <> upper_node.io.outer
  io.to_lower_layer <> lower_node.io.outer

  // Connect two sidebandNodes and switcher
  upper_node.io.inner.layer_to_node <> switcher.io.outer.layer_to_node_above
  upper_node.io.inner.node_to_layer <> switcher.io.outer.node_to_layer_above
  lower_node.io.inner.layer_to_node <> switcher.io.outer.layer_to_node_below
  lower_node.io.inner.node_to_layer <> switcher.io.outer.node_to_layer_below

  // Connect inner signals
  io.inner <> switcher.io.inner
}

class PHYSidebandChannel(
    val myID: BigInt = BigInt(2),
    val sbParams: SidebandParams,
    val fdiParams: FdiParams,
) extends Module {
  val io = IO(new PHYSidebandChannelIO(sbParams, fdiParams))

  // Instantiate submodule
  val upper_node = Module(new SidebandNode(sbParams, fdiParams))
  val switcher = Module(new sidebandSwitcher(myID, sbParams))
  val lower_node = Module(new SidebandLinkNode(sbParams, fdiParams))

  // Connect outer signals
  io.to_upper_layer <> upper_node.io.outer
  io.to_lower_layer <> lower_node.io.outer

  // Connect two sidebandNodes and switcher
  upper_node.io.inner.layer_to_node <> switcher.io.outer.layer_to_node_above
  upper_node.io.inner.node_to_layer <> switcher.io.outer.node_to_layer_above
  when(io.inner.inputMode === RXTXMode.PACKET) {
    io.inner.rawInput.nodeq()
    lower_node.io.inner.layer_to_node <> switcher.io.outer.layer_to_node_below
  }.otherwise {
    switcher.io.outer.layer_to_node_below.nodeq()
    lower_node.io.inner.layer_to_node <> io.inner.rawInput
  }

  when(io.inner.rxMode === RXTXMode.RAW) {
    switcher.io.outer.node_to_layer_below.noenq()
    switcher.io.inner.node_to_layer_below.nodeq()

    lower_node.io.inner.node_to_layer <> io.inner.switcherBundle.node_to_layer_below
    switcher.io.inner.layer_to_node_below <> io.inner.switcherBundle.layer_to_node_below
    switcher.io.inner.node_to_layer_above <> io.inner.switcherBundle.node_to_layer_above
    switcher.io.inner.layer_to_node_above <> io.inner.switcherBundle.layer_to_node_above

  }.otherwise {
    lower_node.io.inner.node_to_layer <> switcher.io.outer.node_to_layer_below
    io.inner.switcherBundle <> switcher.io.inner
  }
  lower_node.io.rxMode := io.inner.rxMode

  // Connect inner signals
  // io.inner.switcherBundle <> switcher.io.inner
}
