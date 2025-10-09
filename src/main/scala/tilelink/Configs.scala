package edu.berkeley.cs.ucie.digital
package tilelink


import chisel3._
import freechips.rocketchip.util._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.{Field, Config, Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import interfaces._
import protocol._
import sideband.{SidebandParams}
import logphy.{LinkTrainingParams}

case class UCITLParams(
    val protoParams: ProtocolLayerParams,
    val tlParams: TileLinkParams,
    val fdiParams: FdiParams,
    val rdiParams: RdiParams,
    val sbParams: SidebandParams,
    val linkTrainingParams: LinkTrainingParams,
    val afeParams: AfeParams,
    val laneAsyncQueueParams: AsyncQueueParams,
    val onchipAddr: Option[BigInt] = None,
)

case class InwardAddressTranslator(blockRange : AddressSet, replicationBase : Option[BigInt] = None)(implicit p: Parameters) extends LazyModule {
  val module_side = replicationBase.map { base =>
    val baseRegion   = AddressSet(0, base-1)
    val replicator   = LazyModule(new RegionReplicator(ReplicatedRegion(baseRegion, baseRegion.widen(base))))
    val prefixSource = BundleBridgeSource[UInt](() => UInt(1.W))
    replicator.prefix := prefixSource
    InModuleBody { prefixSource.bundle := 0.U(1.W) } // prefix is unused for TL uncached, so this is ok
    replicator.node
  }.getOrElse { TLTempNode() }

  // val bus_side = TLFilter(TLFilter.mSelectIntersect(blockRange))(p)
  val bus_side = TLFilter(TLFilter.mSubtract(blockRange))(p)

  // module_side := bus_side

  def apply(node : TLNode) : TLNode = {
    node := module_side := bus_side
  }

  lazy val module = new LazyModuleImp(this) {}
}

case object UCITLKey extends Field[Option[UCITLParams]](None)

trait CanHaveTLUCIAdapter { this: BaseSubsystem =>
  val uciTL = p(UCITLKey) match {
    case Some(params) => {
      val sbus = locateTLBusWrapper(SBUS)
      val uciTL = LazyModule(
        new UCITLFront(
          tlParams = params.tlParams,
          protoParams = params.protoParams,
          fdiParams = params.fdiParams,
          rdiParams = params.rdiParams,
          sbParams = params.sbParams,
          // myId        = params.myId,
          linkTrainingParams = params.linkTrainingParams,
          afeParams = params.afeParams,
          laneAsyncQueueParams = params.laneAsyncQueueParams,
        ),
      )

      uciTL.clockNode := sbus.fixedClockNode
      val manager_addr = AddressSet(params.tlParams.ADDRESS, params.tlParams.ADDR_RANGE)

      val translator = uciTL {
        LazyModule(InwardAddressTranslator(manager_addr, params.onchipAddr)(p))
      }

      sbus.coupleTo(s"ucie_tl_man_port") {
          translator(uciTL.managerNode) := TLWidthWidget(sbus.beatBytes) := TLBuffer() := TLSourceShrinker(params.tlParams.sourceIDWidth) := TLFragmenter(sbus.beatBytes, p(CacheBlockBytes)) := TLBuffer() := _
      }
      sbus.coupleFrom(s"ucie_tl_cl_port") { _ := TLBuffer() := TLWidthWidget(sbus.beatBytes) := TLBuffer() := uciTL.clientNode }
      sbus.coupleTo(s"ucie_tl_ctrl_port") { uciTL.regNode.node := TLWidthWidget(sbus.beatBytes) := TLFragmenter(sbus.beatBytes, sbus.blockBytes) := TLBuffer() := _ }
      Some(uciTL)
    }
    case None => None
  }
  val ucie_io = uciTL.map { uci =>
    InModuleBody {
      val ucie_io = IO(new UcieDigitalTopIO())
      ucie_io <> uci.module.io
      ucie_io
    }
  }
}

class WithUCITLAdapter(params: UCITLParams)
    extends Config((site, here, up) => { case UCITLKey =>
      Some(params)
    })
