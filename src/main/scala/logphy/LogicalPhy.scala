package edu.berkeley.cs.ucie.digital
package logphy

import interfaces._
import sideband._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor
import freechips.rocketchip.util.AsyncQueueParams

class LogicalPhy(
    linkTrainingParams: LinkTrainingParams,
    afeParams: AfeParams,
    rdiParams: RdiParams,
    fdiParams: FdiParams,
    sbParams: SidebandParams,
    laneAsyncQueueParams: AsyncQueueParams,
) extends Module {

  val io = IO(new Bundle {
    val rdi = Flipped(new Rdi(rdiParams))
    val mbAfe =
      if (afeParams.STANDALONE) Some(new MainbandAfeIo(afeParams)) else None
    val phyAfe =
      if (afeParams.STANDALONE) None
      else Some(new MainbandLaneIOWithValid(afeParams))
    val sbAfe = new SidebandAfeIo(afeParams)
    val train =
      if (afeParams.STANDALONE) None
      else
        Some(
          new TrainingOperation(afeParams, linkTrainingParams.maxPatternCount),
        )
  })

  val trainingModule = {
    Module(
      new LinkTrainingFSM(linkTrainingParams, sbParams, afeParams),
    )
  }

  /** TODO: replace this with MMIO module instantiations */
  if (afeParams.STANDALONE) {
    trainingModule.io.trainingOperationIO := DontCare
  } else {
    trainingModule.io.trainingOperationIO <> io.train.get
  }

  if (afeParams.STANDALONE) {
    trainingModule.io.mainbandFSMIO.rxEn <> io.mbAfe.get.rxEn
  }
  trainingModule.io.sidebandFSMIO.rxEn <> io.sbAfe.rxEn
  trainingModule.io.rdi.rdiBringupIO.lpStateReq <> io.rdi.lpStateReq

  /** TODO: this is wrong for plError, plError is abut framing error -- when
    * would that occur?
    */
  io.rdi.plError := trainingModule.io.currentState === LinkTrainingState.linkError
  io.rdi.plTrainError := trainingModule.io.currentState === LinkTrainingState.linkError

  /** TODO: not sure when would encounter a non fatal or correctable error, or
    * what that means
    */
  io.rdi.plNonFatalError := false.B
  io.rdi.plCorrectableError := false.B

  /** not a retimer */
  io.rdi.plRetimerCrd := false.B

  io.rdi.plPhyInRecenter := io.rdi.plStateStatus === PhyState.retrain
  io.rdi.plSpeedMode <> trainingModule.io.mainbandFSMIO.txFreqSel
  if (afeParams.STANDALONE) {
    io.mbAfe.get.txFreqSel <> trainingModule.io.mainbandFSMIO.txFreqSel
  }
  io.rdi.plLinkWidth := PhyWidth.width16
  io.rdi.plClkReq <> trainingModule.io.rdi.rdiBringupIO.plClkReq
  io.rdi.plWakeAck <> trainingModule.io.rdi.rdiBringupIO.plWakeAck
  io.rdi.lpClkAck <> trainingModule.io.rdi.rdiBringupIO.lpClkAck
  io.rdi.lpWakeReq <> trainingModule.io.rdi.rdiBringupIO.lpWakeReq
  io.rdi.plStallReq <> trainingModule.io.rdi.rdiBringupIO.plStallReq
  io.rdi.lpStallAck <> trainingModule.io.rdi.rdiBringupIO.lpStallAck
  io.rdi.plStateStatus <> trainingModule.io.rdi.rdiBringupIO.plStateStatus
  io.rdi.lpLinkError <> trainingModule.io.rdi.rdiBringupIO.lpLinkError

  /** TODO: is this correct behavior, look at spec */
  io.rdi.plInbandPres := trainingModule.io.currentState === LinkTrainingState.linkInit || trainingModule.io.currentState === LinkTrainingState.active

  val rdiDataMapper = Module(new RdiDataMapper(rdiParams, afeParams))

  val lanes =
    if (afeParams.STANDALONE) {
      Module(new Lanes(afeParams, laneAsyncQueueParams))
    } else {
      Module(new LanesNoFifo(afeParams))
    }

  lanes.io.scramble := trainingModule.io.currentState === LinkTrainingState.active
  when(trainingModule.io.currentState === LinkTrainingState.active) {
    rdiDataMapper.io.mainbandIO <> lanes.io.mainbandIO
    trainingModule.io.mainbandFSMIO.mainbandIO.rxData.noenq()
    trainingModule.io.mainbandFSMIO.mainbandIO.txData.nodeq()
  }.otherwise {
    rdiDataMapper.io.mainbandIO.rxData.noenq()
    rdiDataMapper.io.mainbandIO.txData.nodeq()
    trainingModule.io.mainbandFSMIO.mainbandIO <> lanes.io.mainbandIO
  }

  /** Connect internal FIFO to AFE */
  if (afeParams.STANDALONE) {
    lanes.io.mainbandLaneIO.txData <> io.mbAfe.get.txData
    lanes.io.mainbandLaneIO.rxData <> io.mbAfe.get.rxData
    lanes.asInstanceOf[Lanes].asyncQueueIO <> io.mbAfe.get.fifoParams
  } else {

    io.phyAfe.get.tx <> lanes.io.mainbandLaneIO.txData.map(f => {
      val x = Wire(chiselTypeOf(io.phyAfe.get.tx.bits))
      x.data := f
      x.valid := Vec.Lit(Seq.fill(afeParams.mbSerializerRatio)(true.B): _*)
      x
    })
    io.phyAfe.get.rx.map(_.data) <> lanes.io.mainbandLaneIO.rxData
    io.phyAfe.get.rxRst := false.B
    io.phyAfe.get.txRst := false.B
  }

  /** Connect RDI to Mainband IO */
  rdiDataMapper.io.rdi.lpData <> io.rdi.lpData
  io.rdi.plData <> rdiDataMapper.io.rdi.plData

  private val sidebandChannel =
    Module(new PHYSidebandChannel(sbParams = sbParams, fdiParams = fdiParams))
  assert(
    afeParams.sbSerializerRatio == 1,
    "connecting sideband module directly to training module, sb serializer ratio must be 1!",
  )

  sidebandChannel.io.to_upper_layer.tx.bits <> io.rdi.plConfig.bits
  sidebandChannel.io.to_upper_layer.tx.valid <> io.rdi.plConfig.valid
  sidebandChannel.io.to_upper_layer.tx.credit <> io.rdi.plConfigCredit
  sidebandChannel.io.to_upper_layer.rx.bits <> io.rdi.lpConfig.bits
  sidebandChannel.io.to_upper_layer.rx.valid <> io.rdi.lpConfig.valid
  sidebandChannel.io.to_upper_layer.rx.credit <> io.rdi.lpConfigCredit

  /** Inner connections to lower layer */
  sidebandChannel.io.inner.switcherBundle.layer_to_node_below <> trainingModule.io.sidebandFSMIO.packetTxData
  sidebandChannel.io.inner.switcherBundle.node_to_layer_below <> trainingModule.io.sidebandFSMIO.rxData
  sidebandChannel.io.inner.rawInput <> trainingModule.io.sidebandFSMIO.patternTxData
  sidebandChannel.io.inner.inputMode := trainingModule.io.sidebandFSMIO.txMode
  sidebandChannel.io.inner.rxMode := trainingModule.io.sidebandFSMIO.rxMode

  /** Currently no situation where would receive SB packet from above layer. */
  sidebandChannel.io.inner.switcherBundle.layer_to_node_above.noenq()
  sidebandChannel.io.inner.switcherBundle.node_to_layer_above.nodeq()

  assert(
    afeParams.sbWidth == 1,
    "AFE SB width must match hardcoded value",
  )
  io.sbAfe.txData <> sidebandChannel.io.to_lower_layer.tx.bits
  io.sbAfe.txClock <> sidebandChannel.io.to_lower_layer.tx.clock
  io.sbAfe.rxData <> sidebandChannel.io.to_lower_layer.rx.bits
  io.sbAfe.rxClock <> sidebandChannel.io.to_lower_layer.rx.clock

}
