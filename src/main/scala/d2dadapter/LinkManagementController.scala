package edu.berkeley.cs.ucie.digital
package d2dadapter

import chisel3._
import chisel3.util._
import interfaces._
import sideband._

class LinkManagementControllerIO (params: D2DAdapterParams) extends Bundle {
    val fdi = Flipped(new Fdi(params))
    val rdi = new Rdi(params)
    // TODO: We need to define a common packet format for SB internal messages
    val sb_snd = Decoupled(UInt(16.W))
    val sb_rcv = Flipped(Valid(UInt(16.W)))
}

/**
  * LinkManagementController for top level FDI/RDI state machine implementation, 
  * decoding the sideband messages and arbitration of triggers for the D2D adapter
  * state machine
  * @param params D2DAdapterParams
  */
class LinkManagementController (params: D2DAdapterParams) extends Module {
    val io = IO(new LinkManagementControllerIO(params))

    // Submodule instantiations
    val disabled_submodule = Module(new LinkDisabledSubmodule(params))

    // Output registers
    // LinkError signal propagated to the PHY
    val rdi_lp_linkerror_reg = RegInit(PhyStateReq.nop)
    val rdi_lp_state_req_reg = RegInit(PhyStateReq.nop)

    val fdi_pl_rxactive_req_reg = RegInit(false.B)

    // Internal registers
    val fdi_lp_state_req_prev_reg = RegNext(io.fdi.lpStateReq)

    // FDI/RDI state register
    val link_state_reg = RegInit(PhyState.reset)

    // Top level IO signal assignments
    // RDI
    io.rdi.lpLinkError := rdi_lp_linkerror_reg
    io.rdi.lpStateReq := rdi_lp_state_req_reg
    // FDI
    io.fdi.plStateStatus := link_state_reg
    io.fdi.plRxActiveReq := fdi_pl_rxactive_req_reg

    // LinkError propagation from Protocol layer to PHY
    rdi_lp_linkerror_reg := io.fdi.lpLinkError

    // Submodule IO signal assignments
    // Disabled submodule
    disabled_submodule.io.fdi_lp_state_req := io.fdi.lpStateReq
    disabled_submodule.io.fdi_lp_state_req_prev := fdi_lp_state_req_prev_reg
    disabled_submodule.io.rdi_pl_state_sts := io.rdi.plStateStatus
    disabled_submodule.io.link_state := link_state_reg
    val disabled_entry := disabled_submodule.io.disabled_entry
    // Intermediate sideband messgaes which gets assigned to top IO when required
    val disabled_sb_snd := disabled_submodule.io.disabled_sb_snd
    disabled_submodule.io.sb_rcv := io.sb_rcv

    // FDI/RDI common state change triggers
    // LinkError logic
    // PHY informs the adapter over RDI that it is in linkError state
    val linkerror_phy_sts = io.rdi.plStateStatus === PhyState.linkError
    // Protocol initiates linkError through lp_linkerror assertion
    val linkerror_fdi_req = io.fdi.lpLinkError
    // Placeholder for any other internal request logic which can trigger linkError

    // rx_deactive and rx_active signals for checking if rx on mainband is disabled
    val rx_deactive := ~io.fdi.lpRxActiveStatus & ~io.fdi.plRxActiveReq
    val rx_active := io.fdi.lpRxActiveStatus & io.fdi.plRxActiveReq

    // Moved this condition to the disabled module
    // Reset to disabled requires atleast one clock cycle of lp_state_req = Reset(NOP)
    //val disabled_from_reset_fdi_req = ((fdi_lp_state_req_prev_reg === PhyStateReq.reset &&
    //                                    io.fdi.lpStateReq === PhyStateReq.reset) &&
    //                                    io.rdi.plStateStatus === PhyState.reset)

    // TODO: Sideband message generation logic

    // TODO: RDI lp state request generation logic
    when(link_state_reg === PhyState.reset) {

    }.elsewhen(link_state_reg === PhyState.active) {

    }.elsewhen(link_state_reg === PhyState.retrain) {
        
    }.elsewhen(link_state_reg === PhyState.linkError) {
        // Section 8.3.4.2 for link error exit
        when(io.fdi.lpStateReq === PhyStateReq.active && !linkerror_fdi_req &&
                (io.rdi.plStateStatus === PhyState.linkError)) {
            rdi_lp_state_req_reg := PhyStateReq.active
        }.otherwise {
            rdi_lp_state_req_reg := PhyStateReq.nop
        }
    }.elsewhen(link_state_reg === PhyState.disabled) {
        when(io.fdi.lpStateReq === PhyStateReq.active) {
            rdi_lp_state_req_reg := PhyStateReq.active
        }.otherwise{
            rdi_lp_state_req_reg := PhyStateReq.disabled
        }
    }.elsewhen(link_state_reg === PhyState.linkReset) {
        
    }

    // FDI/RDI state machine. We use the same SM for optimized code as the spec
    // seems to trigger the state machines in tandem with no intermediate signalling 
    switch(link_state_reg) {
        // RESET
        is(PhyState.reset){
            when(linkerror_phy_sts || linkerror_fdi_req) {
                // TODO: any internal condition to trigger linkError? + SB msgs
                link_state_reg := PhyState.linkError
            }.elsewhen(disabled_entry && rx_deactive) {
                link_state_reg := PhyState.disabled
            }.elsewhen() {
                link_state_reg := PhyState.linkReset
            }.elsewhen() {
                link_state_reg := PhyState.active
            }.otherwise {
                link_state_reg := link_state_reg
            }
        }
        // ACTIVE
        is(PhyState.active) {
            when(linkerror_phy_sts || linkerror_fdi_req) {
                link_state_reg := PhyState.linkError
            }.elsewhen(disabled_entry && rx_deactive) {
                link_state_reg := PhyState.disabled
            }.otherwise {
                link_state_reg := link_state_reg
            }
        }
        // RETRAIN
        is(PhyState.retrain) {
            when(linkerror_phy_sts || linkerror_fdi_req) {
                link_state_reg := PhyState.linkError
            }.elsewhen(disabled_entry) {
                link_state_reg := PhyState.disabled
            }.otherwise {
                link_state_reg := link_state_reg
            }
        }
        // LINKERROR
        is(PhyState.linkError) {
            // TODO: Check this logic, also needs state change on internal reset request
            // 
            when(io.fdi.lpStateReq === PhyStateReq.active && !linkerror_fdi_req &&
                (io.rdi.plStateStatus === PhyState.linkError) && rx_deactive) {
                    link_state_reg := PhyState.reset
                }.otherwise {
                    link_state_reg := link_state_reg
                }
        }
        // DISABLED
        is(PhyState.disabled) {
            when(linkerror_phy_sts || linkerror_fdi_req) {
                link_state_reg := PhyState.linkError
            }.elsewhen(io.fdi.lpStateReq === PhyStateReq.active || 
                        io.rdi.plStateStatus === PhyState.reset) {
                link_state_reg := PhyState.reset
            }.otherwise {
                link_state_reg := link_state_reg
            }
        }
        // LINKRESET
        is(PhyState.linkReset) {
            when(linkerror_phy_sts || linkerror_fdi_req) {
                link_state_reg := PhyState.linkError
            }.elsewhen(disabled_entry && rx_deactive) {
                link_state_reg := PhyState.disabled
            }.otherwise {
                link_state_reg := link_state_reg
            }
        }
    }
}