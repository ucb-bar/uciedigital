package edu.berkeley.cs.uciedigital.logphy

import interfaces._
import sideband._
import chisel3._
import chisel3.util._

/*
    TODO:
        -- Add ability to parse in SBInit for pattern generation
            -- Need to be able to read different patterns on different lanes
                -- based on which state set signals for reader and writer to output to correct lane
                    -- default write pattern can be in the MainbandTransmitter (Lanes Module)

        -- when in LTSM is in RESET the pattern reader needs to be parsing
            for SBINIT pattern 
*/
class SBInitSM(linkTrainingParams: LinkTrainingParams,
               sbParams: SidebandParams,
               afeParams: AfeParams,
               maxPatternCount: Int) extends Module {
    val io = new Bundle {
        val fsmCtrl = new SubFsmControlIO()
        val sidebandCtrl = new SidebandCtrlIO()
        val mainbandCtrl = new MainbandCtrlIO()     
        val sbMsgSource = Output(MsgSource())
        val sbMsgIo = Flipped(new SBMsgWrapperTrainIO())
        val patternGenIo = Flipped(new PatternGeneratorIO(afeParams, 
                                                          maxPatternCount))
    }


    object SBInitState extends ChiselEnum {
        val sSEND_PATTERN, 
    }
    import SBInitState._
    

    val current_state = RegInit(sIDLE)
    val next_state = Wire(SBInitState())

    current_state := next_state

    /*
        - once start == 1 start send_pattern state            
            - once detected transition to detected_pattern state   
            - if not detected continue to send pattern for 8ms (pattern 1ms, low 1ms)


        - if the trigger was from pattern generator then it was remote that initiated the training
        so we need skip to out of reset state

    */

    // state action/output logic
    switch(current_state) {
        is(sIDLE) {

        }
        is(sSEND_PATTERN) {

        }
    }

    // state transition logic
    switch(current_state) {

    }

}

class SBInitRequester extends Module {


}


class SBInitResponder extends Module {
  
}
