XRUN := xrun
XRUN_OPTS := -64bit \
             -timescale 1ps/1fs \
             -access +rwc \
             -sv_ms
VAMS_DIR := ../verilog
SPECTRE_HOME := /tools/cadence/SPECTRE/SPECTRE251
AMSHOME := /tools/cadence/XCELIUM/XCELIUM2409.006/tools.lnx86/affirma_ams
INCLUDES := -incdir $(VAMS_DIR) -incdir $(SPECTRE_HOME)/tools.lnx86/spectre/etc/ahdl
PHY_PROBE := probe.tcl

# Source files
DISCIPLINES := $(SPECTRE_HOME)/tools.lnx86/spectre/etc/ahdl/disciplines.vams
CONSTANTS := $(VAMS_DIR)/constants.vams
PRIMITIVES_SV := $(VAMS_DIR)/primitives.sv
PRIMITIVES_VAMS := $(VAMS_DIR)/primitives.vams
TX_SRC := $(VAMS_DIR)/tx.sv
TX_VAMS := $(VAMS_DIR)/tx.vams
RX_SRC := $(VAMS_DIR)/rx.sv
RX_VAMS := $(VAMS_DIR)/rx.vams
PHY_SRC := $(VAMS_DIR)/phy.sv


phy:
	@echo "=========================================="
	@echo "Running UCIePHY Testbench with xrun..."
	@echo "=========================================="
	$(XRUN) -compile -ams $(XRUN_OPTS) $(INCLUDES) \
		-xmlibdirname worklib \
		$(DISCIPLINES) \
		$(CONSTANTS) \
		$(PRIMITIVES_VAMS) \
		$(TX_VAMS) \
		$(RX_VAMS) \
		-reflib $(AMSHOME)/etc/connect_lib/connectLib:uCR
	$(XRUN) -compile -sv $(XRUN_OPTS) $(INCLUDES) -setdiscipline \
		-xmlibdirname worklib \
		$(CONSTANTS) \
		$(PRIMITIVES_SV) \
		$(TX_SRC) \
		$(RX_SRC) \
		$(PHY_SRC)
	$(XRUN) -elaborate $(XRUN_OPTS) \
		-ams amscf.scs \
		-amsconnrules uCR -xmlibdirname worklib -top phy_tb 
	# Run simulation using the elaborated library (worklib)
	$(XRUN) $(XRUN_OPTS) -xmlibdirname worklib -top phy_tb -input $(PHY_PROBE)
	@echo "UCIePHY simulation completed. Waveforms: phy_waves.shm"
	@echo ""

clean:
	@echo "Cleaning simulation outputs..."
	@rm -rf worklib *.log *.shm *.diag INCA_libs xcelium.d .simvision xrun.history .cadence worklibxrun
	@echo "Clean complete."