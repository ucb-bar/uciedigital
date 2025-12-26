XRUN := xrun
XRUN_OPTS := -sv_ms \
             -timescale 1ps/1fs \
             -access +rwc \
             -iereport
VAMS_DIR := ../verilog
# SPECTRE_HOME := /tools/cadence/SPECTRE/SPECTRE251
# XCELIUM_HOME := /tools/cadence/XCELIUM/XCELIUM2409.006/
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
	$(XRUN) $(XRUN_OPTS) \
		$(DISCIPLINES) \
		$(CONSTANTS) \
		$(PRIMITIVES_VAMS) \
		$(TX_VAMS) \
		$(RX_VAMS) \
		$(PRIMITIVES_SV) \
		$(TX_SRC) \
		$(RX_SRC) \
		$(PHY_SRC) \
		amscf.scs \
		-reflib $(XCELIUM_HOME)/tools.lnx86/affirma_ams/etc/connect_lib/connectLib:uCR \
		-amsconnrules uCR \
		-top phy_tb \
		-input $(PHY_PROBE)
	@echo "UCIePHY simulation completed. Waveforms: phy_waves.shm"
	@echo ""

clean:
	@echo "Cleaning simulation outputs..."
	@rm -rf worklib *.log *.shm *.diag INCA_libs xcelium.d .simvision xrun.history .cadence worklibxrun amscf.ams
	@echo "Clean complete."