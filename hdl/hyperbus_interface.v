// Encapsulates all the timing details of the HyperBus interface
// Contains some half-cycle paths, but these should all be register-register
// (or at most a couple of muxes)

// HyperBus is a DDR interface. Each transaction consists of:
// - CSn assertion while clock is idle (low)
// - A 48 bit command and address (CA) sequence, clocked out on 6 edges
// - An access latency period. Latency clock count is configured via config register in the HRAM,
//   and latency is doubled if a RAM refresh operation is in progress, which is signalled by RWDS
//   high during CA phase.
// - A read/write data burst transferring one byte per clock edge, even number of bytes total.
// - CSn deassertion while clock idle, to terminate the burst
// - A short recovery period before reasserting CSn
//
// HCLK need not be free-running.
// 
// During write data bursts, RWDS functions as a byte masking signal, allowing
// individual bytes on a DRAM row to be updated without R-M-W sequence. We don't use this.
// 
// CA, write data, and RWDS (during write bursts) should be centre-clocked by the HCLK signal,
// so that the slave capture is aligned with the signal eye.
// 
// CSn ¬¬¬____________________________________¬¬¬¬
// CLK __________¬¬¬¬¬¬¬¬________¬¬¬¬¬¬¬¬_________
// DQ  ------<  A   ><  B   ><  C   ><  D   >
//
//
// During read data bursts, RWDS is used as a source-synchronous DDR strobe, aligned
// with DQ transitions. Some RAMs also have a secondary clock input which allows
// RWDS to be skewed to align it with the DQ eye. As we are only aiming for low speed operation,
// we will simply capture DQ on our transmitted clock. TODO: some shmooing on the capture?
//
// For register accesses, there is no latency period: CA is followed immediately by a 16-bit register value.
// The entire access occurs on 8 consecutive clock edges.

module hyperbus_interface #(
	parameter W_BURSTLEN = 5
) (
	input wire                   clk,
	input wire                   rst_n,

	// Control

	input  wire [47:0]           cmd_addr,      // Full contents of the hyperbus CA packet
	input  wire                  start,         // Start a new DRAM/register access sequence
	output wire                  start_rdy,     // Interface is ready to start a sequence
	input  wire [W_BURSTLEN-1:0] burst_len,     // Number of halfwords to transfer (double number of bytes)
	input  wire [3:0]            latency,       // Number of clocks between CA[23:16] being transferred, and first read/write data. Doubled if RWDS high during CA. >= 2
	input  wire [3:0]            recovery,      // Number of clocks to wait 
	input  wire [1:0]            capture_shmoo, // Capture DQi at 0, 180 or 360 clk degrees (0 90 180 HCLK degrees) after the DDR HCLK
	                                            // edge which causes it to transition to *next* data. 0 degrees probably correct for almost all speeds.
	                                            // 2 -> 0 degrees
	                                            // 1 -> 180 degrees
	                                            // 0 -> 360 degrees

	// Data

	input  wire [7:0]            wdata,
	output wire                  wdata_rdy, // Backpressure only. Host must always provide valid data during a write transaction
	output wire [7:0]            rdata,
	output wire                  rdata_vld, // Forward pressure only. Host must always accept data it has previously requested

	// HyperBus

	input  wire [7:0]            dq_i,
	output wire [7:0]            dq_o,
	output wire [7:0]            dq_oe,

	input  wire                  rwds_i,
	output wire                  rwds_o,
	output wire                  rwds_oe,

	output  reg                  hclk_p,  // For 3V RAMs, just use the single-ended (positive) clock
	output  reg                  hclk_n,

	output  reg                  cs_n
);

// ----------------------------------------------------------------------------
// Hyperbus state machine

localparam W_STATE = 3;

localparam S_IDLE     = 3'd0; // Ready to start a new sequence
localparam S_SETUP    = 3'd1; // Asserting CS and first CA byte
localparam S_CA       = 3'd2; // Driving clock and shifting CA packet
localparam S_LATENCY  = 3'd3; // Driving clock until access latency elapses
localparam S_RBURST   = 3'd4; // Driving clock and capturing read data
localparam S_WBURST   = 3'd5; // Driving clock and write data
localparam S_RECOVERY = 3'd6; // Hold CS high a while before accepting new command

reg [W_STATE-1:0] bus_state_next; // combinatorial
reg [W_STATE-1:0] bus_state;
reg [W_STATE-1:0] bus_state_prev;

always @ (posedge clk or negedge rst_n) begin
	if (!rst_n) begin
		bus_state <= S_IDLE;
		bus_state_prev <= S_IDLE;
	end else begin
		bus_state <= bus_state_next;
		bus_state_prev <= bus_state;
	end
end

// Some useful housekeeping values

reg [W_BURSTLEN-1:0] cycle_ctr;

reg latency_2x; // RWDS sample taken during CA phase (2 clocks in seems ok)
always @ (posedge clk or negedge rst_n)
	if (!rst_n)
		latency_2x <= 1'b0;
	else if (bus_state == S_CA && cycle_ctr == 5'h3 && hclk_p)
		latency_2x <= rwds_i;

reg is_reg_write;
always @ (posedge clk or negedge rst_n)
	if (!rst_n)
		is_reg_write <= 1'b0;
	else if (start_rdy)
		is_reg_write <= start && cmd_addr[47:46] == 2'b01;

reg is_write;
always @ (posedge clk or negedge rst_n)
	if (!rst_n)
		is_write <= 1'b0;
	else if (start && start_rdy)
		is_write <= !cmd_addr[47];

// Counter logic

// - 1 because the count starts after row address (1 hclk before end of CA)
wire [W_BURSTLEN-1:0] latency_after_ca = (latency << latency_2x) - 5'h1;

always @ (posedge clk or negedge rst_n) begin
	if (!rst_n) begin
		cycle_ctr <= {W_BURSTLEN{1'b0}};
	end else begin
		if (bus_state == S_IDLE && bus_state_next == S_CA) begin
			cycle_ctr <= 5'h3;
		end else if (bus_state == S_CA && bus_state_next == S_LATENCY) begin
			cycle_ctr <= latency_after_ca;
			// Note that for low latency settings (2 cycles) we go straight from CA to burst if RWDS was low:
		end else if ((bus_state == S_CA || bus_state == S_LATENCY) && (bus_state_next == S_RBURST || bus_state_next == S_WBURST)) begin
			cycle_ctr <= is_reg_write ? 1 : burst_len;
		end else if (hclk_p) begin
			// Counter transitions each time hclk returns to idle state (count full pulses, not DDR edges)
			cycle_ctr <= cycle_ctr - 1'b1;
		end
	end
end

// Main state transitions

wire final_edge = cycle_ctr == 5'h1 && hclk_p;

always @ (*) begin
	bus_state_next = bus_state;
	case (bus_state)
	S_IDLE: begin
		if (start)
			bus_state_next = S_CA;
	end
	S_CA: begin
		if (final_edge) begin
			if (|latency_after_ca && !is_reg_write)
				bus_state_next = S_LATENCY;
			else
				bus_state_next = is_write ? S_WBURST : S_RBURST;
		end
	end
	S_LATENCY: begin
		if (final_edge)
			bus_state_next = is_write ? S_WBURST : S_RBURST;
	end
	S_RBURST: begin
		if (final_edge)
			bus_state_next = S_RECOVERY;
	end
	S_WBURST: begin
		if (final_edge)
			bus_state_next = S_RECOVERY;
	end
	S_RECOVERY: begin
		bus_state_next = S_IDLE; //TODO add some way of controlling this length
	end

	endcase
end

// ----------------------------------------------------------------------------
// Handle non-DQ bus signals

wire drive_clk =
	bus_state == S_CA      ||
	bus_state == S_LATENCY ||
	bus_state == S_WBURST  ||
	bus_state == S_RBURST  ;

always @ (posedge clk or negedge rst_n) begin
	if (!rst_n) begin
		hclk_p <= 1'b0;
		hclk_n <= 1'b1;
	end else if (drive_clk) begin
		hclk_p <= hclk_n;
		hclk_n <= hclk_p;
	end
end

reg rwds_assert;
always @ (posedge clk or negedge rst_n)
	if (!rst_n)
		rwds_assert <= 1'b0;
	else
		rwds_assert <= bus_state_next == S_WBURST;

reg rwds_assert_falling;
always @ (negedge clk or negedge rst_n)
	if (!rst_n)
		rwds_assert_falling <= 1'b0;
	else
		rwds_assert_falling <= rwds_assert;

assign rwds_o = 1'b1;
assign rwds_oe = rwds_assert_falling;

always @ (posedge clk or negedge rst_n)
	if (!rst_n)
		cs_n <= 1'b1; // active high, deassert at reset
	else
		cs_n <= bus_state_next == S_IDLE || bus_state == S_RECOVERY;

// ----------------------------------------------------------------------------
// Launch/capture flops for DQ (half-clock retiming)

reg [7:0] dq_o_reg;
reg [7:0] dq_o_reg_falling;

// HCLK transitions align with posedge of clk.
// DQ outputs are eye-aligned, so we write bus data to dq_o_reg on the posedge 
// *before* the HCLK transition, and then register again via dq_o_reg_falling 
// to align transitions with clk negedge.

always @ (negedge clk or negedge rst_n)
	if (!rst_n)
		dq_o_reg_falling <= 8'h0;
	else
		dq_o_reg_falling <= dq_o_reg;


assign dq_o = dq_o_reg_falling;

// Drive period is aligned with assertion of write data
// i.e. we generate it one clk before the HCLK edge
// and then delay by half a clock

reg dq_oe_reg;
always @ (posedge clk or negedge rst_n)
	if (!rst_n)
		dq_oe_reg <= 1'b0;
	else
		dq_oe_reg <= bus_state_next == S_WBURST || bus_state_next == S_CA;

reg dq_oe_reg_falling;
always @ (negedge clk or negedge rst_n)
	if (!rst_n)
		dq_oe_reg_falling <= 1'b0;
	else
		dq_oe_reg_falling <= dq_oe_reg;

assign dq_oe = {8{dq_oe_reg_falling}};

// Read is going to need some diagrams :)

wire [7:0] dq_i_delay;

prog_halfclock_delay #(
	.MAX_DELAY(2),
	.FINAL_FALLING(1)
) dq_i_delay_line [7:0] (
	.clk (clk),
	.in  (dq_i),
	.sel (capture_shmoo),
	.out (dq_i_delay)
);

reg [7:0] dq_i_reg;
always @ (posedge clk or negedge rst_n)
	if (!rst_n)
		dq_i_reg <= 8'h0;
	else
		dq_i_reg <= dq_i_delay;

// ----------------------------------------------------------------------------
// Host interfaces


// The first byte of CA packet goes straight to bus. Rest is captured and shifted:
reg [39:0] ca_shift;
always @ (posedge clk or negedge rst_n)
	if (!rst_n)
		ca_shift <= 40'h0;
	else if (bus_state_next == S_CA && bus_state != S_CA)
		ca_shift <= cmd_addr[39:0];
	else
		ca_shift <= ca_shift << 8;

// Recall that dq_o_reg is delayed by half a clk before appearing on bus,
// and is captured on the *following* HCLK transition.
always @ (posedge clk or negedge rst_n)
	if (!rst_n)
		dq_o_reg <= 8'h0;
	else if (bus_state_next == S_CA)
		dq_o_reg <= bus_state == S_CA ? ca_shift[39:32] : cmd_addr[47:40];
	else
		dq_o_reg <= wdata;

assign wdata_rdy = bus_state_next == S_WBURST;

reg [1:0] rdata_vld_reg;
always @ (posedge clk or negedge rst_n)
	if (!rst_n)
		rdata_vld_reg <= 2'b00;
	else
		rdata_vld_reg <= {rdata_vld_reg[0], bus_state_prev == S_RBURST};

assign rdata_vld = rdata_vld_reg[1];
assign rdata = dq_i_reg;

assign start_rdy = bus_state == S_IDLE;


endmodule