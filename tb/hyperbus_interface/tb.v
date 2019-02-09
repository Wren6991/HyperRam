

module tb;

localparam CLK_PERIOD = 10.0;

localparam W_BURSTLEN = 5;

// ----------------------------------------------------------------------------
// Interconnect

reg                    clk;
reg                    rst_n;

// Control

reg   [47:0]           cmd_addr;      // Full contents of the hyperbus CA packet
reg                    start;         // Start a new register/DRAM access sequence
wire                   start_rdy;     // Interface is ready to start a sequence
reg   [W_BURSTLEN-1:0] burst_len;     // Number of halfwords to transfer (double number of bytes)
reg   [3:0]            latency;       // Number of clocks between CA[23:16] being transferred, and first read/write data. Doubled if RWDS high during CA. >= 2
reg   [3:0]            recovery;      // Number of clocks to wait 
reg   [1:0]            capture_shmoo; // Capture DQi at 0, 180 or 360 clk degrees (0 90 180 HCLK degrees) after the DDR HCLK

// Data

reg   [7:0]            wdata;
wire                   wdata_rdy; // Backpressure only. Host must always provide valid data during a write transaction
wire  [7:0]            rdata;
wire                   rdata_vld; // Forward pressure only. Host must always accept data it has previously requested

// HyperBus

wire  [7:0]            dq_i;
wire  [7:0]            dq_o;
wire  [7:0]            dq_oe;

wire                   rwds_i;
wire                   rwds_o;
wire                   rwds_oe;

wire                   hclk_p;  // For 3V RAMs, just use the single-ended (positive) clock
wire                   hclk_n;

wire                    cs_n;

// ----------------------------------------------------------------------------
// Tristating

wire [7:0] dq;

genvar i;
generate
for (i = 0; i < 8; i = i + 1) begin: dq_tristate
	assign dq[i] = dq_oe[i] ? dq_o[i] : 1'bz;
end
endgenerate

assign dq_i = dq;

wire rwds = rwds_oe ? rwds_o : 1'bz;
assign rwds_i = rwds;


// ----------------------------------------------------------------------------
// Instantiate DUT and models

hyperbus_interface #(
	.W_BURSTLEN(W_BURSTLEN)
) dut (
	.clk           (clk),
	.rst_n         (rst_n),
	.cmd_addr      (cmd_addr),
	.start         (start),
	.start_rdy     (start_rdy),
	.burst_len     (burst_len),
	.latency       (latency),
	.recovery      (recovery),
	.capture_shmoo (capture_shmoo),
	.wdata         (wdata),
	.wdata_rdy     (wdata_rdy),
	.rdata         (rdata),
	.rdata_vld     (rdata_vld),
	.dq_i          (dq_i),
	.dq_o          (dq_o),
	.dq_oe         (dq_oe),
	.rwds_i        (rwds_i),
	.rwds_o        (rwds_o),
	.rwds_oe       (rwds_oe),
	.hclk_p        (hclk_p),
	.hclk_n        (hclk_n),
	.cs_n          (cs_n)
);


s27kl0641 #(
	.UserPreload(0)//,
) ram (
	.DQ7      (dq[7]),
	.DQ6      (dq[6]),
	.DQ5      (dq[5]),
	.DQ4      (dq[4]),
	.DQ3      (dq[3]),
	.DQ2      (dq[2]),
	.DQ1      (dq[1]),
	.DQ0      (dq[0]),
	.RWDS     (rwds),
	.CSNeg    (cs_n),
	.CK       (hclk_p),
	.RESETNeg (rst_n)
);

// ----------------------------------------------------------------------------
// Stimulus

always #(0.5 * CLK_PERIOD) if (ram.PoweredUp) clk = !clk;

initial begin

	clk = 1'b0;
	rst_n = 1'b0;

	cmd_addr = 0;
	start = 0;
	burst_len = 1;
	latency = 6;
	recovery = 0;
	capture_shmoo = 2; // zero degree capture
	wdata = 8'haa;


	#(10 * CLK_PERIOD);
	rst_n = 1'b1;

	// The model does some horrific string parsing to figure out whether to set this,
	// based on the TimingModel parameter. This is the only variable it sets,
	// and I can't be asked to reverse engineer their shitty string parsing
	ram.SPEED100 = 1;

	@ (posedge ram.PoweredUp); // skip the boring bit :)

	#(100 * CLK_PERIOD);

	@ (posedge clk);
	cmd_addr <= 48'hc000_0000_0000; // ID reg 0 read
	start <= 1'b1;
	@ (posedge clk);
	start <= 1'b0;
	@ (posedge clk);

	while (!start_rdy)
		@ (posedge clk);

	#(20 * CLK_PERIOD);
	$finish;
end

endmodule