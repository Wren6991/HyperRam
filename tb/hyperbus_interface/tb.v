module tb;

localparam CLK_PERIOD = 10.0;

localparam W_BURSTLEN = 5;


reg                    clk;
reg                    rst_n;

// Control

reg   [47:0]           cmd_addr;      // Full contents of the hyperbus CA packet
reg                    start_reg;     // Start a new register access sequence
reg                    start_data;    // Start a new DRAM array access sequence
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

reg   [7:0]            dq_i;
wire  [7:0]            dq_o;
wire  [7:0]            dq_oe;

reg                    rwds_i;
wire                   rwds_o;
wire                   rwds_oe;

wire                   hclk_p;  // For 3V RAMs, just use the single-ended (positive) clock
wire                   hclk_n;

wire                    cs_n;


hyperbus_interface #(
	.W_BURSTLEN(W_BURSTLEN)
) inst_hyperbus_interface (
	.clk           (clk),
	.rst_n         (rst_n),
	.cmd_addr      (cmd_addr),
	.start_reg     (start_reg),
	.start_data    (start_data),
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
	.cs_n          (cs_n),
);

initial clk = 1'b0;
always #(0.5 * CLK_PERIOD) clk = !clk;

initial begin
	rst_n = 1'b0;


	cmd_addr = 0;
	start_reg = 0;
	start_data = 0;
	burst_len = 0;
	latency = 0;
	recovery = 0;
	capture_shmoo = 0;
	wdata = 0;
	dq_i = 0;
	rwds_i = 0;
	
	#(10 * CLK_PERIOD)
	rst_n = 1'b1;

	#(10 * CLK_PERIOD);
	$finish;
end

endmodule