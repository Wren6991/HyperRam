// Alternating posedge/negedge register stages for *input* timing adjustment
//
// Mux sels are decoded from input select. At most one will be selecting the "in" net
// 
// in -+------+-------------+-------------+
//     |      |             |             |
//     |      +--|\         +--|\         +--|\     
//     |         | |           | |           | |    
//     |  +---+  | |-+  +---+  | |-+  +---+  | |---- out 
//     +--|D Q|--|/  +--|D Q|--|/  +--|D Q|--|/
//        |   |         |   |         |   |         
//        +-^-+         +-^-+         +-^-+         
//          o             |             o
//          |             |             |
// clk -----+-------------+-------------+
//
// Above is for a MAX_DELAY of 3

module prog_halfclock_delay #(
	parameter MAX_DELAY = 2,                // Number of register stages to insert
	parameter FINAL_FALLING = 1,          // If 1, final stage is falling edge
	parameter W_SEL = $clog2(MAX_DELAY + 1) // let this default
) (
	input wire clk,
	input wire in,
	input wire [W_SEL-1:0] sel,
	output wire out
);

(* keep = 1'b1 *) reg [MAX_DELAY-1:0] q;
                  reg [MAX_DELAY  :0] d;

// Insert bypass muxes
// Numbering is a bit odd: d[i] is the D *generated from* q[i]
// i.e. the input to the following flop

always @ (*) begin: bypass
	integer i;
	for (i = 0; i <= MAX_DELAY; i = i + 1) begin
		if (i == MAX_DELAY || sel == i)
			d[i] = in;
		else
			d[i] = q[i];
	end
end

genvar i;
generate
for (i = 0; i < MAX_DELAY; i = i + 1) begin: flops
	if (i[0] ^ |FINAL_FALLING) begin
		always @ (negedge clk)
			q[i] <= d[i + 1];
	end else begin
		always @ (posedge clk)
			q[i] <= d[i + 1];
	end
end
endgenerate

assign out = d[0];

endmodule