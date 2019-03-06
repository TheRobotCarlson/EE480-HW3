// basic sizes of things 
`define WORD	 [15:0] // Standard word length
`define OPCODE1	 [15:11] // Typical opcode field
`define OPCODE2  [7:3] // VLIW 2nd Op location
`define REG1	 [10:8] // Typical opcode field 
`define REG2	 [2:0] // VLIW 2nd Op location
`define IMM8	 [7:0] // Immediate 8 bit value location
`define STATE	 [5:0]
`define HALFWORD [7:0]
`define ACC1 3'b000 // Location for first accumulator
`define ACC2 3'b001 // Location for second accumulator
`define TYPEBIT 16 // Where the register type is defined (float = 1, int = 0)

// TODO: modify for our code
`define REGWORD [16:0]
`define REGSIZE [7:0]
`define MEMSIZE [65535:0]

// 8 bit operators - PHASE 1 DECODING
// opcode values, also state numbers
`define OPa2r	5'b00000
`define OPr2a	5'b00001
`define OPlf	5'b00010
`define OPli	5'b00011
`define OPst	5'b00100
`define OPcvt	5'b00101
`define OPsh	5'b00110
`define OPslt	5'b00111
`define OPadd	5'b01000
`define OPsub	5'b01001
`define OPmul	5'b01010
`define OPdiv	5'b01011
`define OPnot	5'b01100
`define OPxor	5'b01101
`define OPand	5'b01110
`define OPor	5'b01111
`define OPjr	5'b10000

// PHASE 2 DECODING
// 13 bit + 3 bit padding: pre jp8 sys
`define OPpre	5'b10001 // must be 17
`define OPjp8   5'b10010
`define OPsys   5'b10011

// 16 bit	
// cf8 ci8 jnz8 jz8
`define OPcf8	5'b10100
`define OPci8	5'b10101
`define OPjnz8	5'b10110
`define OPjz8	5'b10111

// state numbers only
`define Fetch	5'b11111
`define Execute	5'b11110

// Float Field definitions
`define	INT	signed [15:0]	// integer size
`define FLOAT	[15:0]	// half-precision float size
`define FSIGN	[15]	// sign bit
`define FEXP	[14:7]	// exponent
`define FFRAC	[6:0]	// fractional part (leading 1 implied)

// Float Constants
`define	FZERO	16'b0	  // float 0
`define F32767  16'h46ff  // closest approx to 32767, actually 32640
`define F32768  16'hc700  // -32768

module processor(halt, reset, clk);
	output reg halt;
	input reset, clk;

	reg `HALFWORD pre;
	reg `REGWORD regfile `REGSIZE;
	reg `WORD mainmem `MEMSIZE;
	reg `WORD pc = 0;
	reg `WORD ir;
	reg `STATE s = `Fetch;

	// Float module destinations
	wire `WORD f_add_l, f_add_r;
	wire `WORD f_sub_l, f_sub_r;
	wire `WORD f_mul_l, f_mul_r;
	wire `WORD f_recip_l, f_recip_r;
	wire `WORD f_shift_l, f_shift_r;
	wire `WORD f_div_l, f_div_r;
	wire `WORD f_f2i_l, f_f2i_r;
	wire `WORD f_i2f_l, f_i2f_r;
	wire f_slt_l, f_slt_r;

	// Float module instantiation
	// add
	fadd fadd_left(f_add_l, regfile[`ACC1] `WORD, regfile[ir `REG1] `WORD);
	fadd fadd_right(f_add_r, regfile[`ACC2] `WORD, regfile[ir `REG2] `WORD);
	// sub (flip register value's MSB to change sign and then add)
	fadd fsub_left(f_sub_l, regfile[`ACC1] `WORD, (regfile[ir `REG1] `WORD)^16'h8000);
	fadd fsub_right(f_sub_r, regfile[`ACC2] `WORD, (regfile[ir `REG2] `WORD)^16'h8000);
	// mul
	fmul fmul_left(f_mul_l, regfile[`ACC1] `WORD, regfile[ir `REG1] `WORD);
	fmul fmul_right(f_mul_r, regfile[`ACC2] `WORD, regfile[ir `REG2] `WORD);
	// div (recip then mul)
	frecip frecip_left(f_recip_l, regfile[`ACC1] `WORD);
	frecip frecip_right(f_recip_r, regfile[`ACC2] `WORD);
	fmul fdiv_left(f_div_l, f_recip_l, regfile[ir `REG1] `WORD);
	fmul fdiv_right(f_div_r, f_recip_r, regfile[ir `REG2] `WORD);
	// shift
	fshift fshift_left(f_shift_l, regfile[`ACC1] `WORD, regfile[ir `REG1] `WORD);
	fshift fshift_right(f_shift_r, regfile[`ACC2] `WORD, regfile[ir `REG2] `WORD);
	// f2i
	f2i ff2i_left(f_f2i_l, regfile[ir `REG1] `WORD);
	f2i ff2i_right(f_f2i_r, regfile[ir `REG2] `WORD);
	// i2f
	f2i fi2f_left(f_i2f_l, regfile[ir `REG1] `WORD);
	f2i fi2f_right(f_i2f_r, regfile[ir `REG2] `WORD);
	// slt
	fslt fslt_left(f_slt_l, regfile[`ACC1] `WORD, regfile[ir `REG1] `WORD);
	fslt fslt_right(f_slt_r, regfile[`ACC2] `WORD, regfile[ir `REG2] `WORD);


	always @(reset) begin
		halt = 0;
		pc = 0;
		s = `Fetch;
		//$readmemh("vmem0-float.vmem", regfile);
		$readmemh("vmem1-text.vmem", mainmem);
	end

	always @(posedge clk) begin
	  case (s)
		`Fetch: begin ir <= mainmem[pc]; s <= `Execute; end // load from memory
		`Execute: 
			begin 
				$display("pre: %d", pre);
				$display("r0: %d", regfile[`ACC1]);
				$display("r1: %d", regfile[`ACC2]);
				$display("r2: %d", regfile[2]);
				$display("r3: %d", regfile[3]);
				$display("r4: %d", regfile[4]);
				$display("r5: %d", regfile[5]);
				$display("r6: %d", regfile[6]);
				$display("r7: %d", regfile[7]);
				pc <= pc + 1; // bump pc
				if(5'b10000 > ir `OPCODE1) begin // phase 1 decoding
					// Left VLIW Instruction
					case (ir `OPCODE1)
						`OPa2r: begin regfile[ir `REG1] <= regfile[`ACC1]; end
						`OPr2a: begin regfile[`ACC1] <= regfile[ir `REG1]; end
						`OPjr: begin pc <= regfile[ir `REG1] `WORD; end
						`OPst: begin mainmem[regfile[ir `REG1]] <= regfile[`ACC1]; end //to check
						`OPlf: begin regfile[ir `REG1] <= mainmem[pc]; end //to check
						`OPli: begin regfile[ir `REG1] <= mainmem[pc]; end //to check PC increment
						// ALU
						`OPcvt: begin
							regfile[`ACC1] `WORD <= regfile[ir `REG1][`TYPEBIT] ? f_f2i_l : f_i2f_l;
							regfile[`ACC1][`TYPEBIT] <= regfile[`ACC1][`TYPEBIT]^1'b1; // Flip register type
						end
						`OPslt: begin
							if(regfile[ir `REG1][`TYPEBIT]) begin // Use float slt
								regfile[`ACC1] `WORD <= f_slt_l;
								regfile[`ACC1][`TYPEBIT] <= 1'b1; // Set acc type to int
							end else begin // User int slt
								regfile[`ACC1] `WORD <= regfile[`ACC1] < regfile[ir `REG1];
								regfile[`ACC1][`TYPEBIT] <= 1'b1; // Set acc type to int
							end
						end
						`OPsh:  begin regfile[`ACC1] `WORD <= regfile[`ACC1][`TYPEBIT] ? f_shift_l : regfile[`ACC1]<<regfile[ir `REG1]; end
						`OPadd: begin 	$display("add"); regfile[`ACC1] `WORD <= regfile[`ACC1][`TYPEBIT] ? f_add_l : regfile[`ACC1]+regfile[ir `REG1]; end
						`OPsub:	begin regfile[`ACC1] `WORD <= regfile[`ACC1][`TYPEBIT] ? f_sub_l : regfile[`ACC1]-regfile[ir `REG1]; end
						`OPmul: begin regfile[`ACC1] `WORD <= regfile[`ACC1][`TYPEBIT] ? f_mul_l : regfile[`ACC1]*regfile[ir `REG1]; end
						`OPdiv: begin regfile[`ACC1] `WORD <= regfile[`ACC1][`TYPEBIT] ? f_div_l : regfile[`ACC1]/regfile[ir `REG1]; end
						`OPnot: begin regfile[`ACC1] `WORD <= ~(regfile[ir `REG1]); end
						`OPxor: begin regfile[`ACC1] `WORD <= regfile[`ACC1] ^ regfile[ir `REG1]; end
						`OPand: begin regfile[`ACC1] `WORD <= regfile[`ACC1] & regfile[ir `REG1]; end
						`OPor:  begin regfile[`ACC1] `WORD <= regfile[`ACC1] | regfile[ir `REG1]; end
					endcase
					// Right VLIW Instruction
					case (ir `OPCODE2)
						`OPa2r: begin regfile[ir `REG2] <= regfile[`ACC2]; end
						`OPr2a: begin regfile[`ACC2] <= regfile[ir `REG2]; end
						`OPjr: begin pc <= regfile[ir `REG2] `WORD; end
						`OPst: begin mainmem[regfile[ir `REG2]] <= regfile[`ACC2]; end //to check
						`OPlf: begin regfile[ir `REG2] <= mainmem[pc]; end //to check
						`OPli: begin regfile[ir `REG2] <= mainmem[pc]; end //to check PC increment
						// ALU
						`OPcvt: begin
							regfile[`ACC2] `WORD <= regfile[ir `REG2][`TYPEBIT] ? f_f2i_r : f_i2f_r;
							regfile[`ACC2][`TYPEBIT] <= regfile[`ACC2][`TYPEBIT]^1'b1; // Flip register type
						end
						`OPslt: begin
							if(regfile[ir `REG2][`TYPEBIT]) begin // Use float slt
								regfile[`ACC2] `WORD <= f_slt_r;
								regfile[`ACC2][`TYPEBIT] <= 1'b1; // Set acc type to int
							end else begin // User int slt
								regfile[`ACC2] `WORD <= regfile[`ACC2] < regfile[ir `REG2];
								regfile[`ACC2][`TYPEBIT] <= 1'b1; // Set acc type to int
							end
						end
						`OPsh:  begin regfile[`ACC2] `WORD <= regfile[`ACC2][`TYPEBIT] ? f_shift_r : regfile[`ACC2]<<regfile[ir `REG2]; end
						`OPadd: begin regfile[`ACC2] `WORD <= regfile[`ACC2][`TYPEBIT] ? f_add_r : regfile[`ACC2]+regfile[ir `REG2]; end
						`OPsub:	begin regfile[`ACC2] `WORD <= regfile[`ACC2][`TYPEBIT] ? f_sub_r : regfile[`ACC2]-regfile[ir `REG2]; end
						`OPmul: begin regfile[`ACC2] `WORD <= regfile[`ACC2][`TYPEBIT] ? f_mul_r : regfile[`ACC2]*regfile[ir `REG2]; end
						`OPdiv: begin regfile[`ACC2] `WORD <= regfile[`ACC2][`TYPEBIT] ? f_div_r : regfile[`ACC2]/regfile[ir `REG2]; end
						`OPnot: begin regfile[`ACC2] `WORD <= ~(regfile[ir `REG2]); end
						`OPxor: begin regfile[`ACC2] `WORD <= regfile[`ACC2] ^ regfile[ir `REG2]; end
						`OPand: begin regfile[`ACC2] `WORD <= regfile[`ACC2] & regfile[ir `REG2]; end
						`OPor:  begin regfile[`ACC2] `WORD <= regfile[`ACC2] | regfile[ir `REG2]; end
					endcase
				end
				else begin // phase 2 decoding
					case (ir `OPCODE1)
						`OPpre: begin $display("pre"); pre <= ir `IMM8; end
						`OPjp8: begin pc <= {pre, ir `IMM8}; end
						`OPjnz8: begin if (ir `REG1 != 0) pc <= {pre, ir `IMM8}; end
						`OPjz8: begin if (ir `REG1 == 0) pc <= {pre, ir `IMM8}; end
						`OPcf8: begin regfile[ir `REG1] = {1'b1, pre, ir `IMM8}; end
						`OPci8: begin $display("ci8"); regfile[ir `REG1] = {1'b0, pre, ir `IMM8}; end
						`OPsys: begin $display("sys"); halt <= 1; end
					endcase
				end
				s <= `Fetch;
			end
	  endcase
	end
endmodule



// ************************************************ Float ********************************************

// Floating point Verilog modules for CPE480
// Created February 19, 2019 by Henry Dietz, http://aggregate.org/hankd
// Distributed under CC BY 4.0, https://creativecommons.org/licenses/by/4.0/

// Count leading zeros, 16-bit (5-bit result) d=lead0s(s)
module lead0s(d, s);
	output wire [4:0] d;
	input wire `WORD s;
	wire [4:0] t;
	wire [7:0] s8;
	wire [3:0] s4;
	wire [1:0] s2;
	assign t[4] = 0;
	assign {t[3],s8} = ((|s[15:8]) ? {1'b0,s[15:8]} : {1'b1,s[7:0]});
	assign {t[2],s4} = ((|s8[7:4]) ? {1'b0,s8[7:4]} : {1'b1,s8[3:0]});
	assign {t[1],s2} = ((|s4[3:2]) ? {1'b0,s4[3:2]} : {1'b1,s4[1:0]});
	assign t[0] = !s2[1];
	assign d = (s ? t : 16);
endmodule

// Float set-less-than, 16-bit (1-bit result) torf=a<b
module fslt(torf, a, b);
	output wire torf;
	input wire `FLOAT a, b;
	assign torf = (a `FSIGN && !(b `FSIGN)) ||
			  (a `FSIGN && b `FSIGN && (a[14:0] > b[14:0])) ||
			  (!(a `FSIGN) && !(b `FSIGN) && (a[14:0] < b[14:0]));
endmodule

// Floating-point addition, 16-bit r=a+b
module fadd(r, a, b);
	output wire `FLOAT r;
	input wire `FLOAT a, b;
	wire `FLOAT s;
	wire [8:0] sexp, sman, sfrac;
	wire [7:0] texp, taman, tbman;
	wire [4:0] slead;
	wire ssign, aegt, amgt, eqsgn;
	assign r = ((a == 0) ? b : ((b == 0) ? a : s));
	assign aegt = (a `FEXP > b `FEXP);
	assign texp = (aegt ? (a `FEXP) : (b `FEXP));
	assign taman = (aegt ? {1'b1, (a `FFRAC)} : ({1'b1, (a `FFRAC)} >> (texp - a `FEXP)));
	assign tbman = (aegt ? ({1'b1, (b `FFRAC)} >> (texp - b `FEXP)) : {1'b1, (b `FFRAC)});
	assign eqsgn = (a `FSIGN == b `FSIGN);
	assign amgt = (taman > tbman);
	assign sman = (eqsgn ? (taman + tbman) : (amgt ? (taman - tbman) : (tbman - taman)));
	lead0s m0(slead, {sman, 7'b0});
	assign ssign = (amgt ? (a `FSIGN) : (b `FSIGN));
	assign sfrac = sman << slead;
	assign sexp = (texp + 1) - slead;
	assign s = (sman ? (sexp ? {ssign, sexp[7:0], sfrac[7:1]} : 0) : 0);
endmodule

// Floating-point multiply, 16-bit r=a*b
module fmul(r, a, b);
	output wire `FLOAT r;
	input wire `FLOAT a, b;
	wire [15:0] m; // double the bits in a fraction, we need high bits
	wire [7:0] e;
	wire s;
	assign s = (a `FSIGN ^ b `FSIGN);
	assign m = ({1'b1, (a `FFRAC)} * {1'b1, (b `FFRAC)});
	assign e = (((a `FEXP) + (b `FEXP)) -127 + m[15]);
	assign r = (((a == 0) || (b == 0)) ? 0 : (m[15] ? {s, e, m[14:8]} : {s, e, m[13:7]}));
endmodule

// Floating-point reciprocal, 16-bit r=1.0/a
// Note: requires initialized inverse fraction lookup table
module frecip(r, a);
	output wire `FLOAT r;
	input wire `FLOAT a;
	reg [6:0] look[127:0];
	initial $readmemh("vmem0-float.vmem", look);
	assign r `FSIGN = a `FSIGN;
	assign r `FEXP = 253 + (!(a `FFRAC)) - a `FEXP;
	assign r `FFRAC = look[a `FFRAC];
endmodule

// Floating-point shift, 16 bit
// Shift +left,-right by integer
module fshift(r, f, i);
	output wire `FLOAT r;
	input wire `FLOAT f;
	input wire `INT i;
	assign r `FFRAC = f `FFRAC;
	assign r `FSIGN = f `FSIGN;
	assign r `FEXP = (f ? (f `FEXP + i) : 0);
endmodule

// Integer to float conversion, 16 bit
module i2f(f, i);
	output wire `FLOAT f;
	input wire `INT i;
	wire [4:0] lead;
	wire `WORD pos;
	assign pos = (i[15] ? (-i) : i);
	lead0s m0(lead, pos);
	assign f `FFRAC = (i ? ({pos, 8'b0} >> (16 - lead)) : 0);
	assign f `FSIGN = i[15];
	assign f `FEXP = (i ? (128 + (14 - lead)) : 0);
endmodule

// Float to integer conversion, 16 bit
// Note: out-of-range values go to -32768 or 32767
module f2i(i, f);
	output wire `INT i;
	input wire `FLOAT f;
	wire `FLOAT ui;
	wire tiny, big;
	fslt m0(tiny, f, `F32768);
	fslt m1(big, `F32767, f);
	assign ui = {1'b1, f `FFRAC, 16'b0} >> ((128+22) - f `FEXP);
	assign i = (tiny ? 0 : (big ? 32767 : (f `FSIGN ? (-ui) : ui)));
endmodule


module testbench;
    reg reset = 0;
    reg clk = 0;
    wire halted;
    processor PE(halted, reset, clk);
    initial begin
        $dumpfile("output.txt");
        $dumpvars(0, PE);
        #10 reset = 1;
        #10 reset = 0;
        while (!halted) begin
            #10 clk = 1;
            #10 clk = 0;
        end
        $finish;
    end
endmodule
