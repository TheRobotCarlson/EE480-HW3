// basic sizes of things 
`define WORD	[15:0]
`define Opcode	[15:11]
`define Opcode2 [7:3]
`define Reg		[11:9]
`define Reg2	[2:0]
`define Imm		[8:0]
`define STATE	[5:0]

// TODO: modify for our code
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


module processor(halt, reset, clk);
	output reg halt;
	input reset, clk;

	reg `WORD regfile `REGSIZE;
	reg `WORD mainmem `MEMSIZE;
	reg `WORD pc = 0;
	reg `WORD ir;
	reg `STATE s = `Fetch;
	integer a;

	// Module Destinations
	reg `WORD f_add_l, f_add_r;
	reg `WORD f_sub_l, f_sub_r;
	reg `WORD f_mul_l, f_mul_r;
	reg `WORD 

	// Float Modules
	fadd fadd_left(, regfile[0] `Word, regfile[ir `Reg] `Word);
	fadd fadd_right()

	always @(reset) begin
	  halt = 0;
	  pc = 0;
	  s = `Fetch;
	  $readmemh2(regfile);
	  $readmemh1(mainmem);
	end

	always @(posedge clk) begin
	  case (s)
		`Fetch: begin ir <= mainmem[pc]; s <= `Execute; end // load from memory
		`Execute: 
			begin 
				pc <= pc + 1;            // bump pc
				if(5'b10000 < s)
					begin // phase 1 decoding
						case (ir `Opcode)
							`OPadd: begin regfile[0] <= regfile[0] + regfile[ir `Reg]; end
							`OPand: begin regfile[0] <= regfile[ir `Dest] & regfile[ir `Src];  end
							`OPany: begin regfile[0] <= |regfile[ir `Src]; s <= `Start; end
							`OPdup: begin regfile[0] <= regfile[ir `Src]; s <= `Start; end
							`OPjz: begin if (regfile[ir `Dest] == 0) pc <= regfile[ir `Src]; s <= `Start; end
							`OPld: begin regfile[ir `Dest] <= mainmem[regfile[ir `Src]]; s <= `Start; end
							`OPli: begin regfile[ir `Dest] <= mainmem[pc]; pc <= pc + 1; s <= `Start; end
							`OPor: begin regfile[ir `Dest] <= regfile[ir `Dest] | regfile[ir `Src]; s <= `Start; end
							`OPsz: begin if (regfile[ir `Dest] == 0) pc <= pc + 1; s <= `Start; end
							`OPshr: begin regfile[ir `Dest] <= regfile[ir `Src] >> 1; s <= `Start; end
							`OPst: begin mainmem[regfile[ir `Src]] <= regfile[ir `Dest]; s <= `Start; end
							`OPxor: begin regfile[ir `Dest] <= regfile[ir `Dest] ^ regfile[ir `Src]; s <= `Start; end
						
						endcase
						case (ir `Opcode2)
						
						
						endcase
					end
				else
					begin // phase 2 decoding
						case (ir `Opcode)
						
						
						endcase
					end
			end
		// phase 2 decoding

		default: halt <= 1;
	  endcase
	end
endmodule

// Floating point Verilog modules for CPE480
// Created February 19, 2019 by Henry Dietz, http://aggregate.org/hankd
// Distributed under CC BY 4.0, https://creativecommons.org/licenses/by/4.0/

// Field definitions
`define	WORD	[15:0]	// generic machine word size
`define	INT	signed [15:0]	// integer size
`define FLOAT	[15:0]	// half-precision float size
`define FSIGN	[15]	// sign bit
`define FEXP	[14:7]	// exponent
`define FFRAC	[6:0]	// fractional part (leading 1 implied)

// Constants
`define	FZERO	16'b0	  // float 0
`define F32767  16'h46ff  // closest approx to 32767, actually 32640
`define F32768  16'hc700  // -32768

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
	initial $readmemh0(look);
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