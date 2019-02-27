// basic sizes of things 
`define WORD	[15:0]
`define Opcode	[15:11]
`define Reg		[11:9]
`define Imm		[8:0]
`define STATE	[5:0]

// TODO: modify for our code
`define REGSIZE [63:0]
`define MEMSIZE [65535:0]

// 8 bit operators
// opcode values, also state numbers
`define OPa2r	4'b00000
`define OPr2a	4'b00001
`define OPlf	4'b00010
`define OPli	4'b00011
`define OPst	4'b00100
`define OPcvt	4'b00101
`define OPsh	4'b00110
`define OPslt	4'b00111
`define OPadd	4'b01000
`define OPsub	4'b01001
`define OPmul	4'b01010
`define OPdiv	4'b01011
`define OPnot	4'b01100
`define OPxor	4'b01101
`define OPand	4'b01110
`define OPor	4'b01111
`define OPjr	4'b10000

// 13 bit + 3 bit padding: pre jp8 sys
`define OPpre	4'b10001 // must be 17
`define OPjp8   4'b10010
`define OPsys   4'b10011

// 16 bit
// cf8 ci8 jnz8 jz8
`define OPcf8	4'b10100
`define OPci8	4'b10101
`define OPjnz8	4'b10110
`define OPjz8	4'b10111


// state numbers only
`define OPjz	`OPjzsz
`define OPsys	5'b10000
`define OPsz	5'b10001
`define Start	5'b11111
`define Start1	5'b11110

// source field values for sys and sz
`define SRCsys	6'b000000
`define SRCsz	6'b000001

module processor(halt, reset, clk);
	output reg halt;
	input reset, clk;

	reg `WORD regfile `REGSIZE;
	reg `WORD mainmem `MEMSIZE;
	reg `WORD pc = 0;
	reg `WORD ir;
	reg `STATE s = `Start;
	integer a;

	always @(reset) begin
	  halt = 0;
	  pc = 0;
	  s = `Start;
	  $readmemh0(regfile);
	  $readmemh1(mainmem);
	end

	always @(posedge clk) begin
	  case (s)
		`Start: begin ir <= mainmem[pc]; s <= `Start1; end
		`Start1: begin
				 pc <= pc + 1;            // bump pc
			 case (ir `Opcode)
			 `OPjzsz:
					case (ir `Src)	      // use Src as extended opcode
					`SRCsys: s <= `OPsys; // sys call
					`SRCsz: s <= `OPsz;   // sz
					default: s <= `OPjz;  // jz
			 endcase
				 default: s <= ir `Opcode; // most instructions, state # is opcode
			 endcase
			end

		`OPadd: begin regfile[ir `Dest] <= regfile[ir `Dest] + regfile[ir `Src]; s <= `Start; end
		`OPand: begin regfile[ir `Dest] <= regfile[ir `Dest] & regfile[ir `Src]; s <= `Start; end
		`OPany: begin regfile[ir `Dest] <= |regfile[ir `Src]; s <= `Start; end
		`OPdup: begin regfile[ir `Dest] <= regfile[ir `Src]; s <= `Start; end
		`OPjz: begin if (regfile[ir `Dest] == 0) pc <= regfile[ir `Src]; s <= `Start; end
		`OPld: begin regfile[ir `Dest] <= mainmem[regfile[ir `Src]]; s <= `Start; end
		`OPli: begin regfile[ir `Dest] <= mainmem[pc]; pc <= pc + 1; s <= `Start; end
		`OPor: begin regfile[ir `Dest] <= regfile[ir `Dest] | regfile[ir `Src]; s <= `Start; end
		`OPsz: begin if (regfile[ir `Dest] == 0) pc <= pc + 1; s <= `Start; end
		`OPshr: begin regfile[ir `Dest] <= regfile[ir `Src] >> 1; s <= `Start; end
		`OPst: begin mainmem[regfile[ir `Src]] <= regfile[ir `Dest]; s <= `Start; end
		`OPxor: begin regfile[ir `Dest] <= regfile[ir `Dest] ^ regfile[ir `Src]; s <= `Start; end

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

// Testing
module testbench;
	reg `FLOAT a, b;
	reg `WORD r;
	wire `FLOAT addr,mulr, recr, shir, i2fr;
	wire `INT f2ir, i, j, ia, ib, addri;
	reg `WORD ref[1024:0];
	f2i myfa(ia, a);
	f2i myfb(ib, b);
	fadd myadd(addr, a, b);
	f2i myaddf(addri, addr);
	fmul mymul(mulr, a, b);
	frecip myrecip(recr, a);
	fshift myshift(shir, a, f2ir);
	f2i myf2i(f2ir, a);
	f2i myib(i, b);
	f2i myiadd(j, addr);
	i2f myi2f(i2fr, f2ir);
	initial begin
	  $readmemh1(ref);
	  r = 0;

	  while (ref[r] != 0) begin
		a = ref[r]; b = ref[r+1];
		#1 $display("Testing (int)%x = %d, (int)%x = %d", a, ia, b, ib);
		if (addr != ref[r+2]) $display("%x + %x = %x # %x", a, b, addr, ref[r+2]);
		if (mulr != ref[r+3]) $display("%x * %x = %x # %x", a, b, mulr, ref[r+3]);
		if (recr != ref[r+4]) $display("1 / %x = %x # %x", a, recr, ref[r+4]);
		r = r + 5;
	  end
	end
endmodule