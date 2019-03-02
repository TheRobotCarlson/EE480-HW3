// basic sizes of things 
`define WORD	 [15:0]
`define Opcode	 [15:11]
`define Opcode2  [7:3]
`define Reg		 [11:9]
`define Reg2	 [2:0]
`define Imm		 [7:0]
`define STATE	 [5:0]
`define HALFWORD [7:0]

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


module phase_1_alu(regfile, mainmem, pc, clk, opcode, regloc);
	parameter ACC = 0;
	input reg pre `HALFWORD;
	input reg `REGWORD regfile `REGSIZE;
	input reg `WORD mainmem `MEMSIZE;
	input reg `WORD pc = 0;

		// Float module destinations
	reg `WORD f_add;
	reg `WORD f_sub;
	reg `WORD f_mul;
	reg `WORD f_recip;
	reg `WORD f_shift;
	reg `WORD f_div;
	reg `WORD f_f2ir;
	reg `WORD f_i2f;

	// Float module instantiation
	// add
	fadd fadd_inst(f_add, regfile[ACC] `WORD, regfile[regloc] `WORD);

	// sub (flip register value's MSB to change sign and then add)
	fadd fsub_inst(f_sub, regfile[ACC] `WORD, (regfile[regloc] `WORD)^16`h8000);

	// mul
	fmul fmul_inst(f_mul, regfile[ACC] `WORD, regfile[regloc] `WORD);

	// div (recip then mul)
	frecip frecip_inst(f_recip, regfile[ACC] `WORD);
	fmul fdiv_inst(f_div, f_recip, regfile[regloc] `WORD);

	// shift
	fshift fshift_inst(f_shift, regfile[ACC] `WORD, regfile[regloc] `WORD);
	// f2i
	f2i ff2i_inst(f_f2i, regfile[regloc] `WORD);
	// i2f
	f2i fi2f_inst(f_i2f, regfile[regloc] `WORD);

	always @(posedge trigger) begin
		if(5'b10000 > opcode) begin // phase 1 decoding
			case (opcode)
				`OPa2r: begin regfile[regloc] <= regfile[0]; end
				`OPr2a: begin regfile[0] <= regfile[regloc]; end
				`OPjr: begin pc <= regfile[regloc] `WORD; end
				`OPst: begin mainmem[regfile[regloc] `WORD ] <= regfile[0]; end //to check
				`OPlf: begin regfile[regloc] <= {1, mainmem[pc]}; end // to check, set 1 for float
				`OPli: begin regfile[regloc] <= {0, mainmem[pc]}; end //to check PC increment, set 0 for int
				// ALU
				`OPcvt: begin
					regfile[0] `WORD <= regfile[regloc][16] ? f_f2i : f_i2f;
					regfile[0][16] <= regfile[0][16]^1`b1; // Flip register type
				end
				`OPslt: 
				`OPsh:  begin regfile[0] `WORD <= regfile[0][16] ? f_shift : regfile[0]<<regfile[regloc]; end
				`OPadd: begin regfile[0] `WORD <= regfile[0][16] ? f_add : regfile[0]+regfile[regloc]; end
				`OPsub:	begin regfile[0] `WORD <= regfile[0][16] ? f_sub : regfile[0]-regfile[regloc]; end
				`OPmul: begin regfile[0] `WORD <= regfile[0][16] ? f_mul : regfile[0]*regfile[regloc]; end
				`OPdiv: begin regfile[0] `WORD <= regfile[0][16] ? f_div : regfile[0]/regfile[regloc]; end
				`OPnot: begin regfile[0] `WORD <= ~(regfile[regloc]); end
				`OPxor: begin regfile[0] `WORD <= regfile[0] ^ regfile[regloc]; end
				`OPand: begin regfile[0] `WORD <= regfile[0] & regfile[regloc]; end
				`OPor:  begin regfile[0] `WORD <= regfile[0] | regfile[regloc]; end
			endcase
		end
	end
	
endmodule

module processor(halt, reset, clk);
	output reg halt;
	input reset, clk;

	reg pre `HALFWORD;
	reg `REGWORD regfile `REGSIZE;
	reg `WORD mainmem `MEMSIZE;
	reg `WORD pc = 0;
	reg `WORD ir;
	reg `STATE s = `Fetch;
	reg [2:0] reg1, reg2;
	reg [4:0] opcode1, opcode2;

	// Float module instantiation
	phase_1_alu #(0) alu_l(regfile, mainmem, pc, clk, opcode1, reg1);
	phase_1_alu #(1) alu_r(regfile, mainmem, pc, clk, opcode2, reg2);

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
				opcode1 = ir `Opcode;
				opcode2 = ir `Opcode2;
				reg1 = ir `reg1;
				reg2 = ir `reg2;
				if(5'b10000 < opcode1) begin  // phase 2 decoding
					case (ir `Opcode)
						`OPpre: begin pre <= ir `Imm; end
						`OPjp8: begin pc <= {pre, ir `Imm}; end
						`OPjnz8: begin if (ir `Reg != 0) pc <= {pre, ir `Imm}; end
						`OPjz8: begin if (ir `Reg == 0) pc <= {pre, ir `Imm}; end
						`OPcf8: begin regfile[ir `Reg] = {1, pre, ir `Imm}; end
						`OPci8: begin regfile[ir `Reg] = {0, pre, ir `Imm}; end
						`OPsys: begin halt <= 1; end
					endcase
				end
				s <= `Fetch;
			end
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
