// basic sizes of things
`define WORD	[15:0]
`define Opcode	[15:12]
`define Dest	[11:6]
`define Src	[5:0]
`define STATE	[4:0]
`define REGSIZE [63:0]
`define MEMSIZE [65535:0]

// opcode values, also state numbers
`define OPadd	4'b0000
`define OPinvf	4'b0001
`define OPaddf	4'b0010
`define OPmulf	4'b0011
`define OPand	4'b0100
`define OPor	4'b0101
`define OPxor	4'b0110
`define OPany	4'b0111
`define OPdup	4'b1000
`define OPshr	4'b1001
`define OPf2i	4'b1010
`define OPi2f	4'b1011
`define OPld	4'b1100
`define OPst	4'b1101
`define OPjzsz	4'b1110
`define OPli	4'b1111

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