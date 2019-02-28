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