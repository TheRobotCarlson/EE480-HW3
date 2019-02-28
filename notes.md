## TACKY design notes
- registers are 17 bits - 1 bit for a float flag, 16 bit word
- two accumulators - only applicable for two instruction words. slot 0 uses acc0 (aka r0), slot 1 uses acc1 (aka r1)
	- these instructions happen simultaneously
	- add $5, a2r $0 <- ignore duel writes like this 

	$5	$ra	return address for simple functions

- special registers
	- $0	$r0	accumulator for slot 0 instructions
	- $1	$r1	accumulator for slot 1 instructions
	- $5	$ra	return address for simple functions
	- $6	$rv	return value
	- $7	$sp	stack pointer (there is no frame pointer)

while not halt:	
	loading instruction
	
	
	decode
	act on code
	memory

	
processor
	always(clk)
		switch(s)
			`Fetch: ir <= mainmem[pc]; s <= `Execute;
			`Execute:
				pc <= pc + 1;
				if op < b10000: PHASE 1
					switch(op1)
						cases
					switch(op2)
						cases
						
				else: PHASE 2
					switch(op)
						cases
	

- double wide - op r0, op r1 - PHASE 1 DECODING

	$r = $acc <--> $acc = $r
		a2r 
		r2a

	$r = memory[$acc], bit for float
		lf  
		li 
		
	pc = $r
		jr
	
	memory[$r] = $acc
		st

	int to float, float to int	
		cvt 

	ALU ops
		$acc += $r
			add
			
		$acc -= $r
			sub

		$acc *= $r
			mul

		$acc /= $r	
			div

		$acc = (~$r)	
			not

		$acc ^= $r
			xor
		
		$acc &= $r
			and
			
		$acc |= $r
			or 
		
		$acc = shift($acc,$r) where $r holds an int	
			sh 

		$acc = ($acc<$r)
			slt


- PHASE 2 DECODING		
	pre - 8 bit register

	$r = {pre, imm8}
	cf8 
	ci8 

	jump ops
		jnz8
		jz8
		jp8 

	sys
