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

- double wide - op r0, op r1 
a2r 
r2a
 
lf -  $r = memory[$acc]
li 

st 
cvt 
sh 
slt 
add 
sub 
mul 
div 
not 
xor 
and 
or 

jr  


pre jp8 sys  