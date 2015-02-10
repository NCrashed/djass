globals
	constant integer SOME_CONSTANT = 42
endglobals

native writeln takes string msg returns nothing
native I2S takes integer i returns string

function square takes integer n returns integer
	return n*n
endfunction