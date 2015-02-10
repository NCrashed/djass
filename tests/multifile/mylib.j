globals
	constant int SOME_CONSTANT = 42
endglobals

native writeln takes string msg returns nothing

function square takes integer n returns integer
	return n*n
endfunction