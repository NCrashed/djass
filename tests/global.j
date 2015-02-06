globals
	integer i = 42
	string j = "Hello!"
endglobals

function getGlobalI takes nothing returns integer
	return i
endfunction

function setGlobalI takes integer value returns nothing
	set i = value
endfunction

function getGlobalJ takes nothing returns string
	return j
endfunction