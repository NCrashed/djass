globals
	real array a
endglobals

function setA takes integer i, real value returns nothing
	set a[i] = value
endfunction

function getA takes integer i returns real
  return a[i]
endfunction

function testLocalArray takes integer n returns integer
	local integer array b
	local integer i = 0
	local integer sum = 0

	loop
	exitwhen i >= n
		set b[i] = i
		set i = i + 1
	endloop
	set i = 0
	loop
	exitwhen i >= n
	  set sum = sum + b[i]
		set i = i+1
	endloop
	return sum
endfunction