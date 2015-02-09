globals
	integer a
	real b
endglobals

native setCallback takes code callback returns nothing

function getA takes nothing returns integer
	return a
endfunction

function getB takes nothing returns real
	return b
endfunction

function myCallback takes integer aVal, real bVal returns nothing
	set a = aVal
	set b = bVal
endfunction

function main takes nothing returns nothing
	call setCallback(function myCallback)
endfunction