function summ takes integer a, integer b returns integer
	return a + b
endfunction

function subs takes integer a, integer b returns integer
	return a - b
endfunction

function square takes integer a returns integer
	return a * a
endfunction

function inc takes integer a returns integer
	return a + 1
endfunction

function incf takes real a returns real
	return a + 1
endfunction

function greater takes integer a, integer b returns boolean
	return a > b
endfunction

function greaterif takes integer a, real b returns boolean
	return a > b
endfunction

function fib takes integer n returns integer
	if n <= 1 then
		return 0
  elseif n == 2 then
  	return 1
  else
  	return fib(n-2) + fib(n-1)
  endif
endfunction

function fact takes integer n returns integer
	local integer prod = 1
	local integer i = 1
	loop
	  exitwhen i > n
		set prod = prod * i
		set i = i + 1
	endloop
	return prod
endfunction

function factAndFib takes integer n returns integer
	return fib(n) + fact(n)
endfunction