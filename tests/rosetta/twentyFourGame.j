import console

globals
  integer array numbers
  integer numbers_count = 0
endglobals

function generateNumbers takes integer i returns nothing
  call writeln("Getting " + I2S(i) + " random numbers")
  set numbers_count = i
  call write("Numbers: ")
  loop exitwhen i <= 0
    set numbers[i-1] = getRandInt(1,9)
    call write(I2S(numbers[i-1]) + " ") 
    set i = i - 1
  endloop
  call writeln("")
  call writeln("Now make a equation with them to get 24!")
endfunction

function main takes nothing returns nothing
  call generateNumbers(4)
  
endfunction
