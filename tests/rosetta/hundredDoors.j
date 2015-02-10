import console

globals
  boolean array doors
  integer i
  integer j
endglobals

function main takes nothing returns nothing
  loop exitwhen i >= 100
    set doors[i] = false
    set i = i + 1
  endloop
  set i = 0
  loop exitwhen i >= 100
    set j = i
    loop exitwhen j >= 100
      set doors[j] = not doors[j]
      set j = j + 1
    endloop
  endloop
  set i = 0
  loop exitwhen i >= 100
    call writeln(I2S(i) + " ")
    if doors[i] then
      call writeln("open")
    else
      call writeln("closed")
    endif
  endloop
endfunction