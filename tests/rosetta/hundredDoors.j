import console

globals
  boolean array doors
  integer i
  integer j
endglobals

function main takes nothing returns nothing
  set i = 0
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
    set i = i + 1
  endloop
  set i = 0
  loop exitwhen i >= 100
    call write(I2S(i) + " ")
    if doors[i] then
      call writeln("open")
    else
      call writeln("closed")
    endif
    set i = i + 1
  endloop
endfunction