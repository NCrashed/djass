import console

globals
  integer array numbers
  integer numbers_count = 0
  string parseError = ""
  string inputReminder = ""
endglobals

function generateNumbers takes integer i returns nothing
  call writeln("Getting " + I2S(i) + " random numbers")
  set numbers_count = i
  call write("Numbers: ")
  set i = 0
  loop exitwhen i >= numbers_count
    set numbers[i] = getRandInt(1,9)
    call write(I2S(numbers[i]) + " ") 
    set i = i + 1
  endloop
  call writeln("")
  call writeln("Now make a equation with them to get 24!")
endfunction

function parseParens takes string s returns integer
  set parseError = ""
  return parseAdditive(s)
endfunction

function isInGenerated takes integer i returns boolean
  local integer j = 0
  loop exitwhen j >= numbers_count
    if numbers[j] == i then
      return true
    endif
  set j = j + 1
  endloop
  return false
endfunction

function isValidNumber takes integer i returns boolean
  local integer dig = 0
  if     i == S2C("0") or i == S2C("1") or i == S2C("2") 
      or i == S2C("3") or i == S2C("4") or i == S2C("5")
      or i == S2C("6") or i == S2C("7") or i == S2C("8") 
      or i == S2C("9") then
    set dig = S2I(C2S(i))
    return isInGenerated(dig)
  else
    return false
  endif
endfunction

function parsePostfix takes string s returns integer
  // postfix <- '(' multiplicative ')' | number
  local integer tok = S2C(s)
  local integer val
  if tok == S2C("(") then
    set inputReminder = getSubString(s, 1, getStringLength(s)-1)
    set val = parseParens(inputReminder)
    if getStringLength(inputReminder) == 0 then
      set parseError = "Expected ), but got end of input"
      return 0
    endif
    set tok = S2C(inputReminder)
    if tok == S2C(")") then
      set inputReminder = getSubString(inputReminder, 1, getStringLength(inputReminder)-1)
      return val
    else
      set parseError = "Expected ), but got " + C2S(tok)
      return 0
    endif
  elseif isValidNumber(tok) then
    set inputReminder = getSubString(s, 1, getStringLength(s)-1) 
    return S2I(C2S(tok))
  else 
    set parseError = "Unexpected " + C2S(tok) + ", expecting parens or one of valid digits"
    return 0
  endif
endfunction

function parseUnary takes string s returns integer
  // unary <- ('-' | '+') unary | postfix
  local integer left
  local integer right
  local integer tok
  local integer l
  
  set tok = S2C(s)
  if tok == S2C("+") then
    set inputReminder = getSubString(s, 1, getStringLength(s)-1)
    return parseUnary(inputReminder)
  elseif tok == S2C("-") then
    set inputReminder = getSubString(s, 1, getStringLength(s)-1)
    return -parseUnary(inputReminder)
  else 
    return parsePostfix(s)
  endif
endfunction

function parseAdditive takes string s returns integer
  // additive <- multiplicative | multiplicative ('+' | '-') additive
  local integer left
  local integer right
  local integer tok
  local integer l
  set left = parseMultiplicative(s)
  set s = inputReminder
  set l = getStringLength(s)
  if l == 0 then
    return left
  else
    set tok = S2C(s)
    if tok == S2C("+") then
      set inputReminder = getSubString(s, 1, l-1)
      set right = parseAdditive(inputReminder)
      return left+right
    elseif tok == S2C("-") then
      set inputReminder = getSubString(s, 1, l-1)
      set right = parseAdditive(inputReminder)
      return left-right
    else
      return left
    endif
  endif
endfunction

function parseMultiplicative takes string s returns integer
  // multiplicative <- unary | unary ('*' | '/') multiplicative
  local integer left
  local integer right
  local integer tok
  local integer l
  set left = parseUnary(s)
  set s = inputReminder
  set l = getStringLength(s)
  if l == 0 then
    return left
  else
    set tok = S2C(s)
    if tok == S2C("*") then
      set inputReminder = getSubString(s, 1, l-1)
      set right = parseMultiplicative(inputReminder)
      return left*right
    elseif tok == S2C("/") then
      set inputReminder = getSubString(s, 1, l-1)
      set right = parseMultiplicative(inputReminder)
      return left/right
    else
      //set parseError = "Expected * or / but got " + C2S(tok)
      return left
    endif
  endif
endfunction

function printValidNumbers takes nothing returns nothing
  local integer i = 0
  call write("Valid numbers: ")
  set i = 0
  loop exitwhen i >= numbers_count
    call write(I2S(numbers[i]) + " ") 
    set i = i + 1
  endloop
  call writeln("")
endfunction

function play takes nothing returns nothing
  local integer res = 0
  local string input
  
  call writeln("Your expression (type nothing to exit): ")
  set input = readln()
  if(input == "") then
    return
  endif
  
  set parseError = ""
  set inputReminder = ""
  set res = parseAdditive(input)
  if parseError != "" then
    call writeln(parseError)
    call printValidNumbers()
    call play()
    return
  endif
  if res == 24 then
    call writeln("Yahoo! You win. Bye")
  else 
    call writeln("Your answer is " + I2S(res) + ", not 24. Try again")
    call printValidNumbers()
    call play()
    return
  endif
endfunction

function main takes nothing returns nothing
  call generateNumbers(4)
  call play()
endfunction