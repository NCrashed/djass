function testStrings takes string a returns string
	if a == "Hello, " then
		return a + "World!"
	elseif a == "Bye, " then
		return a + "Dude!"
	endif
	return "I don't know that input!"
endfunction