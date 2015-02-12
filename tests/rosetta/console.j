native writeln takes string msg returns nothing
native write takes string msg returns nothing
native readln takes nothing returns string

native I2S takes integer i returns string
native S2I takes string s returns integer

native R2S takes real i returns string
native S2R takes string s returns real

native getRandInt takes integer low, integer high returns integer
native getRandReal takes real low, real high returns real
native getRandBool takes nothing returns boolean

native getStringLength takes string s returns integer
native getSubString takes string s, integer beg, integer count returns string
native getChar takes string s, integer i returns integer
native C2S takes integer i returns string
native S2C takes string s returns integer 

// function must takes nothing and return nothing
native execute takes string funcName returns nothing