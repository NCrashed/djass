module samples.multithreaded;

import llvm.all;
import std.stdio;

void main(string[] args)
{
    static if(LLVM_Version >= 3.3)
    {
        writefln("LLVM multithreading on? %s", cast(bool) LLVMIsMultithreaded());
        writefln("Turning it on"); LLVMStartMultithreaded();
        writefln("LLVM multithreading on? %s", cast(bool) LLVMIsMultithreaded());
        writefln("Turning it off"); LLVMStopMultithreaded();
        writefln("LLVM multithreading on? %s", cast(bool) LLVMIsMultithreaded());
    }
}