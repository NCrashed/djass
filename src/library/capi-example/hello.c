#include <stdio.h>
#include "hjass.h"

void nativeWriteln(char* msg)
{
	printf("%s\n", msg);
}

#define NATIVES_COUNT 1

char* nativesNamesArr[NATIVES_COUNT] = {
	"writeln"
};

hjassGenericNative nativesPtrArr[NATIVES_COUNT] = {
	(hjassGenericNative)&nativeWriteln
};

long nativeMaker(hjassJITModule module, char*** nativesNames, hjassGenericNative** nativesPtrs, char** error)
{
	*nativesNames = &nativesNamesArr[0];
	*nativesPtrs = &nativesPtrArr[0];
	return NATIVES_COUNT;
}

long execJass(hjassJITModule module, char** error)
{
	hjassGenericNative ptr;
	long res = hjassGetJassFuncPtr(module, "main", &ptr);
	if(!res) {
		*error = hjassGetLastError();
		return 0;
	}

	ptr();
	return 1;
}

int main(int argc, char* argv[])
{
	hjassJassProgram prog = hjassConstructProgramFromSource("./jass", "hello.j");
	if(!prog)
	{
		printf("%s\n", hjassGetLastError());
		return 1;
	}

	long res = hjassExecuteProgram(prog, &nativeMaker, &execJass);
	if(!res)
	{
		printf("%s\n", hjassGetLastError());
		return 1;
	}

	return 0;
}
