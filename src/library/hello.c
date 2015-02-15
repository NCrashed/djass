#include <stdio.h>
#include "hjass.h"

void nativeWriteln(char* msg)
{
	printf("%s", msg);
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
	*nativesNames = nativesNamesArr;
	*nativesPtrs = nativesNamesArr;
	return NATIVES_COUNT;
}

long execJass(hjassJITModule module, char** error)
{
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
