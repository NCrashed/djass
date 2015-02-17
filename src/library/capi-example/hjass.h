/*
Copyright (c) 2015 NCrashed <ncrashed@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
#ifndef HJASS_HEADER
#define HJASS_HEADER

/// Descriptor of JASS program
typedef long hjassJassProgram;
/// Descriptor of running JASS module
typedef long hjassJITModule;

/// Returns last error of hjass API
/**
 * Returned string should be released by free
 */
extern char* hjassGetLastError();

/// Creates JASS program from list of modules names, modules sources and main module name
/**
 *	Loads JASS files from memory, parses and checks them and returns program identifier.
 *	@param[in] modulesNames   array of modules names
 *	@param[in] modulesSources array of modules source code
 *	@param[in] size           length of two previous arrays
 *	@param[in] mainModuleName name of main module, the name have to be presented at *modulesNames*
 *	@return program identifier, if return value is zero, then there was an error that could be read by *hjassGetLastError*
 *	@see hjassGetLastError, hjassFreeProgram
 */
extern hjassJassProgram hjassConstructProgram(const char** modulesNames, const char** modulesSources, long size
		, const char* mainModuleName);

/// Creates JASS program from file system folder
/**
 *	Loads JASS files from folder, parses and checks them and returns program identifier.
 *	@param[in] folderPath path to folder with JASS sources
 *	@param[in] mainPath   relative path from *folderPath* to main module (including extension)
 *  @return program identifier, if return value is zero, then there was an error that could be read by *hjassGetLastError*
 *  @see hjassGetLastError, hjassFreeProgram
 */
extern hjassJassProgram hjassConstructProgramFromSource(const char* folderPath, const char* mainPath);

/// Generic function pointer
/**
 * The type is used to store native pointers in homogeneous array.
 */
typedef void (*hjassGenericNative)();

/**
 * The callback type is used to create table with mapping "name of native"<->"function pointer".
 * @param[in] module        executing JASS model that could be helpful when reading info about JASS function references
 * @param[out] nativesNames output array of names of natives
 * @param[out] nativesPtrs  output array of native pointers (all function pointers are casted to *hjassGenericNative* type)
 * @param[out] error        if callback returns negative number, hjass will load error message from the parameter
 * @return size of *nativesNames* and *nativesPtrs* arrays, if the value is negative, then an error is occurred and the *error*
 * parameter should store string with a error message.
 */
typedef long (*hjassNativeMaker)(hjassJITModule module, char*** nativesNames, hjassGenericNative** nativesPtrs, char** error);

/**
 * The callback type is used to run action in context of executing JASS program
 * @param[in] module executing JASS model that is used to ran JASS functions
 * @param[out] error if the method returns zero output, then hjass loads error message from the parameter
 * @returns zero value is treated as an error, and a message is loaded from last parameter
 */
typedef long (*hjassExecutingCallback)(hjassJITModule module, char** error);

/// Executes JASS program with given set of natives and callback that is used to run JASS functions
/**
 * Loads JASS program into memory and starts execution. You can control the execution via JIT module reference and
 * by providing following callbacks.
 * @param[in] program           JASS program to execute
 * @param[in] nativeMaker       callback that is called before execution and loads native table
 * @param[in] executingCallback callback that is called after JIT process begin, here user calls JASS functions
 * @return program identifier, if return value is zero, then there was an error that could be read by *hjassGetLastError*
 */
extern long hjassExecuteProgram(hjassJassProgram program, hjassNativeMaker nativeMaker
		, hjassExecutingCallback executingCallback);

/// Returns function pointer in JASS program, user should know actual return type and parameters types of returned function
/**
 * Loads JASS function pointer from JIT module and stores the pointer in last parameter.
 * @param[in] jit      JASS executing module
 * @param[in] funcName name of JASS function in *jit* module
 * @param[out] fptr    where to store function pointer, isn't set if an error is occured
 * @return zero value is threated as an error, and a message could be read by *hjassGetLastError*, *fptr* parameter isn't set 
 * if there is an error.
 */
extern long hjassGetJassFuncPtr(hjassJITModule jit, const char* funcName, hjassGenericNative* fptr);

#endif
