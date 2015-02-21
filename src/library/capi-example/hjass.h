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
/// Descriptor of JASS callback
typedef long hjassJassCode;
/// Descriptor of JASS type
typedef long hjassType;

/// Returns last error of hjass API
/**
 * Returned string should NOT be released
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
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*, *fptr* parameter isn't set
 * if there is an error.
 */
extern long hjassGetJassFuncPtr(hjassJITModule jit, const char* funcName, hjassGenericNative* fptr);

/// Reads jass reference inner structure into hjassCode object
/**
 * @param[in] jit JASS executing module
 * @param[in] ptr pointer to return type of type `code` from jass function
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*
 */
extern hjassJassCode hjassLiftCode(hjassJITModule jit, void* ptr);

/// Frees inner hjassCode object
/**
 * @param[in] ref Reference to code reference that was got by hjassLiftCode
 */
extern void hjassFreeCode(hjassJassCode ref);

/// Returns callback function pointer
/**
 * @param[in] ref   reference to JASS code callback
 * @param[out] fptr here result is stored, pointer to callback function of code reference
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*,
 * 	*fptr* isn't set if there is an error.
 */
extern long hjassCodeFunctionPtr(hjassJassCode ref, hjassGenericNative* fptr);

/// Returns result type of JASS callback
/**
 * @param[in] jit reference to JASS executing module
 * @param[in] ref reference to JASS callback
 * @param[out] retptr here result is stored, JASS type reference (zero is special value for void return type)
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*,
 * 	*retptr* isn't set if there is an error.
 */
extern long hjassCodeReturnType(hjassJITModule jit, hjassJassCode ref, hjassType* retptr);

/// Returns arguments count of JASS callback
/**
 * @param[in] ref     reference to JASS callback
 * @param[out] retptr here result is stored, number of JASS callback arguments
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*,
 * 	*retptr* isn't set if there is an error.
 */
extern long hjassCodeArgumentCount(hjassJassCode ref, long* retptr);

/// Returns argument type reference
/**
 * @param[in] jit   reference to JASS executing module
 * @param[in] ref   reference to JASS callback
 * @param[in] index zero based index of a parameter
 * @param[out] retptr here result is stored, reference of JASS type
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*,
 * 	*retptr* isn't set if there is an error.
 */
extern long hjassCodeGetArgument(hjassJITModule jit, hjassJassCode ref, long index, hjassType* retptr);

/// Returns void type
/**
 * @param[in] jit	  reference to JASS executing module
 * @param[out] retptr where to store result, requested type reference
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*
 */
extern long hjassVoid(hjassJITModule jit, hjassType* retptr);

/// Returns integer type
/**
 * @param[in] jit	  reference to JASS executing module
 * @param[out] retptr where to store result, requested type reference
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*
 */
extern long hjassInteger(hjassJITModule jit, hjassType* retptr);

/// Returns real type
/**
 * @param[in] jit	  reference to JASS executing module
 * @param[out] retptr where to store result, requested type reference
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*
 */
extern long hjassReal(hjassJITModule jit, hjassType* retptr);

/// Returns boolean type
/**
 * @param[in] jit	  reference to JASS executing module
 * @param[out] retptr where to store result, requested type reference
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*
 */
extern long hjassBoolean(hjassJITModule jit, hjassType* retptr);

/// Returns string type
/**
 * @param[in] jit	  reference to JASS executing module
 * @param[out] retptr where to store result, requested type reference
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*
 */
extern long hjassString(hjassJITModule jit, hjassType* retptr);

/// Returns handle type
/**
 * @param[in] jit	  reference to JASS executing module
 * @param[out] retptr where to store result, requested type reference
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*
 */
extern long hjassHandle(hjassJITModule jit, hjassType* retptr);

/// Returns code (callback) type
/**
 * @param[in] jit	  reference to JASS executing module
 * @param[out] retptr where to store result, requested type reference
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*
 */
extern long hjassCode(hjassJITModule jit, hjassType* retptr);

/// Returns user defined type (based on one of basic types)
/**
 * @param[in] jit	  reference to JASS executing module
 * @param[in] name    name of custom type
 * @param[out] retptr where to store result, requested type reference
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*
 */
extern long hjassUserDefinedType(hjassJITModule jit, const char* name, hjassType* retptr);

/// Returns string representation of JASS type
/**
 * @param[in] jit     reference to JASS executing module
 * @param[in] type    reference to JASS type
 * @param[out] result where to store result, string (name) of JASS type
 * @return zero value is treated as an error, and a message could be read by *hjassGetLastError*
 * @note You should free resulted string by yourself
 */
extern long hjassPrintType(hjassJITModule jit, hjassType type, char** result);

#endif
