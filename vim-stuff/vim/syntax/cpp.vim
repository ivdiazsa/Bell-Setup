" This file contains an extension of Vim's default C++ syntax containing special
" types in the CoreCLR codebase.

syn keyword cppType WORD DWORD DWORD_PTR PDWORD DWORD64
syn keyword cppType VOID PVOID LPVOID TYPE PTR_VOID
syn keyword cppType INT32 UINT32 ULONG PULONG LONGLONG LONG ULONGLONG
syn keyword cppType LARGE_INTEGER UINT ULONG64 ULONG_PTR UINT8
syn keyword cppType BYTE PBYTE PTR_BYTE
syn keyword cppType WCHAR LPWSTR LPCWSTR
syn keyword cppType PTRARRAYREF OBJECTREF PTR_PTR_Object
syn keyword cppType HANDLE BOOL HKEY HRESULT PCODE ADDR TADDR PTR_PCODE
syn keyword cppType STARTUPINFO PROCESS_INFORMATION
syn keyword cppType pid_t

syn keyword cppModifier OPTIONAL Volatile WINAPI __declspec noinline __cdecl
syn keyword cppModifier nothrow DECLSPEC_NORETURN
syn keyword cppBoolean  FALSE TRUE

syn keyword cppStatement printf strcmp memset fputs puts malloc calloc min max
syn keyword cppStatement dac_cast static_cast dynamic_cast
syn keyword cppStatement DECLSPEC_ALIGN _ASSERTE assert
syn keyword cppStatement SVAL_DECL SPTR_DECL SVAL_IMPL SVAL_IMPL_INIT SPTR_IMPL

