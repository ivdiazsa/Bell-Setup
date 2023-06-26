" This file contains an extension of Vim's default C# syntax containing special
" types in the CoreCLR codebase.

syn keyword csType     WORD DWORD
syn keyword csType     VOID PVOID LPVOID
syn keyword csType     INT32 UINT32 ULONG PULONG LONGLONG LONG ULONGLONG
syn keyword csType     LARGE_INTEGER
syn keyword csType     BYTE PBYTE
syn keyword csType     HANDLE BOOL STREAM_TYPE

syn keyword csType     String List Dictionary Object Tuple Func Action
syn keyword csType     Int32 Int64

syn keyword csModifier OPTIONAL Volatile WINAPI __declspec noinline __cdecl

syn keyword csUnspecifiedStatement DECLSPEC_ALIGN SVAL_IMPL SVAL_IMPL_INIT
syn keyword csUnspecifiedStatement SPTR_IMPL SVAL_DECL SPTR_DECL

