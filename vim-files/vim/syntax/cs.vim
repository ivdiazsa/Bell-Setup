" Extension to make the C# syntax highlighting more pleasant to look at!

syn match csIdentifier "\<\u\i*\>"
syn match csFunction "\w\+\ze\((\|<.*>(\)"

hi def link csFunction Function
hi def link csIdentifier Identifier
