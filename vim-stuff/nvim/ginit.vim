" Enable Mouse

" Set Neovim-qt's Font
if exists(':GuiFont')
    GuiFont! Ubuntu Mono derivative Powerline:h15
endif

" Disable GUI Tabline
if exists(':GuiTabline')
    GuiTabline 0
endif

" Right Click Context Menu (Copy-Cut-Paste)
nnoremap <silent><RightMouse> :call GuiShowContextMenu()<CR>
inoremap <silent><RightMouse> <Esc>:call GuiShowContextMenu()<CR>
xnoremap <silent><RightMouse> :call GuiShowContextMenu()<CR>gv
snoremap <silent><RightMouse> <C-G>:call GuiShowContextMenu()<CR>gv
