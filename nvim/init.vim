" Bring our so beloved Vim's configuration to Neovim.
set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

" Load Nvim-Dap and Nvim-Dap-UI
luafile ~/.config/nvim/dap.lua

" Key mappings for Nvim-Dap.
nnoremap <silent> <F5> <Cmd>lua require'dap'.continue()<CR>
nnoremap <silent> <F10> <Cmd>lua require'dap'.step_over()<CR>
nnoremap <silent> <F11> <Cmd>lua require'dap'.step_into()<CR>
nnoremap <silent> <F12> <Cmd>lua require'dap'.step_out()<CR>
nnoremap <silent> <Leader>b <Cmd>lua require'dap'.toggle_breakpoint()<CR>
nnoremap <silent> <Leader>ro <Cmd>lua require'dap'.continue()<CR>
nnoremap <silent> <Leader>rl <Cmd>lua require'dap'.continue()<CR>

