set runtimepath^=~/.vim
let &packpath = &runtimepath
source ~/.vimrc

" This helps the terminal colors be rendered properly when in GUI.
set termguicolors

" For the Nvim-Tree plugin to render the icons properly, we need one of the
" patched special fonts.
set guifont=UbuntuMono_Nerd_Font:h13

" Neovim seems to need the terminal colors configuration like this, instead of
" the usual 'g:terminal_ansi_colors' in Vim.
let g:terminal_color_1 = '#EF2929'
let g:terminal_color_2 = '#8AE234'
let g:terminal_color_3 = '#FCE94F'
let g:terminal_color_4 = '#729FCF'
let g:terminal_color_5 = '#AD7FA8'
let g:terminal_color_6 = '#34E2E2'
let g:terminal_color_7 = '#EEEEEC'

luafile ~/.config/nvim/load-nvim-plugins.lua
