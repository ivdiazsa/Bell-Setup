set runtimepath^=~/.vim
let &packpath = &runtimepath
source ~/.vimrc

luafile ~/.config/nvim/load-nvim-tree.lua
