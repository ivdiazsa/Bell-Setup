" Show line numbers
set number

" Enable mouse usage
set mouse=a

" Have Vim show matching parentheses, brackets, etc, when typing
set showmatch

" Highlight searched text
set hlsearch

" Display incomplete commands
set showcmd

" Display completion matches in status line
set wildmenu

" Show the cursor position at all times
set ruler

" Search as you type
set incsearch

" Switch tabs to spaces and adjust the size to 2
set expandtab
set tabstop=4
set shiftwidth=4

" Set Encoding to UTF-8
set encoding=utf-8

" Terminal configuration.
syntax on

" Unrestrict Windows Backspace allowance configuration
set backspace=indent,eol,start

" Highlight current line number
set cursorline
set cursorlineopt=number

" Neovim doesn't like this for some reason
if !has('nvim')
    set cursorlineopt=number
endif

" GVim Settings (Vim-only)
if !has('nvim') && has('gui_running')
    set guifont=Ubuntu\ Mono\ derivative\ Powerline\ Regular\ 12.5
    set guioptions-=T
    set guioptions-=r
    set guioptions-=m
endif

" Blinking cursor is prettier, but Vim and Neovim configure it differently
if !has('nvim')
    set guicursor-=a:blinkon0
endif

" Syntax Highlighting Theme (Default to 'Luna' with GUI and 'Monokain' in Terminal)
if has("gui_running") || has('nvim')
    colorscheme luna
else
    colorscheme monokain
endif

" Certain file extensions share syntax highlighting with other more known ones
augroup lesser_known_file_extensions
    autocmd!
    autocmd BufNewFile,BufRead *.*proj   set filetype=xml | set tabstop=2 | set shiftwidth=2
    autocmd BufNewFile,BufRead *.props   set filetype=xml | set tabstop=2 | set shiftwidth=2
    autocmd BufNewFile,BufRead *.targets set filetype=xml | set tabstop=2 | set shiftwidth=2
augroup END

" Enable Airline Plugin and default theme
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'luna'
let g:airline_section_z = '%3p%% %#__accent_bold#%{g:airline_symbols.linenr}%#__accent_bold#%4l/%L:%#__restore__#%3v'

" Bug fix for Airline not showing on first window
set laststatus=2

" Key maps to make life easier :)
:map <S-H> <C-W><
:map <S-J> <C-W>-
:map <S-K> <C-W>+
:map <S-L> <C-W>>

" Commands I use often
:map <M-z> :set wrap! <Enter>
:map <M-S-z> :pclose <Enter>
:noremap <Leader><F7> :set relativenumber! <Enter>
:map <M-g> :vertical Git <Enter>

" LVim Commands
:map <M-n> :lnext <Enter>
:map <M-p> :lprevious <Enter>
:map <M-o> :lopen <Enter>
:map <M-c> :lclose <Enter>

" Quick Fix Commands
:map <M-N> :cnext <Enter>
:map <M-P> :cprevious <Enter>
:map <M-O> :copen <Enter>
:map <M-C> :cclose <Enter>

" Configure netrw to be as good as (or even better than) the NERDtree plugin
:map <F4> :Vexplore <Enter>
let g:netrw_liststyle=3
let g:netrw_browse_split=4
let g:netrw_winsize=15
let g:netrw_preview=1

" Open/Close Tagbar easily
:map <F8> :TagbarToggle <Enter>

" Neovim doesn't like this either for some reason
if !has('nvim')
    let g:netrw_altv=&sn
endif

" Configure clang_complete plugin
let g:clang_library_path='/usr/lib/llvm-10/lib/libclang-10.so.1'
let g:clang_close_preview=1

" Configure omnisharp-vim plugin
filetype indent plugin on
syntax enable
let g:OmniSharp_server_use_net6=1

" Configure vim-cpp-modern plugin for C/C++ semantic highlighting
let g:cpp_attributes_highlight=1
let g:cpp_member_highlight=1

" Add additional configuration to Tagbar
let g:tagbar_autofocus=1
let g:tagbar_show_data_type=1
let g:tagbar_show_tag_linenumbers=2

" Implementing my own 'Toggle Comment' functionality :)
augroup toggle_comment
    autocmd!
    autocmd FileType c,cpp,cs let b:comment_leader = '// '
    autocmd FileType ruby,yaml,python let b:comment_leader = '# '
    autocmd FileType bash,conf,sh,tmux let b:comment_leader = '# '
    autocmd FileType vim let b:comment_leader = '" '
augroup END

noremap <silent> ,cc :<C-b>silent <C-e>norm ^i<C-r>=b:comment_leader<CR><CR>
noremap <silent> ,uc :<C-b>silent <C-e>norm ^2x<CR>

