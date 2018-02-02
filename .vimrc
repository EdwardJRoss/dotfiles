" execute pathogen#infect()

set nocompatible

" Change cursor shape on xterm
if len(&term) > 0 && split(&term, '-')[0] == "xterm"
    " '\x1b[\x30 q' # changes to blinking block
    " '\x1b[\x31 q' # changes to blinking block also
    " '\x1b[\x32 q' # changes to steady block
    " '\x1b[\x33 q' # changes to blinking underline
    " '\x1b[\x34 q' # changes to steady underline
    " '\x1b[\x35 q' # changes to blinking bar
    " '\x1b[\x36 q' # changes to steady bar
    let &t_ti.="\e[1 q"
    let &t_SI.="\e[5 q"
    let &t_EI.="\e[1 q"
    let &t_te.="\e[0 q"
endif

set ruler
set incsearch
" Map F4 to toggle search highlighting
map <silent> <F4> :set hls! <CR>

set autoindent
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4

set modeline
set relativenumber

set autochdir


" Custom key mappings
noremap , <C-W>
nnoremap ;w :w<CR>
nnoremap ;x :x<CR>
nnoremap ;q :q<CR>

" Leader
let mapleader = " "
nnoremap <Leader>f :e
nnoremap <Leader>e :e
nnoremap <Leader>b :b
nnoremap <Leader>w :w<CR>
nnoremap <Leader>x :
nnoremap <Leader>q :q<CR>
nnoremap <Leader>j <C-W>j
nnoremap <Leader>h <C-W>h
nnoremap <Leader>k <C-W>k
nnoremap <Leader>l <C-W>l
nnoremap <Leader>H <C-W>v
nnoremap <Leader>L <C-W>v
nnoremap <Leader>v <C-W>v
nnoremap <Leader>K <C-W>s
nnoremap <Leader>J <C-W>s
nnoremap <Leader>s <C-W>s
nnoremap <Leader>o <C-W>o
nnoremap <Leader>+ <C-W>+
nnoremap <Leader>- <C-W>-
nnoremap <Leader>< <C-W><
nnoremap <Leader>> <C-W>>
nnoremap <Leader>= <C-W>=
nnoremap <Leader>b :b<CR>
nnoremap <Leader>B :bp<CR>

" Where am I?
nnoremap <F1> :echo expand('%:p')<CR>

set directory=$HOME/.backup,$HOME/tmp,$HOME
set backupdir=$HOME/.backup,$HOME/tmp,$HOME

" Highlight searches
set hlsearch
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>


" Highlight whitespace that might be bad
:highlight SuspiciousWhitespace ctermbg=red guibg=red
:autocmd ColorScheme * highlight SuspiciousWhitespace ctermbg=red guibg=red
" Highlight returns, tabs, and whitespace to end of line (unless we are
" editing the line)
:match SuspiciousWhitespace /\r\|\t\|\s\+\%#\@<!$/
:autocmd InsertLeave * redraw!
colorscheme desert

syntax on
filetype indent plugin on

" Allow an insert mode escape
" I never actually used/rememered this
" inoremap jj <ESC>

" Can use backspace to delete back past start of "a"ppend, etc.
set backspace=indent,eol,start

set laststatus=2
set statusline=
set statusline +=%n\                            "buffer number
set statusline+=%f                              "tail of filename
set statusline +=%m                             "modified flag
set statusline+=[%{strlen(&fenc)?&fenc:'none'}], "file encoding
" set statusline +=%{fugitive#statusline()}     "git status
set statusline+=%=                              "left right separator
set statusline +=%l                             "current line
set statusline +=/%L                            "total lines
set statusline +=%4v\                           "virtual column number
set statusline +=0x%04B\                        "character under cursor

" More annoying than useful
" set cc=80
