" Configure Vundle
filetype off
set nocompatible
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

" Vundle plugins
call vundle#end()
"=========================================

execute pathogen#infect()
syntax on

set number
set autoread
set backspace=indent,eol,start
set tabstop=4 softtabstop=0 noexpandtab shiftwidth=4
set hlsearch
set ignorecase
set incsearch
set modelines=50
set showmatch

highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%>72v.\+/


"=========================================
" Status line: Format
set laststatus=2
set statusline=%<%f\ -   " Filename
set statusline+=\ %l:%c  " Line:column
set statusline+=%=       " Left/right divider
set statusline+=%y       " Filetype

" Status line: Colours
highlight statusline ctermbg=DarkGreen ctermfg=Black
