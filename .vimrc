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

set ruler
set autoread
set backspace=indent,eol,start
set tabstop=4 softtabstop=0 noexpandtab shiftwidth=4
set hlsearch incsearch
set modelines=50
filetype plugin on

highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%>72v.\+/
