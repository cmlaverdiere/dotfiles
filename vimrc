" All system-wide defaults are set in $VIMRUNTIME/archlinux.vim (usually just
" /usr/share/vim/vimfiles/archlinux.vim) and sourced by the call to :runtime
" you can find below.  If you wish to change any of those settings, you should
" do it in this file (/etc/vimrc), since archlinux.vim will be overwritten
" everytime an upgrade of the vim packages is performed.  It is recommended to
" make changes after sourcing archlinux.vim since it alters the value of the
" 'compatible' option.

" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages.
runtime! archlinux.vim

" If you prefer the old-style vim functionalty, add 'runtime! vimrc_example.vim'
" Or better yet, read /usr/share/vim/vim74/vimrc_example.vim or the vim manual
" and configure vim to your own liking!

" Pathogen
execute pathogen#infect()
syntax on
filetype plugin indent on

" Critical remaps
let mapleader=","
inoremap jk <esc>

" Other Mappings
nnoremap <Leader>q :q!<CR>
nnoremap <Leader>cp :CtrlP<CR>
nnoremap <Leader>oo o<esc>S
nnoremap <Leader>py :!python %<CR>
nnoremap <Leader>rb :!ruby %<CR>
nnoremap <Leader>mr :MRU<CR>
nnoremap <Leader>nt :NERDTree<CR>
nnoremap <Leader>pl :!perl %<CR>
nnoremap <Leader>asc ggVG:Tab /;<CR>

" Smart navigation
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Tabbing
set expandtab
set tabstop=2
set shiftwidth=2
set autoindent
set number
color emacs

" Backups
set nobackup
set noswapfile

" Special
autocmd FileType python setlocal shiftwidth=4 tabstop=4
