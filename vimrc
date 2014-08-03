" Distro specific settings. Pick one.
runtime! debian.vim
" runtime! archlinux.vim

" Tmux / color compatability settings.
set shell=bash
set t_Co=256
set background=dark

" Theme settings
colorscheme desert

" Vundle setup.
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'kien/ctrlp.vim'
Plugin 'tmhedberg/matchit'
Plugin 'yegappan/mru'
Plugin 'scrooloose/nerdtree'
Plugin 'myusuf3/numbers.vim'
Plugin 'beyondmarc/opengl.vim'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'ervandew/supertab'
Plugin 'scrooloose/syntastic'
Plugin 'godlygeek/tabular'
Plugin 'tomtom/tcomment_vim'
Plugin 'tomtom/tlib_vim'
Plugin 'bling/vim-airline'
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'kana/vim-filetype-haskell'
Plugin 'tpope/vim-fugitive'
Plugin 'lepture/vim-jinja'
Plugin 'tpope/vim-repeat'
Plugin 'garbas/vim-snipmate'
Plugin 'honza/vim-snippets'
Plugin 'tpope/vim-surround'
Plugin 'triglav/vim-visual-increment'
Plugin 'gmarik/Vundle.vim'

" Vundle teardown.
call vundle#end()
filetype plugin indent on

let g:syntastic_cpp_check_header = 1

if has("syntax")
  syntax on
endif

" Case settings
set ignorecase
set smartcase

" Search settings
set incsearch

" Critical remaps
let mapleader=","
inoremap jk <esc>

" Character remaps
inoremap /l λ

" Other Mappings
nnoremap <Leader>. :CtrlPTag<CR>
nnoremap <Leader>q :q!<CR>
nnoremap <Leader>cd :cd %:p:h<CR>
nnoremap <Leader>cp :CtrlP<CR>
nnoremap <Leader>nt :tabnew<CR>
nnoremap <Leader>oo o<esc>S
nnoremap <Leader>py :!python %<CR>
nnoremap <Leader>p2 :!python2 %<CR>
nnoremap <Leader>p3 :!python3 %<CR>
nnoremap <Leader>rb :!ruby %<CR>
nnoremap <Leader>rl :so ~/.vim/vimrc<CR>
nnoremap <Leader>rs :!Rscript %<CR>
nnoremap <Leader>rh :!runhaskell %<CR>
nnoremap <Leader>rt :RainbowParenthesesToggle<CR>
nnoremap <Leader>mr :MRU<CR>
nnoremap <Leader>nt :NERDTree<CR>
nnoremap <Leader>pi :PluginInstall<CR>
nnoremap <Leader>pl :!perl %<CR>
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>tr :%s/\s*$//g<CR>
nnoremap <Leader>asc ggVG:Tab /;<CR>
nnoremap <Leader>dsB diB]pkdk<CR>
nnoremap <Leader>gcc :!gcc % -o %< && ./%<<CR>
nnoremap <Leader>gpp :!g++ % -o %< && ./%<<CR>
nnoremap <Leader>pdf :!pdflatex % && !okular %<CR>
nnoremap <Leader>rot ggVGg?
nnoremap <Leader>scm :!racket -r %<CR>
nnoremap <Leader>xxd :%!xxd<CR>
nnoremap <Leader>xxr :%!xxd -r<CR>
set pastetoggle=<Leader>pt

" Smart navigation
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Tabbing
set expandtab
set tabstop=2
set shiftwidth=2
set smarttab
set number
set laststatus=2
" set autoindent

" Backups
set nobackup
set noswapfile

" Python specific indentation.
autocmd FileType python setlocal shiftwidth=4 tabstop=4

" Auto generate tag files on write.
au BufWritePost *.c,*.cpp,*.h silent! !ctags -R --c++-kinds=+p --fields=+iaS &
