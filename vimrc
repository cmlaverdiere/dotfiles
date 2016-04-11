" Chris Laverdiere's vimrc
" Requires: plug.vim

" Shell compatibility settings.
set shell=bash
set background=dark
set nocompatible

" Buffer settings
set hidden

" Case settings
set ignorecase
set smartcase

" Clipboard settings
set clipboard=unnamedplus

" Search settings
set incsearch
set hlsearch

" Tabbing / Indentation
set expandtab
set tabstop=4
set shiftwidth=4
set smarttab
set autoindent
set textwidth=79
set wildmenu
set wildmode=full
" set colorcolumn=81

" Scrolling
set scrolloff=3

" Backups
set noswapfile
set backup
set backupdir=~/.vimbak
set undofile
set undodir=~/.vimundo

" Spelling
setlocal spell

" Misc
set gdefault
set nonumber
set laststatus=2
set scrolloff=3
set cryptmethod=blowfish2
set timeoutlen=300
set showcmd

syntax on
filetype plugin indent on

" Plugins (mostly syntax files and motion extensions)
runtime macros/matchit.vim
call plug#begin('~/.vim/plugged')
Plug 'morhetz/gruvbox'
Plug 'beyondmarc/opengl.vim', { 'for': ['c', 'cpp'] }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'godlygeek/tabular'
Plug 'wellle/targets.vim'
Plug 'tomtom/tcomment_vim'
Plug 'kana/vim-altr'
Plug 'PeterRincker/vim-argumentative'
Plug 'octol/vim-cpp-enhanced-highlight', { 'for': ['c', 'cpp'] }
Plug 'kana/vim-filetype-haskell', { 'for': 'haskell' }
Plug 'tikhomirov/vim-glsl'
Plug 'tommcdo/vim-lion'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-user'
Plug 'triglav/vim-visual-increment'
Plug 'nelstrom/vim-visual-star-search'
call plug#end()

" Critical remaps
map <Space> <Leader>
nnoremap Y y$
inoremap jk <esc>
nnoremap ' `
nnoremap ` '

" Repeat maps
vnoremap . :normal .<CR>
xnoremap @q :normal @q<CR>

" Spelling maps
inoremap <C-y> <c-g>u<Esc>[s1z=`]a<c-g>u
nnoremap <C-y> [s1z=<c-o>

" Window resize maps
nnoremap <Left> 5<C-w><
nnoremap <Right> 5<C-w>>
nnoremap <Up> 5<C-w>-
nnoremap <Down> 5<C-w>+

" Window movement maps
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
inoremap <C-h> <esc><C-w>h
inoremap <C-j> <esc><C-w>j
inoremap <C-k> <esc><C-w>k
inoremap <C-l> <esc><C-w>l

" Misc maps
nnoremap <silent> dsa ds}dF\
inoremap <C-]> {<CR>}<esc>O

" Leader Mappings
nnoremap <Leader>asc ggVG:Tab /;<CR>
nnoremap <Leader>c :make<CR>
nnoremap <Leader>d :cd %:p:h<CR>
nnoremap <Leader>gcc :!gcc -g -std=c99 % -o %< && ./%<<CR>
nnoremap <Leader>gpp :!g++ -g -std=c++11 % -o %< && ./%<<CR>
nnoremap <Leader>i :e ~/.vim/vimrc<CR>
nnoremap <silent> <Leader>n :nohlsearch<CR>
nnoremap <Leader>me :au BufWritePost * make<CR>
nnoremap <Leader>mt :make tests<CR>
nnoremap <Leader>o <Plug>(altr-forward)
nnoremap <Leader>p2 :!python2 %<CR>
nnoremap <Leader>p3 :!python3 %<CR>
nnoremap <Leader>pfc <Leader>pt"*p<Leader>pt
nnoremap <Leader>pi :PlugInstall<CR>
nnoremap <Leader>pl :!perl %<CR>
nnoremap <Leader>py :!python %<CR>
nnoremap <Leader>q :q!<CR>
nnoremap <Leader>rb :!ruby %<CR>
nnoremap <Leader>rh :!runhaskell %<CR>
nnoremap <Leader>rl :so ~/.vim/vimrc<CR>
nnoremap <Leader>rs :!Rscript %<CR>
nnoremap <Leader>sap vip:sort<CR>
nnoremap <Leader>scm :!racket -r %<CR>
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>tr :%s/\s*$//g<CR><C-o>zz
nnoremap <Leader>T :!ctags -R<CR>
nnoremap <Leader>v :vs<CR>
nnoremap <Leader>V :sp<CR>
nnoremap <Leader>w :w<CR>

xnoremap <Leader>bc :!bc -l<CR>
xnoremap <Leader>pe :!python <CR>

set pastetoggle=<Leader>pt

" Remove trailing whitespace function.
fun! RTW()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfun
command! RTW call RTW()

" Disable folding in tex documents.
au Filetype tex setlocal nofoldenable

" Python specific indentation.
au FileType python setlocal shiftwidth=4 tabstop=4

" Markdown compatibility
au BufNewFile,BufReadPost *.md set filetype=markdown

" Disable ftplugin-mail maps.
let no_mail_maps=1

" Add to jumplist for multi j/k jumps.
nnoremap <silent> k :<C-U>execute 'normal!'
      \ (v:count > 1 ? "m'" . v:count : '') . 'k'<CR>
nnoremap <silent> j :<C-U>execute 'normal!'
      \ (v:count > 1 ? "m'" . v:count : '') . 'j'<CR>

" Color scheme must be loaded at the end.
colorscheme gruvbox
