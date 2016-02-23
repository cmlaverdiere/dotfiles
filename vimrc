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

" Search settings
set incsearch
set timeoutlen=300
set showcmd
set hlsearch

" Tabbing / Indentation
set expandtab
set tabstop=4
set shiftwidth=4
set smarttab
set autoindent
set textwidth=80

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
set number
set laststatus=2
set scrolloff=3
set cryptmethod=blowfish2

syntax on

" Plugins (mostly syntax files and motion extensions)
call plug#begin('~/.vim/plugged')
Plug 'morhetz/gruvbox'
Plug 'beyondmarc/opengl.vim', { 'for': ['c', 'cpp'] }
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
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-user'
Plug 'triglav/vim-visual-increment'
Plug 'nelstrom/vim-visual-star-search'
call plug#end()

filetype plugin indent on

" Critical remaps
map <Space> <Leader>
nnoremap Y y$
inoremap jk <esc>

" Less critical remaps
inoremap <C-]> {<CR>}<esc>O
vnoremap . :normal .<CR>
xnoremap @q :normal @q<CR>
inoremap <C-y> <c-g>u<Esc>[s1z=`]a<c-g>u
nnoremap <C-y> [s1z=<c-o>
nnoremap <silent> dsa ds}dF\
nnoremap <Left> 5<C-w><
nnoremap <Right> 5<C-w>>
nnoremap <Up> 5<C-w>-
nnoremap <Down> 5<C-w>+

" Leader Mappings
nnoremap <Leader>V :sp<CR>
nnoremap <Leader>asc ggVG:Tab /;<CR>
nnoremap <Leader>b :b#<CR>
nnoremap <Leader>cK viWK
nnoremap <Leader>cd :cd %:p:h<CR>
nnoremap <Leader>dsB diB]pkdk<CR>
nnoremap <Leader>ee :winc =<CR>
nnoremap <Leader>gcc :!gcc -g -std=c99 % -o %< && ./%<<CR>
nnoremap <Leader>glK ?gl<CR>llx~K
nnoremap <Leader>gpp :!g++ -g -std=c++11 % -o %< && ./%<<CR>
nnoremap <Leader>i :e ~/.vim/vimrc<CR>
nnoremap <Leader>ln :lnext<CR>
nnoremap <silent> <Leader>n :nohlsearch<CR>
nnoremap <Leader>me :au BufWritePost * make<CR>
nnoremap <Leader>mt :make tests<CR>
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
nnoremap <Leader>rot ggVGg?
nnoremap <Leader>rs :!Rscript %<CR>
nnoremap <Leader>sap vip:sort<CR>
nnoremap <Leader>scm :!racket -r %<CR>
nnoremap <Leader>sw <Plug>(altr-forward)
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>tr :%s/\s*$//g<CR><C-o>zz
nnoremap <Leader>v :vs<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>xxd :%!xxd<CR>
nnoremap <Leader>xxr :%!xxd -r<CR>

xnoremap <Leader>bc :!bc -l<CR>
xnoremap <Leader>pe :!python <CR>

set pastetoggle=<Leader>pt

" Quick window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

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

" Add to jumplist for multi j/k jumps.
nnoremap <silent> k :<C-U>execute 'normal!'
      \ (v:count > 1 ? "m'" . v:count : '') . 'k'<CR>
nnoremap <silent> j :<C-U>execute 'normal!'
      \ (v:count > 1 ? "m'" . v:count : '') . 'j'<CR>

" Color scheme must be loaded later for some reason.
colorscheme gruvbox
