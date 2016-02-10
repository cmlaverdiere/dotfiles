" Chris Laverdiere's vimrc
" Requires: plug.vim

" Tmux / color compatability settings.
set shell=bash
set background=dark
set nocompatible

syntax on

" Plugins (mostly syntax files and motion extensions)
call plug#begin('~/.vim/plugged')
Plug 'tmhedberg/matchit'
Plug 'wellle/targets.vim'
Plug 'beyondmarc/opengl.vim'
Plug 'godlygeek/tabular'
Plug 'tomtom/tcomment_vim'
Plug 'kana/vim-altr'
Plug 'PeterRincker/vim-argumentative'
" Plug 'altercation/vim-colors-solarized'
Plug 'chriskempson/vim-tomorrow-theme'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'Lokaltog/vim-easymotion'
Plug 'kana/vim-filetype-haskell'
Plug 'tikhomirov/vim-glsl'
Plug 'lepture/vim-jinja'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'triglav/vim-visual-increment'
call plug#end()

filetype plugin indent on

" Case settings
set ignorecase
set smartcase

" Search settings
set incsearch
set timeoutlen=300
set showcmd

" Critical remaps
map <Space> <Leader>
map Y y$
map <c-j> <c-d>
map <c-k> <c-u>
imap jk <esc>

" Less critical remaps
nmap s <Plug>(easymotion-s)
inoremap <C-]> {<CR>}<esc>O
vnoremap . :normal .<CR>
imap <c-l> <c-g>u<Esc>[s1z=`]a<c-g>u
nmap <c-l> [s1z=<c-o>
nmap <silent> dsa ds}dF\
nmap <Leader>sw <Plug>(altr-forward)
nmap gy ggyG
nmap <Left> 5<C-w><
nmap <Right> 5<C-w>>
nmap <Up> 5<C-w>-
nmap <Down> 5<C-w>+

" Leader Mappings
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
nnoremap <Leader>me :au BufWritePost * make<CR>
nnoremap <Leader>mt :make tests<CR>
nnoremap <Leader>oo o<esc>S
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
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>tr :%s/\s*$//g<CR><C-o>zz
nnoremap <Leader>v :vs<CR>
nnoremap <Leader>V :sp<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>xxd :%!xxd<CR>
nnoremap <Leader>xxr :%!xxd -r<CR>
xnoremap <Leader>bc :!bc -l<CR>
xnoremap <Leader>pe :!python <CR>
set pastetoggle=<Leader>pt

" Quick window navigation
map gj <C-W>j
map gk <C-W>k
map gh <C-W>h
map gl <C-W>l

" Buffer switching
map gn :bn<cr>
map gp :bp<cr>
map gx :bd<cr>
set hidden

" Tabbing / Indentation
set expandtab
set tabstop=4
set shiftwidth=4
set smarttab
set autoindent

" Scrolling
set scrolloff=3

" Backups
set nobackup
set noswapfile
set undofile

" Misc
set nonumber
set laststatus=2
set scrolloff=3
set cm=blowfish2

" Remove trailing whitespace function.
fun! <SID>RTW()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfun

" Remove trailing whitespace on each save.
au BufWritePre * :call <SID>RTW()

" Disable folding in tex documents.
au Filetype tex setlocal nofoldenable

" Python specific indentation.
au FileType python setlocal shiftwidth=4 tabstop=4

" Markdown compatability
au BufNewFile,BufReadPost *.md set filetype=markdown

" Bebop filetype
au BufNewFile,BufReadPost *.bb set filetype=bebop

" Add to jumplist for multi j/k jumps.
nnoremap <silent> k :<C-U>execute 'normal!'
      \ (v:count > 1 ? "m'" . v:count : '') . 'k'<CR>
nnoremap <silent> j :<C-U>execute 'normal!'
      \ (v:count > 1 ? "m'" . v:count : '') . 'j'<CR>

" Convenient settings for prose.
au BufRead,BufNewFile *.txt,*.md setlocal textwidth=80
au BufRead *.txt,*.md setlocal spell

" Color scheme must be loaded later for some reason.
colorscheme Tomorrow-Night
