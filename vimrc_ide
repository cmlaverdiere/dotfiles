" Chris Laverdiere's vimrc
" Requires: plug.vim

" Tmux / color compatability settings.
set shell=bash
set background=dark
set nocompatible

syntax on

" Plugins
call plug#begin('~/.vim/plugged')
Plug 'rking/ag.vim'
Plug 'kien/ctrlp.vim'
Plug 'tmhedberg/matchit'
Plug 'scrooloose/nerdtree'
Plug 'wellle/targets.vim'
Plug 'beyondmarc/opengl.vim'
Plug 'kien/rainbow_parentheses.vim'
Plug 'mtth/scratch.vim'
Plug 'ervandew/supertab'
Plug 'scrooloose/syntastic'
Plug 'godlygeek/tabular'
Plug 'tomtom/tcomment_vim'
Plug 'tomtom/tlib_vim'
Plug 'bling/vim-airline'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'kana/vim-altr'
Plug 'PeterRincker/vim-argumentative'
Plug 'bling/vim-bufferline'
" Plug 'altercation/vim-colors-solarized'
Plug 'chriskempson/vim-tomorrow-theme'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'Lokaltog/vim-easymotion'
Plug 'kana/vim-filetype-haskell'
Plug 'tpope/vim-fugitive'
Plug 'tikhomirov/vim-glsl'
Plug 'lepture/vim-jinja'
Plug 'lervag/vim-latex'
Plug 'tpope/vim-repeat'
Plug 'jpalardy/vim-slime'
Plug 'garbas/vim-snipmate'
Plug 'honza/vim-snippets'
Plug 'tpope/vim-surround'
Plug 'triglav/vim-visual-increment'
Plug 'gmarik/Vundle.vim'
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
map <CR> mao<Esc>k`a
imap jk <esc>

" Less critical remaps
nmap s <Plug>(easymotion-s)
nmap gs :Scratch<CR>
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
nnoremap <Leader>. :CtrlPTag<CR>
nnoremap <Leader>/ "ayaw:Ag! <C-r>a<CR>
nnoremap <Leader>asc ggVG:Tab /;<CR>
nnoremap <Leader>cK viWK
nnoremap <Leader>cd :cd %:p:h<CR>
nnoremap <Leader>dsB diB]pkdk<CR>
nnoremap <Leader>ee :winc =<CR>
nnoremap <Leader>el :15winc ><CR>
nnoremap <Leader>er :15winc <<CR>
nnoremap <Leader>gcc :!gcc -g -std=c99 % -o %< && ./%<<CR>
nnoremap <Leader>glK ?gl<CR>llx~K
nnoremap <Leader>gpp :!g++ -g -std=c++11 % -o %< && ./%<<CR>
nnoremap <Leader>gr :Ag!
nnoremap <Leader>i :e ~/.vim/vimrc<CR>
nnoremap <Leader>ln :lnext<CR>
nnoremap <Leader>me :au BufWritePost * make<CR>
nnoremap <Leader>mr :CtrlPMRU<CR>
nnoremap <Leader>mt :make tests<CR>
nnoremap <Leader>nt :NERDTree<CR>
nnoremap <Leader>oo o<esc>S
nnoremap <Leader>p2 :!python2 %<CR>
nnoremap <Leader>p3 :!python3 %<CR>
nnoremap <Leader>pdf :!pdflatex % && !okular %<CR>
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
nnoremap <Leader>rt :RainbowParenthesesToggle<CR>
nnoremap <Leader>sap vip:sort<CR>
nnoremap <Leader>scm :!racket -r %<CR>
nnoremap <Leader>sr :SyntasticReset<CR>
nnoremap <Leader>st :SyntasticToggleMode<CR>
nnoremap <Leader>td :Ag! TODO<CR>
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>tr :%s/\s*$//g<CR><C-o>zz
nnoremap <Leader>v :vs<CR>
nnoremap <Leader>V :sp<CR>
nnoremap <Leader>ul :call <SID>MDUL()<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>xe :!xelatex %<CR>
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
set cursorline
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

" Convert markdown '#' headings to underlines.
fun! <SID>MDUL()
  silent! g/^# /norm dwyypVr=
  silent! g/^## /norm dwyypVr-
endfun

" Remove trailing whitespace on each save.
au BufWritePre * :call <SID>RTW()

" Disable folding in tex documents.
au Filetype tex setlocal nofoldenable

" Python specific indentation.
au FileType python setlocal shiftwidth=4 tabstop=4

" Auto generate tag files on write.
au BufWritePost *.c,*.cpp,*.h silent! !ctags -R --c++-kinds=+p --fields=+iaS &

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
" au InsertEnter *.txt,*.md setlocal spell
" au InsertLeave *.txt,*.md setlocal nospell

" GUI options
:set go-=m  "remove menu bar
:set go-=T  "remove toolbar
:set go-=r  "remove right-hand scroll bar
:set go-=L  "remove left-hand scroll bar

" Plugin specific settings

" Rainbow parens enabled by default.
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" Scratch settings
let g:scratch_no_mappings = 1
let g:scratch_autohide = 1
let g:scratch_insert_autohide = 0

" Color scheme must be loaded later for some reason.
colorscheme Tomorrow-Night
let g:airline_theme = 'tomorrow'

" Slime settings
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}
let g:slime_python_ipython = 1

" Disable bufferline echo so airline can handle it.
let g:bufferline_echo = 0

" For jumping between syntastic errors.
let g:syntastic_always_populate_loc_list = 1

" Force syntastic to also check header files.
let g:syntastic_cpp_check_header = 1

" Use c++11 for syntastic.
let g:syntastic_cpp_compiler_options = '-std=c++11 -Wall -O2'
let g:syntastic_c_compiler_options = '-std=c99 -Wall -O2'

" Use python 3 for syntastic.
let g:syntastic_python_python_exec = '/usr/bin/python3'

" Disable mode for airline
set noshowmode
