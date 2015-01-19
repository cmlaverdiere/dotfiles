" Distro specific settings. Pick one.
runtime! debian.vim
" runtime! archlinux.vim

" Tmux / color compatability settings.
set shell=bash
set background=dark

" Theme settings
" colorscheme genericdc

" Vundle setup.
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Plugin 'godlygeek/csapprox'
Plugin 'rking/ag.vim'
Plugin 'jaxbot/browserlink.vim'
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
Plugin 'kana/vim-altr'
Plugin 'bling/vim-bufferline'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tikhomirov/vim-glsl'
Plugin 'kana/vim-filetype-haskell'
Plugin 'tpope/vim-fugitive'
Plugin 'lepture/vim-jinja'
Plugin 'lervag/vim-latex'
Plugin 'tpope/vim-repeat'
Plugin 'jpalardy/vim-slime'
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

" Terminal transparency support.
" hi Normal ctermbg=NONE

" Case settings
set ignorecase
set smartcase

" Search settings
set incsearch

" Critical remaps
let mapleader=","
inoremap jk <esc>

" Less critical remaps
inoremap <C-]> {<CR>}<esc>O
nmap <silent> dsa ds}dF\
nmap <Leader>sw <Plug>(altr-forward)

" Character remaps
" inoremap /l λ

" Other Mappings
nnoremap <Leader>. :CtrlPTag<CR>
nnoremap <Leader>asc ggVG:Tab /;<CR>
xnoremap <Leader>bc :!bc -l<CR>
nnoremap <Leader>cd :cd %:p:h<CR>
nnoremap <Leader>cp :CtrlP<CR>
nnoremap <Leader>ln :lnext<CR>
nnoremap <Leader>dsB diB]pkdk<CR>
nnoremap <Leader>gcc :!gcc -g % -o %< && ./%<<CR>
nnoremap <Leader>glK ?gl<CR>llx~K
nnoremap <Leader>gpp :!g++ -g % -o %< && ./%<<CR>
nnoremap <Leader>me :au BufWritePost * make<CR>
nnoremap <Leader>mr :MRU<CR>
nnoremap <Leader>nt :NERDTree<CR>
nnoremap <Leader>oo o<esc>S
xnoremap <Leader>pe :!python <CR>
nnoremap <Leader>p2 :!python2 %<CR>
nnoremap <Leader>p3 :!python3 %<CR>
nnoremap <Leader>pdf :!pdflatex % && !okular %<CR>
nnoremap <Leader>pi :PluginInstall<CR>
nnoremap <Leader>pl :!perl %<CR>
nnoremap <Leader>py :!python %<CR>
nnoremap <Leader>q :q!<CR>
nnoremap <Leader>rb :!ruby %<CR>
nnoremap <Leader>rh :!runhaskell %<CR>
nnoremap <Leader>rl :so ~/.vim/vimrc<CR>
nnoremap <Leader>rot ggVGg?
nnoremap <Leader>rs :!Rscript %<CR>
nnoremap <Leader>rt :RainbowParenthesesToggle<CR>
nnoremap <Leader>sap vap:sort<CR>
nnoremap <Leader>scm :!racket -r %<CR>
nnoremap <Leader>sr :SyntasticReset<CR>
nnoremap <Leader>st :SyntasticToggleMode<CR>
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>tr :%s/\s*$//g<CR>
nnoremap <Leader>vrc :e ~/.vim/vimrc<CR>
nnoremap <Leader>xe :!xelatex %<CR>
nnoremap <Leader>xxd :%!xxd<CR>
nnoremap <Leader>xxr :%!xxd -r<CR>
set pastetoggle=<Leader>pt

" Smart navigation
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Buffer quick maps
map gn :bn<cr>
map gp :bp<cr>
map gx :bd<cr>
set hidden

" Tabbing
set expandtab
set tabstop=2
set shiftwidth=2
set smarttab
set number
set laststatus=2
set autoindent

" Backups
set nobackup
set noswapfile
set undofile
set history=100000

" Disable mode for airline
set noshowmode

" Disable folding in tex documents.
autocmd Filetype tex setlocal nofoldenable

" For jumping between syntastic errors.
let g:syntastic_always_populate_loc_list = 1

" Disable bufferline echo so airline can handle it.
let g:bufferline_echo = 0

" Slime settings
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}
let g:slime_python_ipython = 1

" Solarized must be loaded later for some reason.
colorscheme solarized
let g:airline_theme = 'solarized'

" Python specific indentation.
autocmd FileType python setlocal shiftwidth=4 tabstop=4

" Auto generate tag files on write.
au BufWritePost *.c,*.cpp,*.h silent! !ctags -R --c++-kinds=+p --fields=+iaS &

" Markdown compatability
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Add to jumplist for multi j/k jumps.
nnoremap <silent> k :<C-U>execute 'normal!' (v:count > 1 ? "m'" . v:count : '') . 'k'<CR>
nnoremap <silent> j :<C-U>execute 'normal!' (v:count > 1 ? "m'" . v:count : '') . 'j'<CR>

" Rainbow parens enabled by default.
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" Convenient settings for prose.
au BufRead,BufNewFile *.txt,*.md setlocal textwidth=80
autocmd InsertEnter *.txt,*.md setlocal spell
autocmd InsertLeave *.txt,*.md setlocal nospell
