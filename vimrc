runtime! debian.vim

" Pathogen
execute pathogen#infect('~/.vim/bundle/{}')
filetype plugin indent on

if has("syntax")
  syntax on
endif

set background=dark

" Case settings
set ignorecase
set smartcase

" Search settings
set incsearch

" Critical remaps
let mapleader=","
inoremap jk <esc>

" Other Mappings
nnoremap <Leader>q :q!<CR>
nnoremap <Leader>cd :cd %:p:h<CR>
nnoremap <Leader>cp :CtrlP<CR>
nnoremap <Leader>nt :tabnew<CR>
nnoremap <Leader>oo o<esc>S
nnoremap <Leader>py :!python %<CR>
nnoremap <Leader>p3 :!python3 %<CR>
nnoremap <Leader>rb :!ruby %<CR>
nnoremap <Leader>rs :!Rscript %<CR>
nnoremap <Leader>rh :!runhaskell %<CR>
nnoremap <Leader>rt :RainbowParenthesesToggle<CR>
nnoremap <Leader>mr :MRU<CR>
nnoremap <Leader>nt :NERDTree<CR>
nnoremap <Leader>pl :!perl %<CR>
nnoremap <Leader>asc ggVG:Tab /;<CR>
nnoremap <Leader>dsB diB]pkdk<CR>
nnoremap <Leader>gcc :!gcc % -o %< && ./%<<CR>
nnoremap <Leader>pdf :!pdflatex % && !okular %<CR>
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
set smartindent
set smarttab
set number
color emacs

" Backups
set nobackup
set noswapfile

autocmd FileType python setlocal shiftwidth=4 tabstop=4

" Source a global configuration file if available
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif
