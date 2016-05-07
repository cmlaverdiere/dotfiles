# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="nanotech"

# Default browser
BROWSER="chromium"

# Preferred editor for local and remote sessions
export EDITOR='vim'

# Aliases #

# General
alias df="df -h"
alias du="du -h"
alias em="emacsclient -c -a \"\""
alias ke="killall emacs"
alias matlab="ssh -X umbc 'matlab'"
alias py="python"
alias rh="runhaskell"
alias temp="acpi -t"
alias tmux="tmux -2"

# Ubuntu
alias acs="apt-cache search"
alias aps="apt search"
alias go="gnome-open"
alias sagi="sudo apt-get install"

# Arch
alias pac="\pacman"
alias pacman="sudo pacman"
alias pup="sudo pacman -Syu"
alias pi="sudo pacman -S"

# File aliases
alias blog="vim ~/documents/Misc/blog.txt"
alias books="vim ~/documents/Misc/books.txt"
alias dreamj="vim ~/documents/Misc/dreams.txt"
alias emrc="vim ~/.emacs.d/init.el"
alias fishrc="vim ~/.config/fish/config.fish"
alias ideas="vim ~/documents/Misc/ideas.txt"
alias goals="vim ~/documents/Misc/goals.txt"
alias links="vim ~/documents/Misc/links.txt"
alias rss="vim ~/documents/Misc/rss.txt"
alias robots="vim ~/documents/Misc/robots.txt"
alias shows="vim ~/documents/Misc/shows.txt"
alias stask="vim ~/org/school.org"
alias todo="vim ~/documents/Misc/todo.txt"
alias wanted="vim ~/documents/Misc/wanted.txt"
alias vimrc="vim ~/.vim/vimrc"
alias nvimrc="nvim ~/.config/nvim/init.vim"
alias zshrc="vim ~/.zshrc"

# alias for mkdir and cd into it.
mkcd(){
  mkdir $1
  cd $1
}

# Quick math operations using python interpreter
math(){
  python -c "from math import *; print($*)"
}

# Marks
export MARKPATH=$HOME/.marks
function jump {
  cd -P $MARKPATH/$1 2>/dev/null || echo "No such mark: $1"
}
function mark {
  mkdir -p $MARKPATH; ln -s $(pwd) $MARKPATH/$1
}
function unmark {
  rm -i $MARKPATH/$1
}
function marks {
  ls -l $MARKPATH | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}

# How often oh-my-zsh updates.
# export UPDATE_ZSH_DAYS=30
DISABLE_AUTO_UPDATE="true"

# All plugins.
plugins=(git history vi-mode history-substring-search autojump)

# Autojump
[[ -s /home/chris/.autojump/etc/profile.d/autojump.sh ]] && source /home/chris/.autojump/etc/profile.d/autojump.sh

# Load oh-my-zsh.
source $ZSH/oh-my-zsh.sh

# Syntax highlighting.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Ruby path.
export PATH=/home/chris/.gem/ruby/2.3.0/bin:$PATH
export GEM_HOME=$(ruby -e 'print Gem.user_dir')

# Rust path.
export RUST_SRC_PATH=/usr/src/rust/src

# Start X on login.
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

# A sane amount of history.
export HISTSIZE=1000000
export SAVEHIST=1000000

# fzf setup.
alias fzf="fzf -m"
# . /usr/share/fzf/key-bindings.zsh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
bindkey '^O' fzf-cd-widget

# virtualenv setup (uncomment for speed).
# export WORKON_HOME=~/.virtualenvs
# export PROJECT_HOME=~/devel/python/projects
# source /usr/bin/virtualenvwrapper.sh

alias en_sm="rm ~/.emacs.d && ln -s ~/.spacemacs.d ~/.emacs.d"
alias en_me="rm ~/.emacs.d && ln -s ~/.my-emacs.d ~/.emacs.d"

source ~/.vim/plugged/gruvbox/gruvbox_256palette.sh
