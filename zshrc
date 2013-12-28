# Path to oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="nanotech-arch-white"

# Default browser
BROWSER="chromium"

# General aliases
alias df="df -h"
alias du="du -h"
alias rh="runhaskell"
alias pac="\pacman"
alias pacman="sudo pacman"
alias pacupd="sudo pacman -Syu"
alias temp="acpi -t"

# File aliases
alias blog="vim ~/Documents/Misc/blog.txt"
alias dreamj="vim ~/Documents/Misc/dreams.txt"
alias ideas="vim ~/Documents/Misc/ideas.txt"
alias robots="vim ~/Documents/Misc/robots.txt"
alias shows="vim ~/Documents/Misc/shows.txt"
alias stask="vim ~/Documents/Misc/school_tasks.txt"
alias todo="vim ~/Documents/Misc/todo.txt"
alias vimrc="vim ~/.vim/vimrc"
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

# Quick google search
google(){
  $BROWSER "google.com/search?q=$*"
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

# Dotfiles copy
export DOTPATH=$HOME/Devel/dotfiles
function dot_copy {
  cp ~/.zshrc $DOTPATH/zshrc
  cp ~/.vim/vimrc $DOTPATH/vimrc
  cp ~/.gitconfig $DOTPATH/gitconfig
}

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Plugins to load at startup
plugins=(git history vi-mode zsh-syntax-highlighting history-substring-search)

source $ZSH/oh-my-zsh.sh

# Path export
export PATH=$PATH:/usr/lib/qt4/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/core_perl:/home/chris/.gem/ruby/2.0.0/bin
