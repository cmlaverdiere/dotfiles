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

# Ubuntu
alias acs="apt-cache search"
alias aps="apt search"
alias go="gnome-open"
alias sagi="sudo apt-get install"

# Arch
alias pac="\pacman"
alias pacman="sudo pacman"
alias pacupd="sudo pacman -Syu"

# File aliases
alias blog="vim ~/documents/Misc/blog.txt"
alias books="vim ~/documents/Misc/books.txt"
alias dreamj="vim ~/documents/Misc/dreams.txt"
alias emrc="vim ~/.emacs.d/init.el"
alias fishrc="vim ~/.config/fish/config.fish"
alias ideas="vim ~/documents/Misc/ideas.txt"
alias robots="vim ~/documents/Misc/robots.txt"
alias shows="vim ~/documents/Misc/shows.txt"
alias stask="vim ~/org/school.org"
alias todo="vim ~/documents/Misc/todo.txt"
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
export UPDATE_ZSH_DAYS=30

# All plugins.
plugins=(git history vi-mode history-substring-search autojump)

# Autojump
[[ -s /home/chris/.autojump/etc/profile.d/autojump.sh ]] && source /home/chris/.autojump/etc/profile.d/autojump.sh

# Load oh-my-zsh.
source $ZSH/oh-my-zsh.sh

# Syntax highlighting.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Start X on login.
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
