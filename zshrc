# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Pick one.
# ZSH_THEME="nanotech-mint"
ZSH_THEME="nanotech"

# Default browser
BROWSER="chromium"

# Aliases
alias sagi="sudo apt-get install"
alias acs="apt-cache search"
alias aps="apt search"
alias df="df -h"
alias du="du -h"
alias rh="runhaskell"
alias go="gnome-open"
alias py="python"
alias pac="\pacman"
alias pacman="sudo pacman"
alias pacupd="sudo pacman -Syu"
alias temp="acpi -t"
alias matlab="ssh -X umbc 'matlab'"
alias em="emacsclient -c -a \"\""

# File aliases
alias blog="vim ~/Documents/Misc/blog.txt"
alias books="vim ~/Documents/Misc/books.txt"
alias dreamj="vim ~/Documents/Misc/dreams.txt"
alias emrc="vim ~/.emacs.d/init.el"
alias fishrc="vim ~/.config/fish/config.fish"
alias ideas="vim ~/Documents/Misc/ideas.txt"
alias robots="vim ~/Documents/Misc/robots.txt"
alias shows="vim ~/Documents/Misc/shows.txt"
alias stask="vim ~/Documents/Misc/school_tasks.org"
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
  cp ~/.tmux.conf $DOTPATH/tmux.conf
}

# ZSH_TMUX_AUTOSTART="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
export UPDATE_ZSH_DAYS=30

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

plugins=(git history vi-mode zsh-syntax-highlighting history-substring-search tmux)

source $ZSH/oh-my-zsh.sh

# Path. Pick one.

# Mint:
# export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"

# Arch:
# export PATH=$PATH:/usr/lib/qt4/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/core_perl:/home/chris/.gem/ruby/2.0.0/bin

# export MANPATH="/usr/local/man:$MANPATH"

# Preferred editor for local and remote sessions
export EDITOR='vim'

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# # Setup zsh-autosuggestions
# source /home/chris/.zsh-autosuggestions/autosuggestions.zsh
#
# # Enable autosuggestions automatically
# zle-line-init() {
#     zle autosuggest-start
# }
#
# zle -N zle-line-init
#
# # use ctrl+t to toggle autosuggestions(hopefully this wont be needed as
# # zsh-autosuggestions is designed to be unobtrusive)
# # bindkey '^T' autosuggest-toggle

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# Setup zsh-autosuggestions
source /home/chris/.zsh-autosuggestions/autosuggestions.zsh

# Enable autosuggestions automatically
zle-line-init() {
    zle autosuggest-start
}

zle -N zle-line-init

# use ctrl+t to toggle autosuggestions(hopefully this wont be needed as
# zsh-autosuggestions is designed to be unobtrusive)
bindkey '^T' autosuggest-toggle
