# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="nanotech-mint"

# General aliases
alias sagi="sudo apt-get install"
alias acs="apt-cache search"
alias df="df -h"
alias du="du -h"
alias go="gnome-open"
alias rh="runhaskell"
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

# Temporary alias to update our game on umbc gl.
alias ugame="ssh umbc \"cd ~/www/Four-to-the-Fifth; git pull\""

# alias for mkdir and cd into it.
mkcd(){
  mkdir $1
  cd $1
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

ZSH_TMUX_AUTOSTART="true"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
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

# Uncomment following line if you want to  shown in the command execution time stamp 
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git history vi-mode zsh-syntax-highlighting history-substring-search tmux)

source $ZSH/oh-my-zsh.sh

# User configuration

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
# export MANPATH="/usr/local/man:$MANPATH"

# # Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

