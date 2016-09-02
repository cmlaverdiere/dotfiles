# Zsh config
# Optional deps:
# zsh-syntax-highlighting
# zsh-history-substring-search
# autojump
# fzf

# Defaults
export BROWSER="chromium"
export EDITOR='vim'
export LEDGER_FILE='~/finance/ledger.ldg'
export FIT_FILE='~/fitness/fit.log'
export TERMINAL='urxvt'

# Init vars
ENABLE_GIT_STATUS=false

# Completion
zstyle :compinstall filename '/home/chris/.zshrc'
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' menu select
setopt completeinword
setopt COMPLETE_ALIASES
setopt extendedglob
unsetopt CASE_GLOB
autoload -Uz compinit
compinit

# Misc settings
bindkey -v
autoload -Uz colors && colors
setopt interactivecomments

# Directory stack
setopt AUTO_PUSHD PUSHD_SILENT PUSHD_TO_HOME PUSHD_IGNORE_DUPS PUSHD_MINUS

# Prompt
setopt prompt_subst
autoload -Uz vcs_info
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr '^'
zstyle ':vcs_info:*' unstagedstr '*'
zstyle ':vcs_info:*' formats '%c%u %b%f'
zstyle ':vcs_info:*' enable git
vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "${vcs_info_msg_0_}"
  fi
}
PROMPT='%2c [%f '
if [[ "$ENABLE_GIT_STATUS" == true ]]; then
    RPROMPT='$(vcs_info_wrapper) ] %D{%L:%M} %D{%p}%f'
else
    RPROMPT=' ] %D{%L:%M} %D{%p}%f'
fi
autoload -Uz promptinit
promptinit

# History
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
setopt appendhistory extendedglob
setopt HIST_IGNORE_DUPS

# Aliases (General)
alias df="df -h"
alias du="du -h"
alias em="emacsclient -c -a \"\""
alias ke="killall emacs"
alias matlab="ssh -X umbc 'matlab'"
#alias python="ipython"
alias rh="runhaskell"
alias open="xdg-open"
alias temp="acpi -t"
alias tmux="tmux -2"
alias en_sm="rm ~/.emacs.d && ln -s ~/.spacemacs.d ~/.emacs.d"
alias en_me="rm ~/.emacs.d && ln -s ~/.my-emacs.d ~/.emacs.d"
alias ls="ls --color=auto"
alias ll="ls -l --color=auto"
alias la="ls -a --color=auto"
alias album="youtube-dl -x --embed-thumbnail --add-metadata -f 140"
alias vi='vim -u ~/.vim/vimrc.mini'
alias vidir="EDITOR=vim vidir"

# Aliases (Arch)
alias pac="\pacman"
alias pacman="sudo pacman"
alias pup="sudo pacman -Syu"
alias pi="sudo pacman -S"

# Aliases (Ubuntu)
alias acs="apt-cache search"
alias aps="apt search"
alias go="gnome-open"
alias sagi="sudo apt-get install"

# Aliases (Files)
alias blog="vim ~/documents/Misc/blog.txt"
alias books="vim ~/documents/Misc/books.txt"
alias dreamj="vim ~/documents/Misc/dreams.txt"
alias emrc="vim ~/.emacs.d/init.el"
alias fishrc="vim ~/.config/fish/config.fish"
alias fit="python ~/devel/python/fit/fit.py"
alias fitf="vim $FIT_FILE"
alias ideas="vim ~/documents/Misc/ideas.txt"
alias i3rc="vim ~/.config/i3/config"
alias ledf="vim $LEDGER_FILE"
alias goals="vim ~/documents/Misc/goals.txt"
alias led="ledger"
alias links="vim ~/documents/Misc/links.txt"
alias rss="vim ~/documents/Misc/rss.txt"
alias robots="vim ~/documents/Misc/robots.txt"
alias shop="vim ~/lists/shop.txt"
alias shows="vim ~/documents/Misc/shows.txt"
alias stask="vim ~/org/school.org"
alias wanted="vim ~/documents/Misc/wanted.txt"
alias vimrc="vim ~/.vim/vimrc"
alias nvimrc="nvim ~/.config/nvim/init.vim"
alias zshrc="vim ~/.zshrc"

gpu() {
    DRI_PRIME=1 $@
}

# Functions
mkcd() {
  mkdir $1
  cd $1
}

cdChild() {
  popd > /dev/null
  zle reset-prompt
}

cdParent() {
  pushd .. > /dev/null
  zle reset-prompt
}

zle -N cdChild
zle -N cdParent
bindkey '^N' cdChild
bindkey '^P' cdParent

# Handle updates to the window title.
if [[ $TERM == 'rxvt-unicode-256color' ]]; then
    function precmd {
        print -Pn "\e]0;zsh%L %(1j,%j job%(2j|s|); ,)%~\a"
    }

    function preexec {
        printf "\033]0;%s\a" "$1"
    }
fi

# Start X on login.
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

# fzf setup
alias fzf="fzf -m"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
bindkey '^O' fzf-cd-widget

# Ruby path. Comment out for speed.
# export PATH=/home/chris/.gem/ruby/2.3.0/bin:$PATH
# export GEM_HOME=$(ruby -e 'print Gem.user_dir')

# Autojump setup
[[ -s /etc/profile.d/autojump.sh ]] && source /etc/profile.d/autojump.sh

# Syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Reverse history substring search
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Rust path
export RUST_SRC_PATH=/usr/src/rust/src

# Virtualenv setup. Comment out for speed.
# export WORKON_HOME=~/.virtualenvs
# export PROJECT_HOME=~/devel/python/projects
# source /usr/bin/virtualenvwrapper.sh

# Fix vim colors with gruvbox.
source ~/.vim/plugged/gruvbox/gruvbox_256palette.sh
