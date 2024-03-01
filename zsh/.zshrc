# Zsh config
# Optional deps:
# zsh-syntax-highlighting
# zsh-history-substring-search
# autojump
# fzf

# Defaults
export BROWSER="chromium"
export EDITOR='nvim'
export LEDGER_FILE='~/finance/ledger.ldg'
# export FIT_FILE='~/fitness/fit.log'
# export TERMINAL='urxvt'
# export CC=/usr/bin/clang
# export CXX=/usr/bin/clang++

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
bindkey -M viins 'jk' vi-cmd-mode
autoload -Uz colors && colors
setopt interactivecomments
setopt INC_APPEND_HISTORY

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
# alias db="coredumpctl gdb"
alias df="df -h"
alias du="du -h"
alias psa="ps aux | grep $1"
# alias em="emacsclient -c -a \"\""
# alias ke="killall -s 9 emacs"
alias make="make -j9"
# alias matlab="ssh -X umbc 'matlab'"
# alias python="ipython"
# alias rh="runhaskell"
# alias temp="acpi -t"
# alias en_sm="rm ~/.emacs.d && ln -s ~/.spacemacs.d ~/.emacs.d"
# alias en_me="rm ~/.emacs.d && ln -s ~/.my-emacs.d ~/.emacs.d"
alias ls="exa"
alias ll="exa -l"
alias la="exa -a"
alias ipy="ipython"
# alias album="youtube-dl -x --embed-thumbnail --add-metadata -f 140"
# alias playlist="album -o '%(playlist_index)s - %(title)s.%(ext)s'"
# alias vi='vim -u ~/.vim/vimrc.mini'
alias vidir="EDITOR=vim vidir"
alias vsh="vagrant ssh"
alias gf="git-fuzzy"
# alias vlc='/Applications/VLC.app/Contents/MacOS/VLC'
alias dc='docker-compose'
alias de='docker exec -it $DE_CONT'
alias tp='echo $TMUX_PANE'
alias t='todo.sh'
alias te='vim ~/Documents/todo/todo.txt'
alias gcd='git cm -m $(date +%m-%d-%Y)'
alias nf='terminal-notifier -title "👋" -message "Task complete - Exit status: $?"; curl -d "Task complete" ntfy.sh/cmlaverdiere_new'
alias vimdiff="nvim -d"

# wbeam() {
#     find $1 | entr sh -c "pandoc -t beamer --latex-engine=xelatex $1 -o \
#         $(echo $1 | cut -f 1 -d '.').pdf"
# }

ytdl() {
    youtube-dl "$1"
}

# Aliases (Arch)
# alias pac="\pacman"
# alias pacman="sudo pacman"
# alias pup="sudo pacman -Syu"
# alias pi="sudo pacman -S"

# Aliases (Ubuntu)
# alias acs="apt-cache search"
# alias aps="apt search"
# alias sagi="sudo apt-get install"

# Aliases (Files)
# alias blog="vim ~/documents/Misc/blog.txt"
# alias books="vim ~/documents/Misc/books.txt"
# alias dreamj="vim ~/documents/Misc/dreams.txt"
# alias emrc="vim ~/.emacs.d/init.el"
# alias fishrc="vim ~/.config/fish/config.fish"
# alias fit="python ~/devel/python/fit/fit.py"
# alias fitf="vim $FIT_FILE"
# alias ideas="vim ~/documents/Misc/ideas.txt"
# alias i3rc="vim ~/.config/i3/config"
# alias ledf="vim $LEDGER_FILE"
# alias learn="vim ~/documents/Misc/learn.txt"
# alias goals="vim ~/documents/Misc/goals.txt"
# alias led="ledger"
# alias links="vim ~/documents/Misc/links.txt"
# alias rss="vim ~/documents/Misc/rss.txt"
# alias robots="vim ~/documents/Misc/robots.txt"
# alias shop="vim ~/lists/shop.txt"
# alias shows="vim ~/documents/Misc/shows.txt"
# alias stask="vim ~/org/school.org"
# alias wanted="vim ~/documents/Misc/wanted.txt"
alias vimrc="vim ~/.vim/vimrc"
alias nvimrc="nvim ~/.config/nvim/init.vim"
alias zshrc="vim ~/.zshrc"

# gpu() {
#     DRI_PRIME=1 $@
# }

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

# reload_kbd() {
#     setxkbmap -option ctrl:rctrl
#     setxkbmap -option ctrl:swapcaps
#     setxkbmap -option ctrl:nocaps
#     xset r rate 175 50
#     xset m 3/2 0
# }

zle -N cdChild
zle -N cdParent
bindkey '^N' cdChild
bindkey '^P' cdParent

# Handle updates to the window title.
# if [[ $TERM == 'rxvt-unicode-256color' ]]; then
#     function precmd {
#         print -Pn "\e]0;zsh%L %(1j,%j job%(2j|s|); ,)%~\a"
#     }

#     function preexec {
#         printf "\033]0;%s\a" "$1"
#     }
# fi

# Start X on login.
# [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

# fzf setup
alias fzf="fzf -m"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
bindkey '^O' fzf-cd-widget
export FZF_DEFAULT_COMMAND='fd --follow --type f'
# export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
#  --color fg:#ebdbb2,bg:#282828,hl:#fabd2f,fg+:#ebdbb2,bg+:#3c3836,hl+:#fabd2f
#  --color info:#83a598,prompt:#bdae93,spinner:#fabd2f,pointer:#83a598,marker:#fe8019,header:#665c54'

# Ruby path. Comment out for speed.
# export PATH=/home/chris/.gem/ruby/2.3.0/bin:$PATH
# export GEM_HOME=$(ruby -e 'print Gem.user_dir')

# export PATH=$PATH:/home/chris/.local/bin

# Autojump setup
# [[ -s /etc/profile.d/autojump.sh ]] && source /etc/profile.d/autojump.sh
[ -f $(brew --prefix)/etc/profile.d/autojump.sh ] && . $(brew --prefix)/etc/profile.d/autojump.sh

# Syntax highlighting
# source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Reverse history substring search
# source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source $(brew --prefix)/share/zsh-history-substring-search/zsh-history-substring-search.zsh

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey '^K' history-substring-search-up
bindkey '^J' history-substring-search-down

# Rust path
# export PATH=/Users/thrash/.cargo/bin:$PATH
# export RUST_SRC_PATH=/usr/src/rust/src

# Virtualenv setup. Comment out for speed.
# export WORKON_HOME=~/.virtualenvs
# export PROJECT_HOME=~/devel/python/projects
# source /usr/bin/virtualenvwrapper.sh

# export PATH=$PATH:~/android/tools/
# export PATH=$PATH:~/android/build-tools/25.0.0

export PYTHONPATH=$PYTHONPATH:.

# Fix vim colors with gruvbox.
# source ~/.vim/plugged/gruvbox/gruvbox_256palette.sh

# export PATH="/usr/local/bin:$PATH"
# export PATH="/usr/local/opt/python/libexec/bin:$PATH"

# The shitshow
# export npm_config_prefix=~/.node_modules
# export PATH="$HOME/.node_modules/bin:$PATH"

# Go path
# export GOPATH=$HOME/go
# export GOBIN=$GOPATH/bin
# export PATH="$HOME/go/bin:$PATH"

# adb-dump() {
#     adb shell uiautomator dump && adb shell cat /sdcard/window_dump.xml | xmllint --format -
# }

# export PATH="$PATH:/usr/local/opt/git/share/git-core/contrib/diff-highlight"
# export PATH="/usr/local/sbin:$PATH"

# export PATH="$PATH:/Users/thrash/.gitbin/git-fuzzy/bin"
# export PATH="$PATH:$HOME/.config/emacs/bin"

export BAT_THEME=gruvbox-dark

export DIRENV_LOG_FORMAT=
eval "$(direnv hook zsh)"
