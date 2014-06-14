# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Theme
set fish_theme robbyrussell

# Aliases
alias sagi="sudo apt-get install"
alias acs="apt-cache search"
alias df="df -h"
alias du="du -h"
alias rh="runhaskell"
alias go="gnome-open"
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

# Marks
set -x MARKPATH $HOME/.marks
function jump
  cd $MARKPATH/$argv or echo "No such mark: $argv"
end

function mark
  mkdir -p $MARKPATH; ln -s (pwd) $MARKPATH/$argv
end

function unmark
  rm -i $MARKPATH/$argv
end

function marks
  ls -l $MARKPATH | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g'
  echo
end

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Example format: set fish_plugins autojump bundler
set fish_plugins git tmux

# Path to your custom folder (default path is $FISH/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

set fish_greeting ""

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish
