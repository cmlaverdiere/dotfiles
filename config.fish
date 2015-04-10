# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Theme
set fish_theme robbyrussell

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
alias emacs="emacs -nw"

# File aliases
alias blog="vim ~/Documents/Misc/blog.txt"
alias books="vim ~/Documents/Misc/books.txt"
alias dreamj="vim ~/Documents/Misc/dreams.txt"
alias fishrc="vim ~/.config/fish/config.fish"
alias ideas="vim ~/Documents/Misc/ideas.txt"
alias robots="vim ~/Documents/Misc/robots.txt"
alias shows="vim ~/Documents/Misc/shows.txt"
alias stask="vim ~/Documents/Misc/school_tasks.txt"
alias todo="vim ~/Documents/Misc/todo.txt"
alias vimrc="vim ~/.vim/vimrc"
alias zshrc="vim ~/.zshrc"

# Editor
set -x EDITOR 'vim'

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

# Mail
function mailme
  echo "$argv" | mail cmlaverdiere@gmail.com
end


# Bangs
function sudo
    if test "$argv" = !!
        eval command sudo $history[1]
    else
        command sudo $argv
    end
end

# fzf
set -x TMPDIR /tmp

function fd
  set tmpf $TMPDIR/fzff.result
  set tmp $TMPDIR/fzf.result
  find . -path '*/\.*' -prune -o -type d -print > $tmpf 2> /dev/null
  cat $tmpf | fzf +m > $tmp
  if [ (cat $tmp | wc -l) -gt 0 ]
    cd (cat $tmp)
  end
end

function fe
  if fzf > $TMPDIR/fzf.result
    vim (cat $TMPDIR/fzf.result)
  end
end

# Middle click on laptop.
synclient TapButton3=3 TapButton2=2

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Example format: set fish_plugins autojump bundler
set fish_plugins git autojump

# Path to your custom folder (default path is $FISH/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

set fish_greeting ""

# Enable core dumps.
ulimit -c unlimited

# Better python shell
# which ipython >> /dev/null; and alias python="ipython"
alias python="ipython"

# Set Path
if status --is-login
  set PATH $PATH /usr/local/include
end

# Load autojump.
if test -f /home/chris/.autojump/share/autojump/autojump.fish
  . /home/chris/.autojump/share/autojump/autojump.fish
end

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish
