echo "Installing dotfiles..."

cp emacs ~/.emacs
cp gitconfig ~/.gitconfig
cp spacemacs ~/.spacemacs
cp tmux.conf ~/.tmux.conf
cp Xdefaults ~/.Xdefaults
cp xinitrc ~/.xinitrc
cp xmobarrc ~/.xmobarrc
cp zshrc ~/.zshrc

mkdir -p ~/.config/fish
cp config.fish ~/.config/fish
cp redshift.conf ~/.config

mkdir -p ~/.vim/syntax
mkdir -p ~/.vim/bundle
cp vimrc ~/.vim/vimrc
cp bebop.vim ~/.vim/syntax
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

mkdir -p ~/.xmonad
cp xmonad.hs ~/.xmonad

mkdir -p ~/.tmuxinator
cp mux/* ~/.tmuxinator

echo "Done."
