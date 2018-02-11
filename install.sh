#!/bin/sh

cd "$HOME"

# Silence is golden
[ -e .hushlogin ] || touch .hushlogin

# Create symlinks in $HOME
for i in .bashrc .curlrc .emacs.d .gitconfig .inputrc .profile .vimrc; do
	[ -e $i ] || {
		ln -s .files/$i
		printf 'Symlinked: %s -> %s\n' $i .files/$i
	}
done
unset i

# Tell NPM to pull its head in
command -v npm 2>&1 >/dev/null && {
	npm config set package-lock false
	npm config set save false
}

# Disable blinking cursor in Gnome Terminal
command -v gsettings 2>&1 >/dev/null && {
	gsettings set org.gnome.desktop.interface cursor-blink false
}
