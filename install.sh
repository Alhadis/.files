#!/bin/sh

cd "$HOME"

# Silence is golden
[ -e .hushlogin ] || touch .hushlogin

# Create symlinks in $HOME
dotfiles='
	.bashrc
	.curlrc
	.emacs.d
	.eslintrc.json
	.gitconfig
	.inputrc
	.profile
	.vimrc
'
for i in $dotfiles; do
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
