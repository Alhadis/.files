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

# Ubuntu: Fix Chrome launcher so it doesn't randomly freeze the system
case `uname -s` in Linux)
	flag=--disable-background-networking
	launcher=/usr/share/applications/google-chrome.desktop
	[ -f $launcher ] && ! grep -q -- $flag $launcher && {
		printf 'Updating google-chrome launcher to use `%s`.\n' $flag
		sudo sed -i -e "s,Exec=/usr/bin/google-chrome-stable,& $flag," $launcher &&
		printf 'Successfully updated %s\n' $launcher
		cat <<-EOF
		Some additional steps also need to be performed:
		1. In chrome://settings (under "Advanced"):
		   - Disable "Use hardware acceleration when available"
		   - Disable "Continue running background apps when Google Chrome is closed"
		2. In chrome://flags:
		   - Disable "GPU Rasterization"
		More info: https://askubuntu.com/a/894683
		EOF
	}
	unset flag launcher
esac
