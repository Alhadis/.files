#!/bin/sh

#
# install.sh: Quickly setup a new workstation
#
cd "$HOME"

# Silence is golden
[ -e .hushlogin ] || touch .hushlogin

# Create symlinks in $HOME
dotfiles='
	.alacritty.yml
	.bash_logout
	.bashrc
	.curlrc
	.gitconfig
	.inputrc
	.npmrc
	.profile
	.vimrc
	.wgetrc
'
# Link `$HOME/$file` to `$HOME/.files/$file` unless it's already symlinked
for file in $dotfiles; do [ -h $file ] || {
	ln -sf .files/$file
	printf 'Symlinked: %s -> %s\n' $file .files/$file;
}
done; unset file


# Copy SSH config
[ ! -s .ssh/config ] && {
	cp .files/etc/ssh-config .ssh/config
	printf 'Copied: %s -> %s\n' .files/etc/ssh-config .ssh/config
}

# Tighten permissions on `~/.ssh' directory
chmod go-rwx .ssh/*


# Link Konsole profile
command -v konsole 2>&1 >/dev/null && [ -d .kde4/share/apps ] && {
	[ -d .kde4/share/apps/konsole ] || {
		rm -rf .kde4/share/apps/konsole
		ln -sf .files/etc/konsole .kde4/share/apps/konsole
	}
	
	# Install Menlig font
	fonts=/usr/local/share/fonts
	[ -d "$fonts" ] && [ ! -e "$fonts/Menlig.otf" ] &&
		cp .files/share/desktop/Menlig.otf "$fonts/Menlig.otf"
	unset fonts
}


# Link Inkscape preferences
[ -d .config/inkscape ] && {
	ln -sf ~/.files/etc/inkscape/preferences.xml .config/inkscape/
}


# Disable blinking cursor in Gnome Terminal
command -v gsettings 2>&1 >/dev/null && [ "$DISPLAY" ] && {
	gsettings set org.gnome.desktop.interface cursor-blink false
}

# Configure Xfce4
command -v startxfce4 2>&1 >/dev/null && [ "$DISPLAY" ] && {
	xfconf-query -nt int -c keyboards -p /Default/KeyRepeat/Delay -s 200
	xfconf-query -nt int -c keyboards -p /Default/KeyRepeat/Rate  -s 60
	xfconf-query -nt int -c xfwm4     -p /general/workspace_count -s 1
	xfconf-query -nt string -c xfce4-desktop \
		-p /backdrop/screen0/monitorLVDS-1/workspace0/last-image \
		-s ~/.files/share/desktop/wallpaper.jpg
}


# Perform macOS-specific install stuff
case `uname -s` in [Dd]arwin) cd ~/.files/etc/darwin && make ;; esac


# Install Node programs
command -v npm 2>&1 >/dev/null && {
	npm=`npm --global root`
	install=
	modules='
		acorn
		asar
		babel-eslint
		browserify
		buble
		c8
		chai
		clean-css-cli
		coffeelint
		cson
		electron
		eslint
		guess-sig
		hint
		jsdoc
		json
		karma
		karma-coverage
		karma-chrome-launcher
		karma-firefox-launcher
		karma-mocha-reporter
		karma-mocha
		mocha
		mocha-when
		nyc
		ppjson
		prettier
		rollup
		semver
		terser
		tslint
		ts-node
		typedoc
		typescript
	'
	case `uname -s` in *BSD)
		modules=`printf %s "$modules" | grep -v electron` ;;
	esac
	for module in $modules; do
		[ -d "$npm/$module" ] || install="$install $module";
	done
	[ "$install" ] && npm --global install $install
	unset install module
}
