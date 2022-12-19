#!/bin/sh

#
# install.sh: Quickly setup a new workstation
# shellcheck disable=SC2317
#

# Assume that systems without doas(1) have sudo(1) installed by default
root_cmd='sudo'
command -v doas >/dev/null 2>&1 && root_cmd='doas' || doas()(sudo "$@")

# Re-execute with superuser privileges if they've been requested
if [ "$1" = --root ]; then
	shift
	exec $root_cmd "$0" "$@"
fi

cd ~ || exit

# Silence is golden
[ -e .hushlogin ] || touch .hushlogin

# Create symlinks in $HOME
dotfiles='
	.alacritty.yml
	.bash_logout
	.bash_profile
	.curlrc
	.exrc
	.gitconfig
	.inputrc
	.irbrc
	.manpath
	.npmrc
	.profile
	.vimrc
	.wgetrc
	.zshenv
	.zshrc
'
# Link `$HOME/$file` to `$HOME/.files/$file` unless it's already symlinked
# shellcheck disable=SC2086
for file in $dotfiles; do [ -h $file ] || {
	ln -sf .files/$file
	printf 'Symlinked: %s -> %s\n' $file .files/$file;
}
done; unset file


# Link SSH config
[ ! -s .ssh/config ] && {
	ln .files/etc/ssh-config .ssh/config
	printf 'Linked: %s -> %s\n' .files/etc/ssh-config .ssh/config
}

# Tighten permissions on `~/.ssh' directory
chmod go-rwx .ssh/*


# Link Konsole profile
command -v konsole >/dev/null 2>&1 && [ ! -h .local/share/konsole ] && {
	[ -d .local/share ] || mkdir -p .local/share
	rm -rf .local/share/konsole
	ln -sf ~/.files/etc/konsole .local/share/konsole
}

# Install editor font
if [ "`uname -s`" = Darwin ]; then
	mkdir -p ~/Library/Fonts
	ln -f .files/share/desktop/Menlig.otf ~/Library/Fonts
else
	fonts=/usr/local/share/fonts
	[ -d "$fonts" ] && [ ! -e "$fonts/Menlig.otf" ] &&
		doas cp .files/share/desktop/Menlig.otf "$fonts/Menlig.otf"
	unset fonts
fi


# Link Inkscape preferences
[ -d .config/inkscape ] && {
	ln -sf ~/.files/etc/inkscape/preferences.xml .config/inkscape/
}

# Link youtube-dl(1) configuration
command -v youtube-dl >/dev/null 2>&1 && {
	[ -d .config/youtube-dl ] || mkdir -p .config/youtube-dl
	ln -sf ~/.files/etc/youtube-dl.conf .config/youtube-dl/config
}

# Link Troff macros
# shellcheck disable=SC2167,SC2165
for tmac in /usr/local /usr; do
	cd "$tmac/share/groff/site-tmac" || continue
	for tmac in ~/.files/share/tmac/*; do
		printf 'Linked: %s -> %s\n' "$tmac" "$PWD"
		ln -sf "$tmac" .
	done
	cd - >/dev/null 2>&1 && break
done; unset tmac

# Link Contour configuration
command -v contour >/dev/null 2>&1 && {
	[ -d .config/contour ] || mkdir -p .config/contour
	ln -sf ~/.files/etc/contour.yml .config/contour/
}

# Disable blinking cursor in Gnome Terminal
command -v gsettings >/dev/null 2>&1 && [ "$DISPLAY" ] && {
	gsettings set org.gnome.desktop.interface cursor-blink false
}

# Configure Xfce4
command -v startxfce4 >/dev/null 2>&1 && [ "$DISPLAY" ] && {
	xfconf-query -nt int -c keyboards -p /Default/KeyRepeat/Delay -s 200
	xfconf-query -nt int -c keyboards -p /Default/KeyRepeat/Rate  -s 60
	xfconf-query -nt int -c xfwm4     -p /general/workspace_count -s 1
	xfconf-query -nt string -c xfce4-desktop \
		-p /backdrop/screen0/monitorLVDS-1/workspace0/last-image \
		-s ~/.files/share/desktop/wallpaper.jpg
	xfconf-query -nt int  -c xfce4-screensaver -p /lock/saver-activation/delay -s 5
	xfconf-query -nt int  -c xfce4-screensaver -p /saver/idle-activation/delay -s 10
	xfconf-query -nt bool -c xfce4-screensaver -p /saver/fullscreen-inhibit    -s true
	xfconf-query -nt string -c xfce4-keyboard-shortcuts \
		-p /commands/custom/Super_L \
		-s xfce4-popup-applicationsmenu

	# Symlink xfce4-terminal(1) configuration
	[ -h .config/xfce4/terminal ] || {
		rm -rf .config/xfce4/terminal
		ln -sf ~/.files/etc/xfce4/terminal .config/xfce4/terminal
		chmod -w .config/xfce4/terminal/accels.scm
	}
}


# Perform platform-specific install stuff
case `uname -s` in
	OpenBSD)
		doas ln -sf ~/.files/etc/sysctl.conf    /etc
		doas ln -sf ~/.files/etc/wsconsctl.conf /etc
	;;
	[Dd]arwin)
		cd ~/.files/etc/darwin && make
	;;
esac


exit
# XXX: Leave NPM package installation alone for now, it slows everything else
# down (such as when running `install.sh` to repair missing/deleted symlinks).

# Install Node programs
command -v npm >/dev/null 2>&1 && {
	npm=`npm --global root`
	install=
	modules='
		acorn
		babel-eslint
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
	# shellcheck disable=SC2086
	[ "$install" ] && npm --global install $install
	unset install module
}
