#!/bin/sh

#
# install.sh: Quickly setup a new workstation
#
force= npm= sudo=

# Parse command-line switches
while [ -n "$1" ]; do case $1 in

	# Force replacement of existing files
	-f|--force)
		force=1 ;;

	# Install global NPM modules
	-n|--npm)
		npm=1 ;;

	# Remove global NPM modules
	--uninstall-npm)
		npm=2 ;;

	# Double-dash: terminate option parsing
	--) shift; break ;;

	# Invalid option: exit with usage message
	--* | -?*)
		printf "Usage: ${0##*/} [-f|--force]\n"
		printf "       ${0##*/} [-n|--npm|--uninstall-npm]\n"
		exit 1 ;;

	*) break ;;

esac; shift
done


# Resolve command for running as superuser
command -v sudo 2>&1 >/dev/null && sudo=sudo
command -v doas 2>&1 >/dev/null && sudo=doas


# Install/uninstall Node programs
[ "$npm" ] && {
	command -v npm 2>&1 >/dev/null || {
		printf 'Aborted: `npm` executable not found\n'
		exit 1
	}
	case $npm in
		2) cmd="npm -g uninstall" ;;
		*) cmd="npm -g install"   ;;
	esac
	modules='
		@alhadis/eslint-config
		acorn
		asar
		babel-eslint
		browserify
		buble
		chai
		clean-css-cli
		coffeelint
		cson
		electron
		eslint
		guess-sig
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
	# Unlist Electron if running on OpenBSD; not yet ported
	[ "`uname -s`" = OpenBSD ] &&
		modules=`printf %s "$modules" | grep -v 'electron$'`

	# Should we install as superuser?
	[ -w "`npm -g root`" ] || cmd="$sudo $cmd"

	$cmd $modules
	exit
}


cd "$HOME"

# Silence is golden
[ -e .hushlogin ] || touch .hushlogin

# Create symlinks in $HOME
dotfiles='
	.alacritty.yml
	.bash_logout
	.bashrc
	.curlrc
	.emacs.d
	.gitconfig
	.inputrc
	.npmrc
	.profile
	.vimrc
'
for file in $dotfiles; do
	[ -e $file ] && [ ! "$force" ] || {
		# Link `$HOME/$file` to `$HOME/.files/$file` unless it's already symlinked
		[ ! -h $file ] && {
			ln -sf .files/$file &&
			printf 'Symlinked: %s -> %s\n' $file .files/$file;
		};
	};
done; unset file

# Link Emacs to snippets repository, assuming usual project paths
[ ! -e ~/.emacs.d/snippets ] && [ -d ~/Labs/YASR/snippets ] && {
	ln -s ~/Labs/YASR/snippets ~/.emacs.d/snippets
	printf 'Symlinked: %s -> %s\n' ~/Labs/YASR/snippets ~/.emacs.d/snippets
}

# Copy SSH config
[ ! -s .ssh/config ] && {
	cp .files/etc/ssh-config .ssh/config
	printf 'Copied: %s -> %s\n' .files/etc/ssh-agent .ssh/config
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


# Tell NPM to pull its head in
have npm && npm config set globalconfig ~/.files/etc/npmrc

# Fix ownership of `~/.npm' and its contents
name=`id -un`
find ~/.npm \! \( -user "$name" \) |
while read path; do
	$sudo chown -h "$name" "$path"
done; unset name


# Disable blinking cursor in Gnome Terminal
command -v gsettings 2>&1 >/dev/null && [ "$DISPLAY" ] && {
	gsettings set org.gnome.desktop.interface cursor-blink false
}

# Configure Xfce4
command -v startxfce4 2>&1 >/dev/null && [ "$DISPLAY" ] && {
	xfconf-query -c keyboards -p /Default/KeyRepeat/Delay -s 200
	xfconf-query -c keyboards -p /Default/KeyRepeat/Rate  -s 60
	xfconf-query -c xfwm4     -p /general/workspace_count -s 1
	xfconf-query -c xfce4-desktop \
		-p /backdrop/screen0/monitor0/workspace0/last-image \
		-s ~/.files/share/desktop/wallpaper.jpg
}


# OS-specific configuration
case `uname -s` in

	# macOS: Make Apple bloatware more tolerable
	[Dd]arwin)
		# Disable session saving
		[ -e ~/.bash_sessions_disable ] || {
			touch ~/.bash_sessions_disable
			rm -f ~/.bash_sessions
		};

		# Hide default Perl directory
		[ -d ~/perl5 ] && chflags hidden ~/perl5

		# Everything else
		sudo systemsetup -setrestartfreeze on
		defaults write com.apple.LaunchServices LSQuarantine -bool false
		defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
		defaults write NSGlobalDomain InitialKeyRepeat -int 12
		defaults write NSGlobalDomain AppleKeyboardUIMode -int 3
		defaults write NSGlobalDomain com.apple.springing.enabled -bool true
		defaults write NSGlobalDomain com.apple.springing.delay -float 0
		defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
		defaults write com.apple.dock launchanim -bool false
		defaults write com.apple.iTunes dontAutomaticallySyncIPods -bool true
		defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
		defaults write com.apple.finder DisableAllAnimations -bool true
		defaults write com.apple.finder QuitMenuItem -bool true
	;;
esac
