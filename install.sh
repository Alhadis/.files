#!/bin/sh

#
# install.sh: Quickly setup a new workstation
#
force=

# Parse command-line switches
while [ -n "$1" ]; do case $1 in

	# Force replacement of existing files
	-f|--force)
		force=1 ;;

	# Usual copy+pasta
	--) shift; break ;;
	--* | -?*) echo "Usage: ${0##*/} [-f|--force]"; exit 1 ;;
	*) break ;;

esac; shift
done


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
for file in $dotfiles; do
	[ -e $file ] && [ ! "$force" ] || {
		# Link `$HOME/$file` to `$HOME/.files/$file` unless it's already symlinked
		[ ! -h $file ] && {
			ln -sf .files/$file &&
			printf 'Symlinked: %s -> %s\n' $file .files/$file;
		};
	};
done; unset file

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
command -v npm 2>&1 >/dev/null && {
	npm config set package-lock false
	npm config set save false
}

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

	# Ubuntu: Fix Chrome launcher so it doesn't randomly freeze the system
	Linux)
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
	;;


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
