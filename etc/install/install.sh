#!/bin/bash


#=======================================================================
# Setup new workstation.
#=======================================================================
cd ~/.files || exit 1;


# Disable session saving
[ -e ~/.bash_sessions_disable ] ||
	touch ~/.bash_sessions_disable &&
	rm -rf ~/.bash_sessions;


# Silence login message at startup
[ -e ~/.hushlogin ] || touch ~/.hushlogin


# Link SSH config
[ -d ~/.ssh ] ||
	mkdir ~/.ssh &&
	chmod 0700 ~/.ssh;
[ -L ~/.ssh/config ] ||
	ln -sfv ~/.files/etc/ssh-config ~/.ssh/config;


# Link manpage config
[ -L /private/etc/man.conf ] ||
	sudo ln -sf ~/.files/etc/man.conf /private/etc/man.conf


# Install Homebrew formulae/casks
cd $(dirname "$0") && brew bundle


# Install software managed by other package-managers
source package-lists.sh
eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"
npm -g install $npm_modules && unset npm_modules;
gem install -N $ruby_gems   && unset ruby_gems;
pip install $pip_packages   && unset pip_packages;
cpan -Ti $cpan_modules      && unset cpan_modules;


# Post-install odds-and-ends
chflags hidden ~/perl5
brew doctor


# Shackle NPM v5 to the corner
[ -e ~/.npmrc ] ||
	touch ~/.npmrc &&
	npm config set package-lock false &&
	npm config set save false


# Link dotfiles
for name in bash_profile curlrc emacs.d gitconfig inputrc vimrc; do
	[ -L ~/.$name ] || ln -Fsv ~/.files/.$name ~/.$name;
done



#=======================================================================
# User preferences
# Source: https://github.com/mathiasbynens/dotfiles/blob/master/.macos
#=======================================================================

# Disable the “Are you sure you want to open this application?” dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false


# Disable press-and-hold for keys in favour of key repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false


# Set a blazingly fast keyboard repeat rate
defaults write NSGlobalDomain InitialKeyRepeat -int 12


# Expand the following File Info panes:
# “General”, “Open with”, and “Sharing & Permissions”
defaults write com.apple.finder FXInfoPanesExpanded -dict \
	General -bool true \
	OpenWith -bool true \
	Privileges -bool true


# Don't animate opening applications from the Dock
defaults write com.apple.dock launchanim -bool false


# Enable full keyboard access for all controls (e.g., enable Tab in modal dialogues)
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3


# Enable spring loading for directories, and remove the delay from doing so
defaults write NSGlobalDomain com.apple.springing.enabled -bool true
defaults write NSGlobalDomain com.apple.springing.delay -float 0


# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false


# Finder: disable window animations and Get Info animations
defaults write com.apple.finder DisableAllAnimations -bool true


# Finder: allow quitting via ⌘ + Q; doing so will also hide desktop icons
defaults write com.apple.finder QuitMenuItem -bool true


# Restart automatically if the computer freezes
sudo systemsetup -setrestartfreeze on


# Increase window resize speed for Cocoa applications
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001


# iTunes: Stop iPods from automatically syncing when plugged in
defaults write com.apple.iTunes dontAutomaticallySyncIPods -bool true



# Reset Finder for the changes to take effect
killall Finder
