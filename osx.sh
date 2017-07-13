#!/bin/bash

#==============================================================================
# Source:  https://github.com/mathiasbynens/dotfiles/blob/master/.osx
#==============================================================================


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
