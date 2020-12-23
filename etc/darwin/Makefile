all: configure /etc/man.conf /etc/shells

# Upgrade antique programs that ship with macOS
brew-path = $(shell brew --prefix)/bin
brew-deps = $(addprefix $(brew-path)/,groff grap less)
$(brew-deps):
	HOMEBREW_NO_AUTO_UPDATE=1 brew install $(@F)

# Update man.conf(5) to enable UTF-8 output and use Homebrew-installed Groff
/etc/man.conf: ../man.conf $(brew-deps)
	[ $@ -ef $< ] \
	|| { grep -qe "$(brew-path)/groff" $@ && sudo ln -f $< $@; }\
	|| { sudo mv $@ $@~orig && sudo ln $< $@; }

# Hard-link shells(5) whitelist
/etc/shells: ../shells
	[ $@ -ef $< ] || sudo ln -f $< $@


# Make Apple bloatware more tolerable
configure:
	sudo pmset -a lidwake 1
	sudo pmset -a lessbright 0
	sudo pmset -a proximitywake 0
	sudo systemsetup -setrestartfreeze on
	defaults write -g AppleShowAllExtensions -bool true
	defaults write -g ApplePressAndHoldEnabled -bool false
	defaults write -g KeyRepeat -int 1
	defaults write -g InitialKeyRepeat -int 12
	defaults write -g com.apple.springing.enabled -bool true
	defaults write -g com.apple.springing.delay -float 0
	defaults write com.apple.Accessibility KeyRepeatDelay -float 0.5
	defaults write com.apple.Accessibility KeyRepeatEnabled -int 1
	defaults write com.apple.Accessibility KeyRepeatInterval -float 0.08
	defaults write com.apple.dock autohide -bool false
	defaults write com.apple.dock launchanim -bool false
	defaults write com.apple.dock show-recents -bool false
	defaults write com.apple.finder DisableAllAnimations -bool true
	defaults write com.apple.finder FavoriteTagNames -array
	defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
	defaults write com.apple.finder NewWindowTarget PfDe
	defaults write com.apple.finder NewWindowTargetPath "file://$${HOME}/Desktop/"
	defaults write com.apple.finder NSAutomaticCapitalizationEnabled -bool false
	defaults write com.apple.finder NSAutomaticDashSubstitutionEnabled -bool false
	defaults write com.apple.finder NSAutomaticPeriodSubstitutionEnabled -bool false
	defaults write com.apple.finder NSAutomaticQuoteSubstitutionEnabled -bool false
	defaults write com.apple.finder NSAutomaticSpellingCorrectionEnabled -bool false
	defaults write com.apple.finder NSAutomaticTextCompletionEnabled -bool false
	defaults write com.apple.finder QuitMenuItem -bool true
	defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
	defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true
	defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
	defaults write com.apple.finder ShowRecentTags -bool false
	defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true
	defaults write com.apple.universalaccess closeViewScrollWheelModifiersInt -int 393216
	defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true
	defaults write com.apple.universalaccess closeViewZoomFollowsFocus -bool true
	defaults write com.apple.universalaccess reduceMotion -bool true
