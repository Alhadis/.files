all: configure cronjobs /etc/man.conf /etc/shells

# Configure scheduled tasks with launchd(8)
cron = ~/Library/LaunchAgents
jobs = $(notdir $(wildcard *.plist))
$(cron):
	mkdir $@
	chmod 0700 $@
$(cron)/%: %
	convert-plist < $< > $@
	chmod 0644 $@
cronjobs: $(cron) $(addprefix $(cron)/,$(jobs))

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
	sudo chpass -s /bin/sh Alhadis
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
	defaults write -g com.apple.swipescrolldirection -bool false
	defaults write -g NSAutomaticCapitalizationEnabled -bool false
	defaults write -g NSAutomaticDashSubstitutionEnabled -bool false
	defaults write -g NSAutomaticPeriodSubstitutionEnabled -bool false
	defaults write -g NSAutomaticQuoteSubstitutionEnabled -bool false
	defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false
	defaults write -g NSAutomaticTextCompletionEnabled -bool false
	defaults write -g NSToolbarTitleViewRolloverDelay -float 0.0
	defaults write com.apple.Accessibility KeyRepeatDelay -float 0.5
	defaults write com.apple.Accessibility KeyRepeatEnabled -int 1
	defaults write com.apple.Accessibility KeyRepeatInterval -float 0.08
	test -x /usr/local/bin/bash || brew install bash
	defaults write com.apple.Terminal Shell /usr/local/bin/bash
	defaults write com.apple.Terminal ShowLineMarks -bool false
	defaults write com.apple.Terminal 'Default Window Settings' Pro
	defaults write com.apple.Terminal 'Startup Window Settings' Pro
	defaults write com.apple.dock autohide -bool false
	defaults write com.apple.dock launchanim -bool false
	defaults write com.apple.dock mineffect scale
	defaults write com.apple.dock show-recents -bool false
	defaults write com.apple.dock tilesize -float 82
	defaults write com.apple.finder DisableAllAnimations -bool true
	defaults write com.apple.finder FavoriteTagNames -array
	defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
	defaults write com.apple.finder NewWindowTarget PfDe
	defaults write com.apple.finder NewWindowTargetPath "file://$${HOME}/Desktop/"
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
	defaults write com.apple.touchbar.agent {}
	defaults write com.apple.touchbar.agent PresentationModeFnModes -dict \
		appWithControlStrip functionKeys \
		functionKeys fullControlStrip
	defaults write com.apple.touchbar.agent PresentationModeGlobal appWithControlStrip
	defaults write com.apple.touchbar.agent PresentationModePerApp -dict \
		com.github.atom functionKeys \
		org.gnu.Emacs functionKeys
