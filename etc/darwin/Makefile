all: ~/.bash_sessions_disable configure cronjobs /etc/man.conf

# Configure scheduled tasks with launchd(8)
cron = ~/Library/LaunchAgents
jobs = $(notdir $(wildcard *.plist))
$(cron):
	mkdir $@
	chmod 0700 $@
$(cron)/%: %
	perl -pe 's:<string>\K~/:'`printf %s ~`/:g $< > $@
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


# Disable session saving
~/.bash_sessions_disable:
	touch ~/.bash_sessions_disable
	rm -f ~/.bash_sessions


# Make Apple bloatware more tolerable
configure:
	chflags hidden ~/[a-z]*
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
