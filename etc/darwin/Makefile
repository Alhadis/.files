# -*- makefile-gmake -*-

# Section 1: Workstation setup
all: configure cronjobs /etc/man.conf /etc/shells /etc/hosts


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

# Symlink hosts(5) database
/etc/hosts: ~/.files/etc/hosts
	sudo rm -f $@
	sudo ln -sf $< $@
	sudo chmod 0644 $@
	sudo chown -h root:wheel $@
	sudo killall -HUP mDNSResponder

# Update man.conf(5) to enable UTF-8 output and use Homebrew-installed Groff
/etc/man.conf: ../man.conf $(brew-deps)
	[ $@ -ef $< ] \
	|| { grep -qe "$(brew-path)/groff" $@ && sudo ln -f $< $@; }\
	|| { sudo mv $@ $@~orig && sudo ln $< $@; }

# Hard-link shells(5) whitelist
/etc/shells: ../shells
	[ $@ -ef $< ] || sudo ln -f $< $@

# Link directories to share on Soulseek
soulseek: /usr/local/share/soulseek
/usr/local/share/soulseek: ~/.files/var/share/soulseek
	rm -f $@
	ln -s $^ $@
~/.files/var/share/soulseek:
	mkdir -p $@
	ln -s ~/Music $@
	ln -s /Volumes/Films/Lossless $@/Bandcamp
	ln -s /Volumes/Films/Movies   $@/Movies
	ln -s /Volumes/Films/TV       $@/TV

# Make Apple bloatware more tolerable
configure:
	sudo chpass -s /bin/sh Alhadis
	sudo pmset -a lidwake 1
	sudo pmset -a lessbright 0
	sudo pmset -a proximitywake 0
	sudo scutil --set ComputerName  Stormbringer
	sudo scutil --set LocalHostName Stormbringer
	sudo scutil --set HostName      Stormbringer.gateway
	cd /Library/Preferences/SystemConfiguration \
		&& sudo defaults write ./com.apple.smb.server NetBIOSName -string Stormbringer \
		&& sudo defaults write ./com.apple.smb.server ServerDescription -string Stormbringer
	sudo systemsetup -setrestartfreeze on
	defaults write -g AppleInterfaceStyleSwitchesAutomatically -bool false
	defaults write -g AppleLanguages -array en-AU ar-AU ja-AU ru-AU
	defaults write -g AppleMeasurementUnits Centimeters
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
	defaults write com.apple.Accessibility KeyRepeatDelay -float 0.2
	defaults write com.apple.Accessibility KeyRepeatEnabled -int 1
	defaults write com.apple.Accessibility KeyRepeatInterval -float 0.08
	test -x /usr/local/bin/bash || brew install bash
	defaults write com.apple.Terminal Shell /usr/local/bin/bash
	defaults write com.apple.Terminal ShowLineMarks -bool false
	defaults write com.apple.Terminal 'Default Window Settings' Pro
	defaults write com.apple.Terminal 'Startup Window Settings' Pro
	defaults write com.apple.Preview PVSupressPDFCroppingAlert -bool true
	defaults write com.apple.dock autohide -bool false
	defaults write com.apple.dock launchanim -bool false
	defaults write com.apple.dock magnification -bool false
	defaults write com.apple.dock mineffect scale
	defaults write com.apple.dock show-recents -bool false
	defaults write com.apple.dock tilesize -float 82
	defaults write com.apple.finder DisableAllAnimations -bool true
	defaults write com.apple.finder FavoriteTagNames -array
	defaults write com.apple.finder FXPreferredViewStyle clmv
	defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
	defaults write com.apple.finder NewWindowTarget PfDe
	defaults write com.apple.finder NewWindowTargetPath "file://$${HOME}/Desktop/"
	defaults write com.apple.finder QuitMenuItem -bool true
	defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
	defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true
	defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
	defaults write com.apple.finder ShowRecentTags -bool false
	defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true
	defaults write com.apple.finder WarnOnEmptyTrash -bool false
	defaults write com.apple.digihub com.apple.digihub.cd.music.appeared   -dict action -bool true
	defaults write com.apple.digihub com.apple.digihub.cd.picture.appeared -dict action -bool true
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

# Reset Dock icons
dock:
	defaults write com.apple.dock persistent-apps -array
	for app in \
		'/Applications/Firefox.app' \
		'/Applications/Atom Beta.app' \
		'/System/Applications/Utilities/Terminal.app' \
	; do defaults write com.apple.dock persistent-apps -array-add \
		"<dict>\
			<key>tile-data</key>\
			<dict>\
				<key>file-data</key>\
				<dict>\
					<key>_CFURLStringType</key>\
					<integer>0</integer>\
					<key>_CFURLString</key>\
					<string>$$app/</string>\
				</dict>\
			</dict>\
		</dict>"; \
	done && killall Dock



# Section 2: Periodic file backups
backups: notes-backup assets-backup


# Backup Notes.app storage
notes-backup = ~/Downloads/group.com.apple.notes
notes-backup: $(notes-backup).tgz
$(notes-backup).tgz: $(notes-backup).tar
	$(call compress-tarball,Notes.app storage backup)

$(notes-backup).tar: ~/Library/Group\ Containers/group.com.apple.notes
	$(call create-tarball)



# Shamelessly copy commercial/proprietary assets to use on non-Apple systems
assets = ~/Downloads/Assets
assets-backup: $(assets) dicts-backup desktops-backup
$(assets):
	mkdir -p $@


# Downloaded language dictionaries
dicts-source = /System/Library/AssetsV2/com_apple_MobileAsset_DictionaryServices_dictionaryOSX
dicts-backup = $(assets)/dictionary-assets
dicts-backup: $(assets) $(dicts-backup).tgz
$(dicts-backup).tgz: $(dicts-backup).tar
	$(call compress-tarball,Language dictionaries)

$(dicts-backup).tar: $(dicts-source)
	$(call create-tarball)


# High-res/dynamic HEIC wallpapers
desktops-backup: desktops-system desktops-mobile

# Pre-installed wallpapers
desktops-system = $(assets)/Desktop\ Pictures
desktops-system: $(assets) $(desktops-system).tgz
$(desktops-system).tgz: $(desktops-system).tar
	$(call compress-tarball,Pre-installed wallpapers)

$(desktops-system).tar: /System/Library/Desktop\ Pictures
	$(call create-tarball)


# Downloadable wallpapers
desktops-mobile = $(assets)/com.apple.mobileAssetDesktop
desktops-mobile: $(assets) $(desktops-mobile).tgz
$(desktops-mobile).tgz: $(desktops-mobile).tar
	$(call compress-tarball,Downloaded wallpaper archive)

$(desktops-mobile).tar: ~/Library/Application\ Support/com.apple.mobileAssetDesktop
	$(call create-tarball)


# Don't regenerate tarballs if compressed versions already exist
.INTERMEDIATE: \
	$(notes-backup).tar \
	$(dicts-backup).tar \
	$(desktops-system).tar \
	$(desktops-mobile).tar



# Section 3: Canned recipe definitions

# Generate an uncompressed archive of a directory
define create-tarball
dirs=`printf %s "$^" | tr -Cd / | wc -c`; \
bsdtar --acls --fflags --xattrs --mac-metadata --strip-components="`expr $$dirs`" -cvf "$@" "$^"
touch -cr "$^" "$@"
sha256sum "$@"
endef

# Perform the final steps of generating a compressed tarball
define compress-tarball
gzip -fS .tgz "$^"
@ unset old new; \
if test -f "$@"; then \
	old=`sha256sum 2>/dev/null "$@"`; \
	new=`sha256sum 2>/dev/null "$^.tgz"`; \
fi; \
if [ "$${new%% *}" = "$${old%% *}" ] && [ -n "$$old" ]; then \
	printf '%s already up-to-date\n' "$(or $(1),Archive)"; \
	touch -cr "$^.tgz" "$@"; \
	rm -f "$^.tgz"; \
else \
	mv -vf "$^.tgz" "$@"; \
	chmod -w "$@"; \
fi
endef
