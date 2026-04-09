# -*- makefile-gmake -*-

# Section 1: Workstation setup
all: configure cronjobs /etc/man.conf /etc/shells /etc/hosts


# Configure scheduled tasks with launchd(8)
cron = ~/Library/LaunchAgents
jobs = $(notdir $(wildcard com.alhadis.*.plist))
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
configure: applescript dock terminal
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
	defaults write com.apple.DictionaryServices DCSPreferenceVersion -int 7
	defaults write com.apple.DictionaryServices DCSActiveDictionaries -array \
		com.apple.dictionary.ODE \
		com.apple.dictionary.OTE \
		com.apple.dictionary.AppleDictionary \
		~/Library/Dictionaries/*.dictionary \
		com.apple.dictionary.NOAD
	defaults write com.apple.menuextra.textinput ModeNameVisible -bool false
	defaults -currentHost write com.apple.controlcenter BatteryShowPercentage  -bool true
	defaults -currentHost write com.apple.controlcenter AirplayRecieverEnabled -bool false
	defaults -currentHost write com.apple.controlcenter ScreenMirroring        -int 0
	defaults -currentHost write com.apple.controlcenter Battery                -int 22
	defaults -currentHost write com.apple.controlcenter Bluetooth              -int 18
	defaults -currentHost write com.apple.controlcenter Clock                  -int 18
	defaults -currentHost write com.apple.controlcenter WiFi                   -int 18
	defaults -currentHost write com.apple.controlcenter Sound                  -int 16
	defaults -currentHost write com.apple.controlcenter Display                -int 24
	defaults -currentHost write com.apple.controlcenter NowPlaying             -int 24
	defaults -currentHost write com.apple.controlcenter UserSwitcher           -int 24
	defaults -currentHost write com.apple.controlcenter KeyboardBrightness     -int 24
	defaults -currentHost write com.apple.controlcenter AccessibilityShortcuts -int 24
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

# Configure macOS's terminal emulator
terminal:
	test -x /usr/local/bin/bash || brew install bash
	defaults write com.apple.Terminal Shell /usr/local/bin/bash
	defaults write com.apple.Terminal ShowLineMarks -bool false
	defaults write com.apple.Terminal StartupAction -int 1
	defaults write com.apple.Terminal NSNavLastRootDirectory ~/Desktop
	defaults delete 2>/dev/null com.apple.Terminal CommandHistory   || :
	defaults delete 2>/dev/null com.apple.Terminal PreviousCommands || :
	defaults delete 2>/dev/null com.apple.Terminal 'Window Groups'  || :
	defaults write com.apple.Terminal 'Default Window Settings' Pro
	defaults write com.apple.Terminal 'Startup Window Settings' Pro
	defaults write com.apple.Terminal 'Window Settings' -dict
	for i in \
		/System/Applications/Utilities/Terminal.app/Contents/Resources/Initial\ Settings/*.terminal \
		~/.files/etc/darwin/*.terminal \
	; do defaults write com.apple.Terminal 'Window Settings' -dict-add \
		"`basename "$$i" .terminal`" \
		"`convert-plist -x "$$i" | sed '1,/^<plist/d; /^<\/plist>/d'`"; \
	done



# Section 2: AppleScript libraries and configuration
applescript: scripting-additions script-libraries script-editor


# Download third-party extensions
scripting-additions: \
	~/Library/ScriptingAdditions/Property\ List\ Tools.osax \
	~/Library/ScriptingAdditions/List\ &\ Record\ Tools.osax \
	~/Library/ScriptingAdditions/XML\ Tools.osax

~/Library/ScriptingAdditions/List\ &\ Record\ Tools.osax: /tmp/ListRecordTools1.0.6.dmg
	$(call extract-dmg,09f5ca4bec16923f21ff02610f2541d2)

~/Library/ScriptingAdditions/Property\ List\ Tools.osax: /tmp/PListTools1.0.7.dmg
	$(call extract-dmg,dd8b54729312d354a31414c2ec4e103a)

~/Library/ScriptingAdditions/XML\ Tools.osax: /tmp/XMLTools2.9.4.dmg
	$(call extract-dmg,bb7c4367182263409fc2c5f0210c5a93)

/tmp/ListRecordTools1.0.6.dmg:
	$(call download-asset,\
		http://latenightsw.com/archives/ListRecordTools1.0.6.dmg,\
		80da1d69e071a00ff5c8ceadd7a55ddf1b37e989ab14b472c354e0ba3e2e67e5,\
		20161021130804)

/tmp/PListTools1.0.7.dmg:
	$(call download-asset,\
		http://latenightsw.com/archives/PListTools1.0.7.dmg,\
		30986f95941bf2c7c3b3670df71903cb10b491e3c5f4858c2b0cd8d5a7aec0f8,\
		20161021130750)

/tmp/XMLTools2.9.4.dmg:
	$(call download-asset,\
		http://www.latenightsw.com/archives/XMLTools2.9.4.dmg,\
		e25326aa1440e12eb166c921ce14be117a18b2a50007b6c58df60878063b4a42,\
		20161021125619)


# Download third-party AppleScript libraries
script-libraries: \
	~/Library/Script\ Libraries/Dialog\ Toolkit.scptd \
	~/Library/Script\ Libraries/FileManagerLib.scptd \
	~/Library/Script\ Libraries/Metadata\ Lib.scptd

~/Library/Script\ Libraries/Dialog\ Toolkit.scptd: /tmp/Dialog_Toolkit.zip
	$(call extract-zip,1b32dd7e1b27c5e37300dad5c2a721bd)

~/Library/Script\ Libraries/FileManagerLib.scptd: /tmp/FileManagerLib_stuff.zip
	$(call extract-zip,e1db3c24d780e3b80833d03d9e2bf474)

~/Library/Script\ Libraries/Metadata\ Lib.scptd: /tmp/Metadata_Lib_stuff.zip
	$(call extract-zip,f2da9e083ff2607fed15a3ebce5e7ad9)

/tmp/Dialog_Toolkit.zip:
	$(call download-asset,\
		http://macosxautomation.com/applescript/apps/Dialog_Toolkit.zip,\
		865c0d4dbb895d753de2018d98d717f3c477ef8f08d7cbba3c018a7ea8e73bf0,\
		20160705042247)

/tmp/Metadata_Lib_stuff.zip:
	$(call download-asset,\
		https://www.macosxautomation.com/applescript/apps/Metadata_Lib_stuff.zip,\
		e7074b56df62c8ee42528247dafa00f69c99ccdd1011cac56fb2f08383dbae2d,\
		20180210193730)

/tmp/FileManagerLib_stuff.zip:
	$(call download-asset,\
		http://www.macosxautomation.com/applescript/apps/FileManagerLib_stuff.zip,\
		af517f6092c360a41d4df131fa44b16644c22a8d0f329cdcf6d630adde147c82,\
		20160705013713)


# Don't redownload artefacts for built targets
.INTERMEDIATE: \
	/tmp/ListRecordTools1.0.6.dmg \
	/tmp/PListTools1.0.7.dmg \
	/tmp/XMLTools2.9.4.dmg \
	/tmp/Dialog_Toolkit.zip \
	/tmp/FileManagerLib_stuff.zip \
	/tmp/Metadata_Lib_stuff.zip



# Configure macOS's built-in Script Editor.app
script-editor: sdefs-apps sefp
	convert-plist -x com.apple.ScriptEditor2.plist | defaults import com.apple.ScriptEditor2 -
	convert-plist -x $(sdefs-apps) | defaults import com.apple.ScriptEditor2 -


# SEFormattingPreferences configuration (i.e, syntax highlighting colours)
sefp-as = ~/Library/Preferences/com.apple.applescript.plist
sefp-js = ~/Library/Preferences/com.apple.JavaScriptOSA.plist
sefp: $(sefp-as) $(sefp-js)
$(sefp-as): $(notdir $(sefp-js)); $(call sefp-import,com.apple.applescript,AppleScriptSourceAttributes) 
$(sefp-js): $(notdir $(sefp-js)); $(call sefp-import,com.apple.JavaScriptOSA,JavaScriptSourceAttributes)

# Import an unencoded plist(5) to configure Script Editor.app's syntax highlighting
define sefp-import
	@ echo "Importing syntax-highlighting for $(2:SourceAttributes=)..."
	@ defaults write $(1) $(2) "($$(plutil -convert json $(1).plist -o - \
	| jq -r '.$(2)[] | [(.NSColor | join(" ")), .NSFont] | @tsv' \
	| while IFS=`printf '\t'` read -r colour font; do \
		font=$$(./encode-text-style.scpt font $$font); \
		colour=$$(./encode-text-style.scpt colour $$colour); \
		printf '{ NSFont = <%s>; NSColor = <%s>; },\n' "$$font" "$$colour"; \
	done))"
endef


# Search the system for scripting definitions
sdefs-cache = ~/Library/Caches/Script\ Editor
sdefs-list  = $(sdefs-cache)/scripting-definitions.txt
sdefs-list: $(sdefs-list)
$(sdefs-list):
	@ mkdir -p "$(@D)"
	@ echo "Searching system for scripting definitions..."
	@ find 2>/dev/null > "$@" -s / \( \
		-path '/System/Volumes' -or \
		-path '/Volumes' -or \
		-path '/Users' -or \
		-path '/dev' \
	\) -prune -or -type f -iname '*.sdef' -exec realpath {} + || :
	@ echo "Searching user paths for scripting definitions..."
	@ find 2>/dev/null >> "$@" -s ~ -depth 1 -type d -not \( \
		-name Applications -or \
		-name Library \
	\) -prune -or -type f -iname '*.sdef' -not -name 'PrinterProxy.sdef' -exec realpath {} + || :
	@ sdefs=`grep . "$@" | sort -uf`; printf '%s\n' "$$sdefs" > "$@"


# Compile an array of scriptable applications
sdefs-apps = $(sdefs-cache)/library-apps.plist
sdefs-apps: $(sdefs-apps)
$(sdefs-apps): $(sdefs-list)
	@ echo "Formatting definition files as an importable property list..."
	@ echo '{ LibraryApplications = {' > "$@"; rm -f "$@~"; \
	defaults='/System/Library/Script Editor Plugins/Library.seplugin/Contents/Resources/SEPLibraryDefaults.plist'; \
	if test -f "$$defaults"; then \
		printf 'Importing defaults: %s\n' "$$defaults"; \
		plutil -convert json "$$defaults" -o - \
		| jq -r '.LibraryApplications | to_entries[] | [.key, (.value | .id, .name, .path)] | @tsv' \
		| while IFS=`printf '\t'` read -r key id name path; do \
			printf >> "$@" '"%s"={id="%s";name="%s";path="%s";};\n' "$$key" "$$id" "$$name" "$$path"; \
			echo >> "$@~" "$$id"; \
		done; \
	fi; \
	sed -n < "$^" \
		-e :a \
		-e '/./! d' \
		-e 's|/$$||' \
		-e '/\.app$$/   { p; n; }' \
		-e '/\.osax$$/  { p; n; }' \
		-e '/\.scptd$$/ { p; n; }' \
		-e 's|/[^/]\{1,\}$$||' \
		-e ta \
	| sort -f | uniq | while IFS= read -r path; do \
		plist=`convert-plist -x "$$path/Contents/Info.plist"`; \
		if test -z "$$plist"; then \
			printf >&2 'Failed to load library: %s\n' "$$path"; \
			continue; \
		fi; \
		printf %s "$$plist" | plutil -convert json - -r -o - \
		| jq -r '[.CFBundleIdentifier, .CFBundleName] | @tsv' \
		| while IFS=`printf '\t'` read -r id name; do \
			if grep -qxF "$$id" "$@~"; then \
				printf >&2 'Already defined library with ID "%s"\n' "$$id"; \
				continue; \
			fi; \
			if test -z "$$name"; then \
				name="$${path##*/}"; \
				name="$${name%.*}"; \
			fi; \
			set -- "$$name" "$$id" "$$name" "$$path"; \
			printf '"%s"={id="%s";name="%s";path="%s";};\n' "$$@"; \
			echo >> "$@~" "$$id"; \
			printf >&2 'Defined library with ID "%s"\n' "$$id"; \
		done; \
	done | sort -d -f >> "$@"
	@ echo '};}' >> "$@"; rm -f "$@~"
	@ pl=`convert-plist -x "$@"`; printf '%s\n' "$$pl" | convert-plist > "$@"




# Section 3: Periodic file backups
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



# Section 4: Canned recipe definitions

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

# Download and optionally checksum a binary asset
define download-asset
	wget -O "$@" "$(if $(3),https://web.archive.org/web/$(strip $(3))id_/$(strip $(1)),$(strip $(1)))"
	$(if $(2),sha256sum "$@" | cut -d' ' -f1 | grep -qxiF "$(strip $(2))")
	chmod -w "$@"
endef

# Preliminary logic shared by “~/Library/*/*.{bundle}/” recipes
# NOTE: GNU make(1) does strange things to automatic variables like $@ when they contain whitespace,
#       hence the reason for explicitly-defined local variables for dirname(1) and basename(1).
define prepare-library-bundle
	dir="$@"; base=$${dir##*/}; dir=$${dir%/*}; \
	if test -d "$@"; then \
		case `cd "$$dir" && find 2>/dev/null -s "$$base" | md5sum` in \
			$(1)|$(1)\ *) touch -c "$@"; exit;; \
			*) rm -vrf "$@";; \
		esac; \
	else rm -vf "$@"; fi; \
	mkdir -vp "$$dir"
endef

# Extract a file named $(@F) from the root directory of a DMG package ($^)
define extract-dmg
	$(prepare-library-bundle); \
	mnt=`hdiutil attach -plist -nobrowse -readonly -mountrandom /tmp "$^" \
		| plutil -convert json - -o - \
		| jq -r '.["system-entities"][] | .["mount-point"] | strings' \
		| head -n1`; \
	cp -vpR "$$mnt/$$base" "$$dir/$$base"; \
	diskutil unmount force "$$mnt"
endef

# Extract an entry named $(@F) fromt the root of a ZIP archive ($^)
define extract-zip
	$(prepare-library-bundle); \
	bsdtar -C "$$dir" -xvf "$^" "$$base/"
endef
