all:

# Restore the pre-25.1 Emacs icon with the prettier traditional one
emacs-icon: $(wildcard .emacs.d/*.icns)
	@cp -f $^ /Applications/Emacs.app/Contents/Resources/
	@echo "Emacs icon updated. Restart system to force display in Finder/Spotlight.";

# Setup new workstation
install: symlinks packages perl-links post-install-msg

# Reconnect symlinked dotfiles
symlinks := ~/.emacs.d ~/.gitconfig ~/.vimrc ~/.ssh/config
symlinks: $(symlinks)
~/.%: ./%
	ln -sf $^ $@
~/.ssh/config: ssh-config
	ln -sf $^ $@

# Reinstall packages, modules and Homebrew formulae
packages: $(install-script)
	@./$^

# Link preinstalled Perl stuff
perl-links:
	cd ~ && chflags hidden perl5
	eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"

# Print reminders that won't fit easily into this mess
post-install-msg:
	@permalink="https://discussions.apple.com/thread/7675366?start=0&tstart=0";\
	echo "Done. If MacBook overheats, enable iCloud keychain:";\
	echo "\x1B[4m$$permalink""\x1B[0m";
.PHONY: packages

# ==================================

# Compile a cutting-edge build of V8
v8:
	@cd /Users/johngardner/Mirrors/v8;\
	git pull;\
	tools/dev/v8gen.py x64.release;\
	ninja -C out.gn/x64.release;\
	tools/run-tests.py --gn;\
	path=/usr/local/bin/v8;\
	[ -L $$path ] || ln -fs "$$path";\
	$$path --help

# ==================================

# Update lists of installed packages
lists: $(addsuffix -list,npm gem pip brew)

.ONESHELL:
install-script := etc/install-packages.sh
brew-update-off = export HOMEBREW_NO_AUTO_UPDATE=1
brew-update-on  = export HOMEBREW_NO_AUTO_UPDATE=
print-status    = tput setaf 4; printf '==> '; tput sgr0; echo $(1)

# Global NPM modules
npm-list: $(install-script)
	@$(call print-status,"Updating list: NPM modules");
	@modules=$$(ls /usr/local/lib/node_modules | sort -fi | sed -Ee '/npm|uglifyjs/d; s/^/\t/g;');\
	edit $^ 's/\nnpm_modules="\K[^"]*(?=")/\n'"$$modules"'\n/sm';

# RubyGems
gem-list: $(install-script)
	@$(call print-status,"Updating list: RubyGems");
	@gems=$$(gem list --no-versions | grep -v '* LOCAL GEMS *' | sort -fi | sed -r 's/^/\t/g');\
	edit $^ 's/\nruby_gems="\K[^"]*(?=")/\n'"$$gems"'\n/sm';

# Python packages
pip-list: $(install-script)
	@$(call print-status,"Updating list: Python packages");
	@packages=$$(pip list --format=legacy | cut -d ' ' -f 1 | sort -fi | sed -r 's/^/\t/g');\
	edit $^ 's/\npip_packages="\K[^"]*(?=")/\n'"$$packages"'\n/sm';

# Homebrew: All lists
brew-list: $(addsuffix -list,cask tap formula)

# Homebrew: Casks/Graphical programs
cask-list: $(install-script)
	@$(call print-status,"Updating list: Homebrew casks");
	@$(call brew-update-off);\
	casks=$$(brew cask list 2>/dev/null | sort -fi | sed -r "s/^/\t/g");\
	edit $^ 's|\nbrew_casks="\K[^"]*(?=")|\n'"$$casks"'\n|sm';\
	$(call brew-update-on);

# Homebrew: Tapped repositories
tap-list: $(install-script)
	@$(call print-status,"Updating list: Homebrew taps");
	@$(call brew-update-off);\
	taps=$$(brew tap | sort -fi | sed -r "s/^/\t/g");\
	edit $^ 's|\nbrew_taps="\K[^"]*(?=")|\n'"$$taps"'\n|sm';\
	$(call brew-update-on);

# Homebrew: Installed formulae
formula-list: $(install-script)
	@$(call print-status,"Updating list: Homebrew formulae");
	@$(call brew-update-off);\
	formulae="";\
	names=$$(brew list | sort -fi); \
	for name in $$names; do \
		json=$$(brew info --json=v1 "$$name" 2>/dev/null);\
		[ "$$json" ] && {\
			opts=$$(echo "$$json" | perl -0777 -n -e 'print $$1 if m/"used_options":\[([^\]]+)/;' | tr ',"' ' ');\
			[ "$$opts" ] && formulae+=$$'\n'"$$name $$opts" || formulae+=$$'\n'"$$name";\
		};\
	done; \
	list=$$(echo "$$formulae" | sed -n $$'/[^ \t]/,$$p' | sed -E 's/@/\$\$${AT}/g; s/^/\t/g; s|\ \ +|\ |g;'); \
	edit $^ 'our $$AT="\@"; s|\nbrew_formulae="\K[^"]*(?=")|\n'"$$list"'\n|sm'; \
	$(call brew-update-on);
