all: mirror-list

# Restore the pre-25.1 Emacs icon with the prettier traditional one
emacs-icon: $(wildcard .emacs.d/*.icns)
	@cp -f $^ /Applications/Emacs.app/Contents/Resources/
	@echo "Emacs icon updated. Restart system to force display in Finder/Spotlight.";

# Setup new workstation
install: ~/.bash_sessions_disable ~/.hushlogin symlinks packages post-install

# Disable Bash-session saving
~/.bash_sessions_disable:
	touch $@
	rm -rf ~/.bash_sessions

# Suppress login messages
~/.hushlogin:
	touch $@

# Reconnect symlinks
symlinks: \
	~/.bash_profile \
	~/.curlrc \
	~/.emacs.d \
	~/.gitconfig \
	~/.vimrc \
	~/.ssh/config \
	/private/etc/man.conf

~/%: ./%
	ln -sf $(realpath $^) $@
~/.ssh/config: ssh-config
	ln -sf $(realpath $^) $@
/private/etc/man.conf: etc/man.conf
	sudo ln -sf $(realpath $^) $@

# Reinstall packages, modules and Homebrew formulae
packages: $(install-script)
	@./$^


# Random shite/Reminders that won't fit easily into this mess
post-install:
	cd ~ && chflags hidden perl5
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

.ONESHELL:
brews          := etc/Brewfile
install-script := etc/install.sh
print-status    = tput setaf 4; printf '==> '; tput sgr0; echo $(1)

# Update lists of installed packages
lists: $(brews) $(addsuffix -list,npm gem pip cpan mirror)


# Homebrew: Tapped repositories and installed formulae/casks
$(brews):
	@$(call print-status,"Updating list: Homebrew packages");
	@> $@; cd $(@D) && brew bundle dump --force;
.PHONY: $(brews)


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


# CPAN modules; at least those *we* installed.
# CPAN::Shell->autobundle no good: <http://www.perlmonks.org/?node_id=909966>
cpan-list: $(install-script)
	@$(call print-status,"Updating list: CPAN modules");
	@bundle_file=$$(echo "$$(cpan -a 2>/dev/null)" | tail -n1 | sed -E 's/^ +//;');\
	[ -s "$$bundle_file" ] || { >&2 echo "Empty autobundle at $$bundle_file"; exit 2; }; \
	modules=$$(perl -ne 'print if /^=head1 CONTENTS$$/../^=head1 CONFIGURATION$$/' "$$bundle_file" \
	| grep -vE ^=head | sort -fi | cut -d' ' -f1 | uniq \
	| perl -MModule::CoreList -ne 'print unless Module::CoreList::is_core($$_);' \
	| sed -n $$'/[^ \t]/,$$p' | sed -r 's/^/\t/g');\
	edit $^ 's/\ncpan_modules="\K[^"]*(?=")/\n'"$$modules"'\n/sm';\
	rm -f "$$bundle_file"
.PHONY: cpan-list


# Python packages
pip-list: $(install-script)
	@$(call print-status,"Updating list: Python packages");
	@packages=$$(pip list --format=legacy | cut -d ' ' -f 1 | sort -fi | sed -r 's/^/\t/g');\
	edit $^ 's/\npip_packages="\K[^"]*(?=")/\n'"$$packages"'\n/sm';


# Locally-cloned repositories containing interesting or relevant content
mirror-list: $(install-script)
	@$(call print-status,"Updating list: Mirrored repositories");
	@edit $^ 's/\ninstall;\n\K.*//s';\
	printf '\ncd ~/Mirrors;\n' >> $^;\
	for repo in ~/Mirrors/*/.{git,svn}; do (\
		cd $$repo/..; \
		case $$(echo $$repo | grep -Eo '(git|svn)$$') in \
			git) uri=$$(git remote get-url origin); cmd="git clone %s %s\n";;\
			svn) uri=$$(svn info --show-item url);  cmd="svn co %s %s\n";;\
			*)   continue;; esac;\
		printf "$$cmd" "$$uri" $$(pwd);\
	) | sed -e "s|$$HOME/Mirrors/||g" >> $^;\
	done;
