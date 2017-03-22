all: lists

# Generate a purdy PostScript of V8's glossy new manpage
v8-src := share/man/man1/v8.1
v8-obj := $(v8-src:.1=.ps)
$(v8-obj): $(v8-src)
	groff -man -eC -Tps $^ > $@
v8: $(v8-obj)


# Update lists of installed packages
lists: $(addsuffix -list,npm gem pip brew)

.ONESHELL:
install-script := etc/install-packages.sh
brew-update-off = export HOMEBREW_NO_AUTO_UPDATE=1
brew-update-on  = export HOMEBREW_NO_AUTO_UPDATE=

# Global NPM modules
npm-list: $(install-script)
	@modules=$$(ls /usr/local/lib/node_modules | sort -fi | sed -Ee '/npm|uglifyjs/d; s/^/\t/g;');\
	edit $^ 's/\nnpm_modules="\K[^"]*(?=")/\n'"$$modules"'\n/sm';

# RubyGems
gem-list: $(install-script)
	@gems=$$(gem list --no-versions | grep -v '* LOCAL GEMS *' | sort -fi | sed -r 's/^/\t/g');\
	edit $^ 's/\nruby_gems="\K[^"]*(?=")/\n'"$$gems"'\n/sm';

# Python packages
pip-list: $(install-script)
	@packages=$$(pip list --format=legacy | cut -d ' ' -f 1 | sort -fi | sed -r 's/^/\t/g');\
	edit $^ 's/\npip_packages="\K[^"]*(?=")/\n'"$$packages"'\n/sm';

# Homebrew: All lists
brew-list: $(addsuffix -list,cask tap formula)

# Homebrew: Casks/Graphical programs
cask-list: $(install-script)
	@$(call brew-update-off);\
	casks=$$(brew cask list 2>/dev/null | sort -fi | sed -r "s/^/\t/g");\
	edit $^ 's|\nbrew_casks="\K[^"]*(?=")|\n'"$$casks"'\n|sm';\
	$(call brew-update-on);

# Homebrew: Tapped repositories
tap-list: $(install-script)
	@$(call brew-update-off);\
	taps=$$(brew tap | sort -fi | sed -r "s/^/\t/g");\
	edit $^ 's|\nbrew_taps="\K[^"]*(?=")|\n'"$$taps"'\n|sm';\
	$(call brew-update-on);

# Homebrew: Installed formulae
formula-list: $(install-script)
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
