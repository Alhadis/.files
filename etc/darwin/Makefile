all: cronjobs /etc/man.conf

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
