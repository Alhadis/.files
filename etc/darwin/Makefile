all: cronjobs

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
