#!/usr/bin/env bash
set -xo braceexpand
shopt -s nocaseglob
shopt -s dotglob

#
# scrub.sh: Wipe pointless/unused logs and junk files
#

# This shouldn't happen, but better safe than sorry
if [[ ! $(uname -s) =~ ^[Dd]arwin$ ]]; then
	echo "${0##*/}: This should only be run on macOS" >&2
	exit 2
fi

# We'll need superuser privileges to perform some tasks
sudo -v

# Securely erase trash, unused logs, and autosaved/auto-generated junk
rm -rfPv ~/.Trash /Volumes/*/.Trashes
rm -rfPv ~/.emacs.d/auto-save-list
rm -fPv  ~/.{bash_history,lesshst,node_repl_history,viminfo}
rm -rfPv /private/var/log/asl/*.asl

# Keeping a log of everything I download? Creepy, Apple. Really creepy.
# - Source: https://macgasm.net/2013/01/18/good-morning-your-mac-keeps-a-log-of-all-your-downloads/
sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* "delete from LSQuarantineEvent";
