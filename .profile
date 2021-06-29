# Shell options
set -o braceexpand >/dev/null 2>&1
set -o notify      >/dev/null 2>&1

alias have='command -v 2>&1 >/dev/null'

# Fedora: Load /etc/bashrc as advised
have yum && [ -f /etc/bashrc ] && . /etc/bashrc

# Load connected files
for i in env aliases prompt functions tmp; do
	i=~/.files/$i.sh
	[ -r "$i" ] || continue
	if [ "$ZSH_VERSION" ]; then
		emulate sh -c '(){ typeset -h path; . "$i"; }'
	else
		. "$i"
	fi
done


# Bail if running non-interactively
case $- in *i*) ;; *) return 0 ;; esac


# Silence nag from macOS >=10.15 about using bash(1)
if [ "`uname -s`" = Darwin ]; then
	export BASH_SILENCE_DEPRECATION_WARNING=1

# Initialise TTY
elif [ -x /usr/bin/tset ]; then
	case $DISPLAY in *?*) xset -b;; esac

	if [ "$XTERM_VERSION" ]; then
		eval `/usr/bin/tset -sQ '-munknown:?vt220' $TERM`
	else
		eval `/usr/bin/tset -IsQ '-munknown:?vt220' $TERM`
	fi
fi

# Make less(1) more helpful when reading non-binary stuff
if [ -x /usr/bin/lesspipe ]; then
	eval "`SHELL=/bin/sh lesspipe`"
fi

# Ensure ^T is used for sending SIGINFO
stty status "`printf '\x14'`"

# Enable this last so it doesn't screw with startup code
set -o pipefail >/dev/null 2>&1
