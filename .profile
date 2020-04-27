# Shell options
set -o braceexpand >/dev/null 2>&1
set -o pipefail    >/dev/null 2>&1
set -o notify      >/dev/null 2>&1

alias have='command -v 2>&1 >/dev/null'

# Fedora: Load /etc/bashrc as advised
have yum && [ -f /etc/bashrc ] && . /etc/bashrc

# Load connected files
for i in env aliases prompt functions tmp; do
	i=~/.files/$i.sh
	[ -r "$i" ] && . "$i"
done


# Bail if running non-interactively
case $- in *i*) ;; *) return 0 ;; esac

# Initialise TTY
if [ -x /usr/bin/tset ] && [ ! "`uname -s`" = Darwin ]; then
	case $DISPLAY in *?*) xset -b;; esac

	if [ X"$XTERM_VERSION" = X"" ]; then
		eval `/usr/bin/tset -sQ '-munknown:?vt220' $TERM`
	else
		eval `/usr/bin/tset -IsQ '-munknown:?vt220' $TERM`
	fi
fi

# Make less(1) more helpful when reading non-binary stuff
if [ -x /usr/bin/lesspipe ]; then
	eval "`SHELL=/bin/sh lesspipe`"
fi
