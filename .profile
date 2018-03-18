umask 022
macos=; case `uname -s` in [Dd]arwin) macos=1;; esac

# Load connected files
for i in env aliases prompt functions tmp; do
	i=~/.files/$i.sh
	[ -r "$i" ] && . "$i"

	# macOS-specific extras
	if [ "$macos" ]; then
		i="${i%/*}/macos/${i##*/}"
		[ -r "$i" ] && . "$i"
	fi
done

# Bail if running non-interactively
case $- in *i*) ;; *) return 0 ;; esac

# Initialise TTY
if [ -x /usr/bin/tset ]; then
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

# macOS-specific startup code
[ -z "$macos" ] || . ~/.files/macos/init.sh
unset macos
