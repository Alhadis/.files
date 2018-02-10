umask 022

# Load connected files
for i in env aliases prompt functions tmp; do
	i=~/.files/$i.sh
	[ -r "$i" ] && . "$i"

	# macOS-specific extras
	case `uname -s` in [Dd]arwin)
		i="${i%/*}/macos/${i##*/}"
		[ -r "$i" ] && . "$i" ;;
	esac
done

# Interactive shell setup
case "$-" in *i*)
[ -x /usr/bin/tset ] && {
	if [ X"$XTERM_VERSION" = X"" ]; then
		eval `/usr/bin/tset -sQ '-munknown:?vt220' $TERM`
	else
		eval `/usr/bin/tset -IsQ '-munknown:?vt220' $TERM`
	fi
}

# Make less(1) more helpful when reading non-binary stuff
[ -x /usr/bin/lesspipe ] && eval "`SHELL=/bin/sh lesspipe`"
;; esac
