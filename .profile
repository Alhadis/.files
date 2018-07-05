[ `uname` = "OpenBSD" ] && umask 022

# Fedora: Load /etc/bashrc as advised
command -v yum 2>&1 >/dev/null && [ -f /etc/bashrc ] && . /etc/bashrc

# Load connected files
for i in env aliases prompt functions tmp; do
	i=~/.files/$i.sh
	[ -r "$i" ] && . "$i"
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
