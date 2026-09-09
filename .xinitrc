#!/bin/sh
set -e

# Load X resource files from $HOME
set -- ~/.Xresources ~/.Xdefaults
while [ "$#" -gt 0 ]; do
	test -f "$1" && xrdb -quiet -merge "-I$HOME" "$1"
	shift
done

case `uname -s` in
	# macOS
	Darwin)
		xterm -u8 &
		exec /opt/X11/etc/X11/xinit/xinitrc
	;;

	# OpenBSD
	OpenBSD)
		export LC_CTYPE=en.UTF-8
		if type startxfce4 >/dev/null 2>&1; then
			exo-open --launch TerminalEmulator &
			exec startxfce4
		else
			xterm -u8 &
			exec twm -file /etc/X11/twm/system.twmrc
		fi
	;;

	# Something we've not prepared for
	*)
		echo "Platform not configured: `uname -s`" >&2
		exit 1
	;;
esac
