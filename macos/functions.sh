#!/usr/bin/env bash

# Global function for full-screening the terminal window.
fit(){
	# Make sure we're running interactively.
	case "$-" in *i*) osascript -e '
		tell application "Terminal"
		activate
		set bounds of window 1 to {0, 0, 1440, 800}
		set position of window 1 to {0, 0}
		end tell';;
	esac
}


# Restore Chrome's dimensions after accidentally resizing
fit-chrome(){
	osascript -e '
		tell first window of application "Google Chrome"
		set bounds to {0, 0, 1440, 800}
		end tell'
}


# Upgrade system software and libraries, and keep local repos synced
update(){
	softwareupdate -i -a;
	brew update;
	brew upgrade && brew cleanup --prune=0;
	npm -g update;
	gem update -N --system && gem update -N;
	gem cleanup;
	cd ~/Mirrors && gclient sync;
	update-repos $AF ~/Mirrors ~/Forks
}


# Assemble and link an executable from x86 assembly.
asm(){
	[ $# -eq 0 ] && {
		echo >&2 "Usage: asm /path/to/src.asm"
		return 1;
	}
	for i in "$@"; do
		[ -f "$i" ] && {
			local name=$(echo "$i" | sed -Ee 's/\.[A-Za-z]+$//');
			local obj="$name.o"
			nasm -f macho64 -o "$obj" "$i" && \
			ld -macosx_version_min 10.7.0 -lSystem -o "$name" "$obj" && \
			rm "$obj"
		};
	done
}


# Open Electron in a detached background process.
electron(){
	local execPath=$(which electron) 2>&1 >/dev/null;
	[ -n "$execPath" ] || {
		>&2 echo 'Electron could not be found in your $PATH.'
		>&2 echo 'Run `npm install -g electron` before trying again.'
		return 1;
	};
	{ $execPath $@ & disown; } 2>/dev/null 1>/dev/null
}


# RTBTM: "Read that back to me". Reads out a list of numbers one-by-one.
rtbtm(){
	echo "$*" | perl -pe 's/(\d)\D*/$1. /g' | say
}


# Print the URL a file was downloaded from
where-from(){
	for i in "$@"; do
		url=$(xattr -p user.xdg.origin.url "$i" 2>/dev/null)
		[ $? -eq 0 ] && printf %s\\n "$url" || {
			url=$(xattr -p com.apple.metadata:kMDItemWhereFroms "$i")
			[ $? -eq 0 ] && {
				echo $url | xxd -r -p | plutil -convert xml1 -o - - |\
				grep -E -e '<string>http' | sed -E -e 's/\t?<\/?string>//g' | head -n1
			};
		};
	done;
}


# Open the URL a file was downloaded from
goto-source(){
	where-from "$*" |\
		sed -Ee 's|^(https?://)raw\.githubusercontent\.com|\1github.com|i' |\
		sed -Ee 's|^(https?://github\.com/[^/]+/[^/]+/)|\1blob/|i' |\
		xargs open
}


# Clear clipboard/history if passed no args; execute calc() otherwise
c(){
	# Keep calculator-shorthand available when passed arguments
	[ $# -gt 0 ] && { calc $@; return; }
	
	# Empty system clipboard
	printf '' | pbcopy;
	
	# Actually clear Terminal's history
	history -c; > ~/.bash_history
	rm -fP ~/.{lesshst,viminfo}
	rm -fP ~/.{coffee,node_repl}_history
	defaults delete com.apple.finder FXRecentFolders 2>/dev/null
	mdfind -0 'kMDItemLastUsedDate = "*"' | xargs -0 xattr -d com.apple.lastuseddate#PS 2>/dev/null
	
	# Erase scrollback
	clear;
	osascript -e 'tell application "System Events" to keystroke "k" using command down';
}
