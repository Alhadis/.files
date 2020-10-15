#!/bin/sh

# Jump to whatever directory contains a file or executable
visit(){
	[ -n "$1" ] || {
		>&2 printf "Usage: visit [file | command]\n";
		return 1;
	}

	# If given a directory, go to it
	[ -d "$1" ] && { cd "$1"; return; }

	path=`command -v "$1" 2>/dev/null || printf %s "$1"`;

	[ -e "$path" ] || {
		>&2 printf 'Not found: %s\n' "$path";
		return 1;
	}

	# Resolve symbolic links if possible
	command -v realpath 2>&1 >/dev/null && path=`realpath "$path"`
	cd "${path%/*}"
	unset path
}


# Run git-status(1) with an empty line inserted before each list of changes
g(){
	if [ ! -t 1 ]; then git status "$@"; return $?; fi
	git -c color.status=always status "$@" \
	| sed -e '1h;1!H;$!d;x;{s/\(\n[^]*\)\(\n\)\([[:blank:]]*\[\)/\1\2\2\3/g;}'
}


# Locate files by name
f(){
	find . -type f -name "*$1*";
}


# Browse a manual-page's source code
mansrc(){
	less `man -w $@`
}


# Browse an executable's source code
src(){
	less `command -v "$1"`
}


# Print bytes by decimal value
bytes(){
	case $1 in -h|--help|-\?|'')
		printf >&2 'Usage: bytes [0..255]\n'; [ "$1" ]
		return ;;
	esac
	printf %b "`printf \\\\%03o "$@"`"
}


# Display file permissions in octal format
ostat(){
	case $1 in -h|--help|-\?|'')
		printf >&2 'Usage: ostat [...files]\n'; [ "$1" ]
		return ;;
	esac
	
	# GNU/Linux
	if command -v gstat >/dev/null 2>&1; then gstat -c %a "$@"
	elif stat -c %a /   >/dev/null 2>&1; then stat  -c %a "$@"
	
	# BSD derivatives
	else while [ $# -gt 0 ]; do
		stat -f %Op "$1" | tee -c4
		shift
	done; fi
}


# Display modification date as a Unix timestamp
unixstamp(){
	case `stat --version 2>/dev/null` in
		*GNU*) stat -c %Y "$@" ;; # Linux
		*)     stat -f %m "$@" ;; # BSD/macOS
	esac
}


# Quick 2-way conversion of WebP images
webp(){
	[ -z "$1" ] && {
		>&2 printf 'Usage: webp /path/to/file\n'
		return 1
	}
	[ ! -f "$1" ] && {
		>&2 printf "ERROR: \"%s\" isn't a valid image file.\n" "$1"
		return 1
	}
	case $1 in
		*.webp) dwebp "$1" -o "${1%.webp}.png";;
		*)      cwebp "$1" -o "${1%.[[:alnum:]]*}.webp";;
	esac
}


# Encode characters that have a special meaning in URIs
encodeurl(){
	for arg in "$@"; do
		# Use JavaScript's URI-encoding function if possible
		if command -v node 2>&1 >/dev/null; then
			arg=`printf %s "$arg" | sed 's/"/\\"/g'`
			node -pe 'encodeURIComponent("'"$arg"'").replace(/%20/g, "+")'
		else
			printf '%s\n' "$arg" | sed 's,/,%2F,g; s/\\/%5C/g;
			s/\?/%3F/g; s/&/%26/g; s/\+/%2B/g; s/\s+|%20/+/g'
		fi
	done
}


# Convert PostScript to PNG
ps2png(){
	command -v gs 2>&1 >/dev/null || {
		>&2 printf 'GhostScript is required to use this function.\n'
		return 1
	}
	[ $# -lt 1 ] && {
		>&2 printf 'Usage: ps2png [files...]\n'
		return 1
	}
	for arg in "$@"; do
		\gs -q -r300 -dTextAlphaBits=4 -sDEVICE=png16m -o "${arg%.ps}-%d.png" "$arg"
	done; unset arg
}


# Make other people's projects less aggravating to read
unfuck(){
	command -v prettier 2>&1 >/dev/null || {
		>&2 printf 'Prettier is required to use this function.\n'
		return 1
	}
	[ $# -gt 0 ] || set -- "**/*.{js,jsx,json,ts,mjs,css,less,scss}"
	prettier >/dev/null \
		--config ~/Labs/JG/etc/.prettierrc.json \
		--with-node-modules \
		--loglevel silent \
		--no-editorconfig \
		--write -- "$@"
}


# Print geographical location of an IP address
iplocation(){
	[ $# -eq 0 ] && {
		>&2 printf 'Usage: iplocation [ip-addr]\n'
		return 1
	}
	(printf %s "$1" | grep -Eqe '^([0-9]+\.){3}[0-9]+') || {
		>&2 printf 'Not an IP address: %s\n' "$1"
		return 2
	}
	printf '%s\n' "`curl -s "https://extreme-ip-lookup.com/json/$1"`" | json -i
}


# Empty clipboard, wipe history logs, and nuke pointless junk
purge(){
	rm -fP .{irb,units,node_repl,python}_history .lesshst ~/.DS_Store
	case `uname -s` in [Dd]arwin) rm -fP ~/.Trash/* ~/.Trash/.DS_Store;; esac
	case "${0##-}"  in bash) history -c;; esac
	clip -c
}


# Archive files using 7-Zip's strongest compression settings
crush(){
	command -v 7z 2>&1 >/dev/null || {
		>&2 printf 'error: p7zip not installed or 7z executable not in path\n'
		return 1
	}
	case $# in
		0) >&2 printf 'Usage: crush /path/to/file.psd\n'; return 1 ;;
		1) name=`basename "${1%.[[:alnum:]]*}" | sed 's/[[:blank:]]/-/g; s/--*/-/g'`;;
		*) name=archive ;;
	esac
	7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on "${name}.7z" "$@"
}


# Pretty-print a variable containing a colon-delimited path-list
ppls(){
	case $# in
		0) nolabel=1; set -- PATH ;;
		1) nolabel=1 ;;
		*) nolabel=  ;;
	esac
	while [ $# -gt 0 ]; do
		[ "$nolabel" ] || printf '%s:\n' "$1"
		ls=`eval 'printf "%s\n" "$'$1'"' | tr : '\n'`
		[ "$nolabel" ] || ls=`printf %s "$ls" | sed -e 's/^/	/'`
		printf '%s\n' "$ls"
		shift
	done
	unset nolabel
}


# Print names of files containing binary (non-textual) data
listbinary(){
	for i in "$@"; do
		[ ! -f "$i" ] && continue;
		[ ! -s "$i" ] || grep -Iq "$i" -Ee. || printf %s\\n "$i";
	done
}


# Compare filesize before and after gzip compression
# - Source: https://github.com/mathiasbynens/dotfiles
gzcmp(){
	origsize=`wc -c < "$1"`
	gzipsize=`gzip -c "$1" | wc -c`
	ratio=`printf '%d * 100 / %d' "$gzipsize" "$origsize" | bc -l`
	printf 'Original: %d bytes\n' "$origsize"
	printf 'Gzipped:  %d bytes (%2.2f%%)\n' "$gzipsize" "$ratio"
	unset origsize gzipsize ratio
}
