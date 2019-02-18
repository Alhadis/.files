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


# Convert a font to another format. Defaults to OpenType.
convertfont(){
	command -v fontforge 2>&1 >/dev/null || {
		>&2 printf 'FontForge is required to use this function.\n'
		return 1
	}
	[ $# -eq 0 ] && {
		>&2 printf 'Usage: convertfont /path/to/file [format=.otf]\n'
		return 1
	}
	fontforge 2>/dev/null -nosplash -lang=ff -c '
		fonts = FontsInFile($1);
		count = SizeOf(fonts);
		if(count != 0)
			i = 0;
			while(i < count)
				Open($1 + "(" + i + ")");
				Generate(fonts[i] + "." + $2);
				Close();
				i++;
			endloop
		else
			Open($1);
			Generate($1:r + "." + $2);
		endif
	' "$1" `printf %s ${2:-otf} | tr -d .`
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


# Assign a filename suffix to a list of files
suffix(){
	[ $# -lt 2 ] && {
		>&2 printf 'Usage: suffix [files...] [suffix]\n'
		return 1
	}
	for suffix in "$@"; do :; done
	while [ $# -ge 2 ]; do
		mv -i "$1" "$1.$suffix"
		shift
	done; unset suffix
}


# Shorten an absolute path by replacing $HOME with a tilde
tildify(){
	for arg in "$@"; do
		[ -n "$HOME" ] \
			&& printf '%s\n' "$arg" | sed "s,^$HOME/,~/," \
			|| printf '%s\n' "$arg";
	done
	# Read from stdin if piping additional input through
	[ -t 0 ] || while IFS= read -r line || [ -n "$line" ]; do
		[ -n "$HOME" ] \
			&& printf '%s\n' "$line" | sed "s,^$HOME/,~/," \
			|| printf "%s\n" "$line";
	done
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


# Archive files using 7-Zip's strongest compression settings
7zcrush(){
	command -v 7z 2>&1 >/dev/null || {
		>&2 printf 'error: p7zip not installed or 7z executable not in path\n'
		return 1
	}
	case $# in
		0) >&2 printf 'Usage: 7zcrush /path/to/file.psd\n'; return 1 ;;
		1) name=`basename "${1%.[[:alnum:]]*}" | sed 's/[[:blank:]]/-/g; s/--*/-/g'`;;
		*) name=archive ;;
	esac
	7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on "${name}.7z" "$@"
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
