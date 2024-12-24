#!/bin/sh

# Create an archive of each specified directory
archive()(
	[ $# -eq 0 ] && {
		>&2 printf 'Usage: archive [...dir-paths]\n'
		return 1
	}
	command -v bsdtar >/dev/null 2>&1 && cmd='bsdtar -v --acls --fflags' || cmd='tar -v'
	case `uname -s` in Darwin) cmd="$cmd --xattrs --mac-metadata";; esac
	while [ $# -gt 0 ]; do
		out="${1##*/}"
		[ -e "$1" ] || {
			echo >&2 "Not found: $1"
			return 1
		}
		echo >&2 "Archiving: $1 -> $out.tgz"
		# shellcheck disable=SC2086
		$cmd -cvf "$out" "$1"
		touch -cr "$1" "$out"
		gzip -S .tgz "$out"
		chmod -w "$out.tgz"
		shift
	done
)

# Print bytes by decimal value
bytes(){
	case $1 in -h|--help|-\?|'')
		printf >&2 'Usage: bytes [0..255]\n'; [ "$1" ]
		return ;;
	esac
	printf %b "`printf \\\\%03o "$@"`"
}

# Convert a font using FontForge
have fontforge && convertfont()(
	cmd=~/.files/etc/convert-font.ff
	ext=${1#.}
	shift
	while [ $# -gt 0 ]; do
		fontforge -quiet -lang=ff -script "$cmd" "$1" "${1%.*}.$ext" || return $?
		shift
	done
)

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

# Generate a unified diff with highlighting
diff(){
	set -- diff -r -U4 "$@"
	if [ ! -t 1 ]; then command "$@"; return $?; fi
	command "$@" | format-diff
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

# Locate files by name
f(){
	find . -type f -name "*$1*";
}

# Run git-status(1) with an empty line inserted before each list of changes
g(){
	if [ ! -t 1 ]; then git status "$@"; return $?; fi
	git -c color.status=always status "$@" \
	| sed -e '1h;1!H;$!d;x;{s/\(\n[^]*\)\(\n\)\([[:blank:]]*\[\)/\1\2\2\3/g;}'
}

# Render a GitHub-flavoured markdown document
gfm(){
	case $1 in
		--context|-c) set -- "$2" "$3" ;;
		--context=*)  set -- "${1#*=}" "`shift && printf %s "$@"`" ;;
		-c?*)         set -- "${1#-c}" "`shift && printf %s "$@"`" ;;
		-)            set -- "" "`cat`" ;;
		[!-]*)        set -- "" "$1" ;;
		-*)
			echo >&2 'Usage: gfm [-c|--context user/repo] /path/to/input.md'
			case $1 in --help|--usage|-[h?]) return;; *) return 1;; esac
		;;
	esac
	[ ! "$1" ] || {
		set -- "`printf %s "$1" | json-string`"  "$2"
		set -- "`printf '"context": %s, ' "$1"`" "$2"
	}
	case $2 in
		*?*) set -- "$1" "`json-string < "$2"`" ;;
		'')  set -- "$1" "`cat | json-string`"  ;;
	esac
	printf '{%s"text": %s, "mode": "gfm"}' "$1" "$2" | curl -qLfSs \
		--data @- \
		-X POST \
		-H Accept:\ application/vnd.github+json \
		-H Content-Type:\ application/json \
		https://api.github.com/markdown
	[ -t 1 ] && echo || :
}

# Execute a PostScript program
gx(){
	command -v \gs >/dev/null 2>&1 || {
		>&2 printf 'GhostScript is required to use this function.\n'
		return 1
	}
	if   [ ! -t 0    ]; then set -- - "$@"; fi
	if   [ "$1" = -  ]; then shift; set -- /dev/stdin "$@"
	elif [ ! -f "$1" ]; then >&2 printf 'gs: No such file: %s\n' "$1"; set --
	elif [ $# -lt 1  ]; then >&2 printf 'Usage: gx file.ps [args...]\n'; return 1
	fi; \gs -sDEVICE=txtwrite -sOutputFile=- -q -sBATCH -dNOPAUSE -dNOSAFER -I. -- "$@"
}

# Format a test document using Heirloom Doctools
have heirloom-nroff && h(){
	! test -f test.roff || set -- "$@" test.roff
	heirloom-nroff -x0 "$@" | trimstart | trimend
}

# Read IPA notation out loud
ipa(){
	case $* in *[![:blank:]/\|]*) ;; *)
		>&2 printf 'Usage: ipa [text-in-ipa-notation]\n'
		return 1;;
	esac
	printf '{"text": "%s", "voice": "Nicole"}' "$*" \
	| curl -qLfSs --data @- -X POST \
		-H Content-Type:\ application/json \
		'https://iawll6of90.execute-api.us-east-1.amazonaws.com/production' \
	| jq -r | base64 -d | {
		if command play --help | grep -iq SoX; then
			play -qt mp3 -
		elif command -v ffplay; then
			ffplay -loglevel 0 -nodisp -autoexit -
		elif command -v aucat && command -v lame; then
			lame -r --decode --mp3input - /tmp/ipa.wav &&
			aucat -i /tmp/ipa.wav
		elif command -v audacious; then
			audacious -qH -
		fi
	} >/dev/null 2>&1
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

# Print names of files containing binary (non-textual) data
listbinary(){
	for i in "$@"; do
		[ ! -f "$i" ] && continue;
		[ ! -s "$i" ] || grep -Iq "$i" -Ee. || printf %s\\n "$i";
	done
}

# Print machine's local IP address
localip(){
	case `uname -s` in
		Darwin) ipconfig getifaddr en0;;
		Linux)  ip route get 1;;
		*) ifconfig | sed -n '
			s/^/ /; s/$/ /
			s/[:[:blank:]]127\.0\.0\.1[[:blank:]]//g
			s/.*inet \(addr:\)\{0,1\}\(\([0-9]*\.\)\{3\}[0-9]*\).*/\2/p
		' | head -n1;;
	esac
}

# Browse a manual-page's source code
mansrc(){
	less `man -w $@`
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
		stat -f %Op "$1" | tail -c4
		shift
	done; fi
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

# Empty clipboard, wipe history logs, and nuke pointless junk
purge(){
	rm -fP .{irb,units,node_repl,python}_history .lesshst ~/.DS_Store
	case `uname -s` in [Dd]arwin) rm -fP ~/.Trash/* ~/.Trash/.DS_Store;; esac
	case "${0##-}"  in bash) history -c;; esac
	clip -c
}

# Extract temporal and spatial data from a “Dynamic Desktop” as JSON
have plutil && solarInfo(){
	test -s "$1" || {
		printf >&2 'solarInfo: No such file: \033[4m%s\033[24m\n' "$1"
		return 1
	}
	exiftool -b -Solar "$1" \
	| base64 -d \
	| plutil -convert json -r - -o - \
	| unexpand -t2 \
	| sed '
		s/"ap" *:/"appearance":/
		s/"d" *:/"dark":/
		s/"l" *:/"light":/
		s/"si" *:/"solarInfo":/
		s/"a" *:/"altitude":/
		s/"z" *:/"azimuth":/
		s/"i" *:/"index":/
		/"o" *:/ {
			s//"onlyLightMode":/
			s/: *0\(,*\)$/: false\1/
			s/: *1\(,*\)$/: true\1/
		}
		$ s/^}$/&\n/
	'
}

# Alphabetise a list of SHA-256 checksums
sortsha(){
	for i in "$@"; do
		[ -s "$i" ] || continue
		sorted=`sort -sk2 "$i" | grep .`
		printf '%s\n' "$sorted" > "$i"
	done
	unset sorted
}

# Browse an executable's source code
src(){
	less `command -v "$1"`
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

# Display modification date as a Unix timestamp
unixstamp(){
	# Handle unrecognised options
	case $1 in --help|--short|-[-hs]);; -?*)
		printf >&2 "unixstamp: illegal option '%s'\n" "$1"
		shift; set -- '' "$@"
	;; esac

	# Discard options terminator
	[ "$1" = -- ] && shift

	case $1 in
		-h|--help|-\?|'')
			printf >&2 'Usage: unixstamp [-s|--short] ...files\n'; [ "$1" ]
			return ;;
		
		# Force low-precision (seconds-based) timestamps if requested
		-s|--short) shift; case `stat --version 2>/dev/null` in
			*GNU*) stat -c %Y "$@" ;;
			*)     stat -f %m "$@" ;;
		esac ;;

		# Print high-precision timestamps by default
		*) case `date --version 2>/dev/null` in
			*GNU*) while [ $# -gt 0 ]; do date -r "$1" +%s.%N; shift; done ;;
			*)     stat -f %Fm "$@" ;;
		esac | sed 's/\.\{0,1\}0*$//';;
	esac
}

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

# Print a directory file listing ordered by modification time
whatchanged(){
	[ -n "$1" ] || set -- .
	[ -d "$1" ] || {
		printf 'Usage: whatchanged /path/to/root/dir\n'
		return 1
	}
	case `stat --version 2>&1` in
		*GNU*) stat="stat --printf=%Y\t%n\n";;
		*)     stat='stat -f %m%t%N';;
	esac
	find "$1" -type f -exec $stat {} + | sort -n
}

# Print the URL whence a file was downloaded
have xattr && wherefrom()(
	xattr "$1" | while IFS= read -r attr; do
		case $attr in
			user.xdg.origin.url) xattr -p "$attr" "$1" ;;
			com.apple.metadata:kMDItemWhereFroms) xattr -xp "$attr" "$1" | xxd -p -r | pl;;
			*) continue ;;
		esac; break
	done
)
