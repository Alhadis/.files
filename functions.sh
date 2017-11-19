# Global function for full-screening the terminal window.
fit(){
	# Make sure we're running interactively.
	[[ $- == *i* ]] && {
		osascript -e 'tell application "Terminal"
			activate
			set bounds of window 1 to {0, 0, 1440, 800}
			set position of window 1 to {0, 0}
		end tell';
	};
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


# Run automated tests
t(){
	
	# Nroff without trailing lines
	[ -f test.roff ] && {
		groff -tpUTutf8 $@ < test.roff | trim-end;
		return;
	}
	
	# Run test-scripts in current directory
	[ -f test.sh ] && {      ./test.sh "$@"; return; }
	[ -f test.pl ] && { perl ./test.pl "$@"; return; }
	[ -f test.ru ] && { ruby ./test.ru "$@"; return; }
	[ -f test.js ] && { node ./test.js "$@"; return; }

	# Makefile: "make test"
	[ -f Makefile ] && { grep Makefile -qe ^test:; } && {
		make test;
		return;
	}

	# Atom
	[ -d spec ] && {
		atom -t spec;
		return;
	}

	# Atom + Mocha
	[ -d test ] && [ -d node_modules/atom-mocha ] && {
		atom -t test;
		return;
	}

	# Mocha
	[ -d test ] && {
		mocha --es_staging;
		return;
	}
	
	# Unknown executable assumed to be a test-script
	[ -x test ] && [ -s test ] && { ./test "$@"; return; }

	# No tests found; do nothing
	true;
}


# Quick calculator: copies result to clipboard after evaluation
calc(){

	# Strip alphabetic characters from input; it's common to copy "180.00 pt" from
	# Adobe Illustrator, or other programs that append units to metric fields.
	local result=$(printf "%s\n" "$*" | perl -pe 's/(\d+|\s+)x(\s+|\d+)/$1*$2/gi; s/[A-Za-z]+/ /g;' | bc -l | tr -d '\\\n')

	# Drop trailing zeroes after the decimal point
	printf %s "$result" | perl -pe 's/\.0+$|(\.\d*?)0+$/$1/g' | pbcopy;

	# Copy to STDERR
	pbpaste;
	printf '\n';
}


# Assemble and link an executable from x86 assembly (macOS only).
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


# Switch to whatever directory contains a file or executable
jump-to(){

	# If given a directory, go to it
	[ -d "$1" ] && { cd "$1"; return; }

	path=$(which "$1" || echo "$1")
	[ -e "$path" ] && cd $(dirname $(realpath "$path"))
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


# Browse the source code of a manual page
mansrc(){
	less $(man -w "$1")
}


# Browse an executable's source code
src(){
	less $(which "$1")
}


# Convert a font to another format. Defaults to OpenType.
# - Usage: convert-font [path] [to=otf]
convert-font(){
	[ $# -eq 0 ] && {
		echo >&2 "Usage: convert-font /path/to/file [format=.otf]"
		return 1;
	}
	local format=$(echo ${2:-.otf} | tr -d '.')
	fontforge -nosplash -lang=ff -c '
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
	' "$1" "$2" 2>/dev/null;
}


# Quick 2-way conversion of WebP images
webp(){
	[ ! $1 ] && {
		echo >&2 "Usage: webp /path/to/file";
		return 1;
	}
	[ ! -f "$1" ] && {
		echo >&2 "ERROR: \"$1\" isn't a valid image file.";
		exit 1;
	}
	local abspath=$(realpath "$1")
	local dirname=$(dirname "$abspath");
	local filename=$(basename "$abspath");
	echo "$filename" | grep -e '\.webp$' && { cmd=dwebp; ext=.png; }
	${cmd-cwebp} "$abspath" -o "$dirname/"$(echo "$filename" | sed -E "s/(\.[A-Za-z0-9]+)$//")${ext-.webp};
}


# Upload an image to Imgur
imgur(){
	local id=${IMGUR_CLIENT_ID-c9a6efb3d7932fd}
	[ -z "$id" ] && {
		printf >&2 "imgur: A registered client ID is required"
		return 2;
	}
	for arg in "$@"; do
		local upload=$(xattr -p user.imgur.upload "$arg" 2>/dev/null)
		if [ "$upload" ]; then
			printf >&2 'imgur: Already uploaded: %s\n' "$arg";
		elif file -bI "$arg" | grep -Eq '^image/(png|jpeg|webp|gif|tiff|bmp)'; then 
			upload=$(curl "https://api.imgur.com/3/image.json" -sF "image=@$arg" \
				-H "Authorization: Client-ID $id" \
				-H "Expect: ");
			xattr -w user.imgur.upload "$upload" "$arg"
		else continue; fi
		printf %s "$upload" | json data.link
	done
}


# Compress images with TinyPNG
tinify(){
	[ $# -lt 1 ] && {
		printf >&2 "Usage: tinify /path/to/file.png\n"
		return 1;
	}
	
	# Locate API key
	local key=${TINIFY_KEY:-${TINIFY_API_KEY}}
	[ ! "$key" ] && [ -s ~/.tinify ] && key=$(head -n1 ~/.tinify)
	[ ! "$key" ] && {
		printf >&2 "tinify: Unable to locate API key\n";
		return 2;
	}
	
	# Handle arguments list
	for arg in "$@"; do
		file -bI "$arg" | grep -qE '^image/(png|jpeg)' && {
			echo >&2 "Compressing: $arg"
			curl "https://api.tinify.com/shrink" \
				--user "api:$key" -qs \
				--data-binary @"$arg" \
				--dump-header /dev/stdout \
			| sed -En 's/^location:\s*(.+)$/\1/p' \
			| tr -d '\r\n' \
			| xargs -J% curl -qso "$arg" "%";
		}
	done
}


# Search for a file on GitHub
gh-search(){
	local usage="Usage: gh-search [ext[ension]|file[name]|lang[uage]] query"
	[ "$#" -lt 2 ] && {
		echo >&2 $usage;
		return 3;
	};

	# Check what type of search we're performing
	local type=$(echo "$1" | tr '[A-Z]' '[a-z]')
	case "$type" in
		ext|extension) type=extension ;;
		name|filename) type=filename  ;;
		lang|language) type=language  ;;
		*)
			echo >&2 "Search type must be one of ext, extension, filename, name, lang, language";
			echo >&2 $usage;
			return 3;;
	esac

	local url="https://github.com/search?q=%s%%3A%s+NOT+nothack&type=Code";
	open $(printf $url $type $2);
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


# Encode characters that have a special meaning in URIs
encode-url(){
	command >/dev/null 2>&1 node -v
	[ $? -eq 0 ] && haveNode=1 || haveNode=""
	for arg in "$@"; do
		# Use JavaScript's URI-encoding function if possible
		[ $haveNode ] && {
			arg=$(printf %s "$arg" | sed -Ee 's/"/\\"/g');
			node -pe 'encodeURIComponent("'"$arg"'").replace(/%20/g, "+")';
		} || {
			printf '%s\n' "$arg" | sed -Ee 's,/,%2F,g; s/\\/%5C/g;
			s/\?/%3F/g; s/&/%26/g; s/\+/%2B/g; s/\s+|%20/+/g';
		};
	done
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
		echo >&2 "Usage: iplocation [ip-addr]";
		return 1;
	}
	(echo "$1" | grep -Eqe '^([0-9]+\.){3}[0-9]+') || {
		echo >&2 "Not an IP address: $1";
		return 2;
	}
	local body=$(curl -s "https://extreme-ip-lookup.com/json/$1")
	[ $? -ne 0 ] && return $?
	printf '%s\n' "$body" | json -i
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


# Archive files using 7-Zip's strongest compression settings
7z-crush(){
	[ $# -eq 0 ] && {
		echo >&2 "Usage: 7z-crush /path/to/file.psd"
		return 1;
	}
	local filename
	[ $# = 1 ] && filename=$(basename "$1" | sed -Ee 's/(.)\.\w+$/\1/' | tr " " -);
	7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on "${filename-archive}".7z "$@"
}


# Print names of files containing binary (non-textual) data
list-binary(){
	for i in "$@"; do
		[ ! -f "$i" ] && continue;
		[ ! -s "$i" ] || grep -Iq "$i" -Ee. || printf %s\\n "$i";
	done
}


#=============================================================================#
# Following functions sourced from https://github.com/mathiasbynens/dotfiles: #
#=============================================================================#

# "Find" shorthand
function f(){
	find . -name "$1" 2>&1 | grep -v 'Permission denied'
}

# Change directory to whatever's in the forefront Finder window
function cdf(){
	cd "`osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)'`"
}

# Compare original and gzipped file size
gz() {
	local origsize=$(wc -c < "$1");
	local gzipsize=$(gzip -c "$1" | wc -c);
	local ratio=$(echo "$gzipsize * 100 / $origsize" | bc -l);
	printf "Original: %d bytes\n" "$origsize";
	printf "Gzipped:  %d bytes (%2.2f%%)\n" "$gzipsize" "$ratio";
}
