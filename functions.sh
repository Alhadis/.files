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


# Print a Homebrew-style coloured marker
marker(){
	BOLD=$(tput bold)
	GREEN=$(tput setaf 10)
	RESET=$(tput sgr0)
	ARROW="${BOLD}${GREEN}==>${RESET}"
	echo "${ARROW} ${BOLD}$1${RESET}"
}


# Upgrade system software and libraries
update(){
	2>&1;
	sudo -v || { echo 'User cancelled; aborting updates'; return 1; }
	
	# Update OS X
	marker "Updating OS X";
	softwareupdate -i -a;
	
	# Homebrew
	marker "Updating Homebrew"
	brew update;
	brew upgrade --all --verbose;
	brew cleanup;
	
	# NPM modules
	marker "Updating Node modules"
	npm -g update;
	
	# Ruby gems
	marker "Updating Ruby gems" 
	gem update --system;
	gem update;
	gem cleanup;
	
	# Update local language listing
	marker "Updating Linguist language listing"
	local langfile="https://raw.githubusercontent.com/github/linguist/master/lib/linguist/languages.yml"
	wget $langfile -O ~/Documents/GitHub/Languages.yml
	
	# Update forks/copies of other people's repos
	marker "Updating forked repositories"
	~/Forks/update
}


# Run tests in Mocha or Atom
t(){
	
	# Atom
	[ -d spec ] && {
		atom -t spec;
		return;
	}
	
	# Mocha
	[ -d test ] && {
		mocha --es_staging;
		return;
	}
	
	# Run test-script in current directory
	[ -x test.js ] && {
		./test.js;
		return;
	}
	
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


# Read a string of numbers out one at a time.
# Useful to double-check number codes transcribed from physical sources (serial codes, etc).
rtbtm(){
	echo "$*" | perl -pe 's/(\d)\D*/$1. /g' | say
}


# Browse the source code of a manual page
mansrc(){
	less $(man -w "$1")
}


# Browse an executable's source code
show_src(){
	less $(which "$1")
}


# Convert a font to OpenType format
to_otf(){
	fontforge -nosplash -lang=ff -c 'Open($1); Generate($1:r + ".otf");' "$1" 2>/dev/null
}


# Create a new Babel/ECMAScript2015+ project. Usage: babelplate "Project Name"
babelplate(){
	name="${1:-Babelplate}"
	git clone https://github.com/Alhadis/Babelplate.git "$name";
	cd "$name" && make;
}


# Export a band entry from Metal Archives
ma_save(){
	local json="band-$1.json"
	ma-export --embed-images band "$1" > "$json" && {
		name=$(node -pe "require('./$json').bands[$1].name")
		mv "$json" "$name.json"
		{ rm .DS_Store; } > /dev/null 2>&1;
	}
}


# Lazily symlink a resource into the current working directory.
symlink(){
	local src="$1"
	local src_name=$(basename "$src")
	local name="${2:-$src_name}"
	ln -s "$src" $(pwd)/"$name"
}


# Install some git-hooks to prevent hasty version-committing
git_hooks(){
	hash node 2>&- || { >&2 echo "These hooks require Node.js to run."; return 2; }
	local name=check-version.sh
	local dest=.git/hooks/commit-msg
	test -f $SNIP/shell/git-hooks/$name && { cp $_ $dest; chmod +x $dest; } || {
		echo >&2 "Couldn't locate hook in snippets repo; downloading";
		curl -s 'https://raw.githubusercontent.com/Alhadis/Snippets/master/shell/git-hooks/$name' > $dest;
		chmod +x $dest;
	}
}


# Add Atom's icon to a file in Finder.
# Used to make files without extensions look code-related.
# See also: http://apple.stackexchange.com/q/213302
embed_atom_icon(){
	[ -f "$1" ] || {
		[ -f "Makefile" ] && embed_atom_icon "Makefile" || {
			echo >&2 "Usage: embed_atom_icon [recipients...]"
			return 2;
		}
	};
	
	for i in $@; do
		[ -w "$i" ] && {
			modified=$(stat -f "%Sm" -t "%Y%m%d%H%M.%S" "$i")
			cat ~/.files/etc/atom-icon.rsrc > "$i"/..namedfork/rsrc
			SetFile -a C "$i"
			touch -t $modified "$i"
		} || { echo >&2 "Skipping read-only file: $i"; }
	done
}


# Create a blank HTML document in the current directory and open it in Atom
# Usage: htmldoc [optional-name] -> ./optional-name.htm ("blank.htm" if omitted)
htmldoc(){
	default=blank
	to=${1:-${default}}
	(echo "$to" | grep -sE '\.(html?|php)$' > /dev/null) || to+=".htm";
	cp ~/.files/etc/$default.htm ./$to;
	atom $to;
}


# Print all locations that a file's been hard-linked to
showlinks(){
	[ ! "$1" ] && {
		echo >&2 "Usage: showlinks [file] [root-directory]";
		return 1;
	};
	find ${2:-~} -inum $(ls -i $1 | awk '{print $1}') 2>/dev/null;
	return 0;
}


# Search for a file on GitHub
gh_search(){
	local usage="Usage: gh_search [ext[ension]|file[name]|lang[uage]] query"
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


# Check the V8 optimisation status of a JavaScript file
opt(){
	local usage="Usage: check-v8-opt /path/to/file.js"
	local bin_path=~/.files/bin/check-v8-opt
	
	# Nothing passed
	[ 0 -eq $# ] && {
		>&2 echo $usage;
		return 1;
	};
	
	local file=$(realpath "$1")
	
	# Specified file actually wasn't a file
	[ -f "$file" ] || {
		>&2 echo "Not found: $file";
		>&2 echo $usage;
		return 1;
	};
	
	node --trace_deopt --allow-natives-syntax $bin_path "$file"
}



#==============================================================================
#   FOLLOWING FUNCTIONS ALL SHAMELESSLY PINCHED FROM THESE LOVELY CHAPS:
#
#   *   https://github.com/mathiasbynens/dotfiles
#   *   https://github.com/paulirish/dotfiles
#==============================================================================


# "Find" shorthand
function f(){
	find . -name "$1" 2>&1 | grep -v 'Permission denied'
}


# Change directory to whatever's in the forefront Finder window
# Mnemonic: "cd Finder"
function cdf(){
	cd "`osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)'`"
}


# Use Git's coloured diff
function diff(){
	git diff --no-index --color-words "$@";
}


# Get a character's Unicode code point
function codepoint(){
	perl -e "use utf8; print sprintf('U+%04X', ord(\"$@\"))";

	# Print a newline unless we're piping the output to another program
	if [ -t 1 ]; then
		echo ""; # newline
	fi;
}


# Decode \x{ABCD}-style Unicode escape sequences
function unidecode(){
	perl -e "binmode(STDOUT, ':utf8'); print \"$@\"";
	# Print a newline unless we're piping the output to another program
	if [ -t 1 ]; then
		echo ""; # newline
	fi;
}


# Look-up a domain or a URL in whois
function whois(){
	local domain=$(echo "$1" | awk -F/ '{print $3}')
	if [ -z $domain ] ; then
		domain=$1
	fi
	echo "Getting whois record for: $domain …"
	/usr/bin/whois -h whois.internic.net $domain | sed '/NOTICE:/q'
}



# Check who's been using the laptop's iSight camera
camerausedby(){
	echo -e "Checking to see who is using the iSight camera… \xF0\x9F\x93\xB7"
	usedby=$(lsof | grep -w "AppleCamera\|USBVDC\|iSight" | awk '{printf $2"\n"}' | xargs ps)
	echo -e "Recent camera uses:\n$usedby"
}


# "oak" is a shorthand for "tree" with hidden files and colour enabled, ignoring
# the ".git" directory, listing directories first. The output gets piped into "less"
# with options to preserve colour and line numbers, unless the output is small enough
# for one screen.
oak(){
	tree -aC -I '.git|node_modules|bower_components' --dirsfirst "$@" | less -FRNX;
}


# Compare original and gzipped file size
function gz() {
	local origsize=$(wc -c < "$1");
	local gzipsize=$(gzip -c "$1" | wc -c);
	local ratio=$(echo "$gzipsize * 100 / $origsize" | bc -l);
	printf "Original: %d bytes\n" "$origsize";
	printf "Gzipped:  %d bytes (%2.2f%%)\n" "$gzipsize" "$ratio";
}
