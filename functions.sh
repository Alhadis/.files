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
	brew update --verbose;
	brew upgrade --all --verbose;
	brew cleanup;
	
	# NPM modules
	marker "Updating Node modules"
	npm -g update;
	
	# Ruby gems
	marker "Updating Ruby gems" 
	gem update --system;
	gem update;
	
	# Update forks/copies of other people's repos
	marker "Updating forked repositories"
	~/Forks/update
}




#==============================================================================
#	FOLLOWING FUNCTIONS ALL SHAMELESSLY PINCHED FROM THESE LOVELY CHAPS:
#
#	*	https://github.com/mathiasbynens/dotfiles
#	*	https://github.com/paulirish/dotfiles
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


# Simple calculator
function calc(){
	local result="";
	result="$(printf "scale=10;$*\n" | bc --mathlib | tr -d '\\\n')";
	if [[ "$result" == *.* ]]; then
		# Improve the output for decimal numbers
		printf "$result" |
		sed -e 's/^\./0./'         # add "0" for cases like ".5"  \
		    -e 's/^-\./-0./'       # add "0" for cases like "-.5" \
		    -e 's/0*$//;s/\.$//';  # remove trailing zeros
	else
		printf "$result";
	fi;
	printf "\n";
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