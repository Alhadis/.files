#==============================================================================
#	COMMON SHORTHAND
#==============================================================================
alias bc='bc -l'
alias g='git status'
alias ga='git add . --all'
alias gl='git log'
alias l='ls -alh'
alias p='pbpaste | purify | trim | sed -E s/"\r\n"/"\n"/ | tr "\r" "\n" | pbcopy'
alias u='pbpaste | uglifyjs --mangle 2>/dev/null | pbcopy'
alias wl='watchman watch-list'
alias wlt='watchman trigger-list $(pwd)'
alias ..='cd ..'



#==============================================================================
#	PROGRAM DEFAULTS
#==============================================================================
alias sudo='sudo '
alias cpan='sudo cpan'
alias md5='md5 -q'
alias node='node --es_staging'
alias passgen='passgen -c -l 24'



#==============================================================================
#	ALIASES THAT'RE LITERALLY ALIASES
#==============================================================================
alias woff-decode='woff2sfnt'
alias woff-decompress='woff2sfnt'
alias woff2-compress='woff2_compress'
alias woff2-decompress='woff2_decompress'
alias woff2-decode='woff2_decompress'
alias woff2-encode='woff2_compress'




#==============================================================================
#	USEFUL ONE-LINERS
#==============================================================================

# Get rid of that sodding 1px-border running along Chrome's edges
alias fit-chrome='osascript -e '"'"'tell first window of application "Google Chrome" to set bounds to {0, 0, 1440, 820}'"'"


# Swipe the staging area clean and revert to the last commit
alias fuck-this-shit='git stash; git stash drop; git gc; git prune -v; git clean -fd;'


# Undo last commit (as long as it's not been pushed)
alias fuck-that-shit='git reset --soft HEAD~1;'


# Generate 10 paragraphs of Ancient Roman "wat"
alias lipsum='lorem-ipsum 10 paragraphs'


# Strip embedded metadata from a file or directory of files
alias strip-meta='exiftool $@ "-All=" -overwrite_original'


# Remove com.apple.quarantine attribute recursively
alias unquarantine='find .  -print0 -type f -o -type d | xargs -0 xattr -d com.apple.quarantine 2>/dev/null;'


# Temporary workaround until I get WP-HookTracer finished
alias hooks-on='perl -pi -e '"'"'s/(static\s+\$trace_(?:filters|actions)\s*=\s*)\d/${1}1/g'"'"' hooktracer.php'
alias hooks-off='perl -pi -e '"'"'s/(static\s+\$trace_(?:filters|actions)\s*=\s*)\d/${1}0/g'"'"' hooktracer.php'
alias hooks-reset='> ~/Documents/apache.log'




#==============================================================================
#	LOOTED FROM BELGIAN FLY-KICK GUY:
#	*	https://github.com/mathiasbynens/dotfiles/blob/master/.aliases
#==============================================================================

# IP addresses
alias ip='dig +short myip.opendns.com @resolver1.opendns.com'
alias localip='ipconfig getifaddr en0'

# Clean up LaunchServices to remove duplicates in the "Open With" menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

# Empty the Trash on all mounted volumes and the main HDD.
# Also, clear Apple's System Logs to improve shell startup speed.
# Finally, clear download history from quarantine. https://mths.be/bum
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"