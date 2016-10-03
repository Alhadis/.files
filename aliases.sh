#==============================================================================
#   COMMON SHORTHAND
#==============================================================================
alias au='apm update --no-confirm'
alias bc='bc -l'
alias brake='bundle exec rake'
alias bup='brew update; [ "$(brew outdated)" ] && brew upgrade --all && brew cleanup --prune=0 || true;'
alias c='calc'
alias e='elisp'
alias fs='pbpaste | filesafe | pbcopy'
alias G:='cd ~/VirtualBox/Shared'
alias g='git status'
alias ga='git add . --all'
alias gb='git branch -av'
alias gc='git gc && git prune -v'
alias gd='git diff --cached -w'
alias gl='git log'
alias gp='git push origin'
alias gr='git remote --verbose'
alias k='make'
alias l='ls -alh'
alias m='make'
alias mk='make'
alias mans='mansrc'
alias nah='git checkout -- .'
alias p='perl -pe '
alias src='show_src'
alias tl='pbpaste | clean-discogs | purify | pbcopy'
alias u='pbpaste | uglifyjs -c --mangle | pbcopy'
alias uf='update-font.sh'
alias untag='xattr -d com.apple.metadata:_kMDItemUserTags 2>/dev/null'
alias vopt='opt -v'
alias wl='watchman watch-list'
alias wlt='watchman trigger-list $(pwd)'
alias w80="perl -p -i -e 's/.{80}/$&\n/g'"
alias yeah="git reset HEAD .;"
alias ..='cd ..'



#==============================================================================
#   PROGRAM DEFAULTS
#==============================================================================
alias sudo='sudo '
alias less='less -R'
alias md5='md5 -q'
alias node='node --es_staging'



#==============================================================================
#   KEG-ONLY HOMEBREW FORMULAE
#==============================================================================
alias curl='/usr/local/opt/curl/bin/curl'
alias cc='clang'
alias clang='/usr/local/opt/llvm/bin/clang'



#==============================================================================
#   ALIASES THAT'RE LITERALLY ALIASES
#==============================================================================
alias embed-atom-icon='embed_atom_icon'
alias ghext='gh-ext'
alias gh-file='gh-name'
alias git-hooks='git_hooks'
alias ma='ma-move'
alias ma-save='ma_save'
alias to-otf='to_otf'
alias woff-decode='woff2sfnt'
alias woff-decompress='woff2sfnt'
alias woff2-compress='woff2_compress'
alias woff2-decompress='woff2_decompress'
alias woff2-decode='woff2_decompress'
alias woff2-encode='woff2_compress'




#==============================================================================
#   USEFUL ONE-LINERS
#==============================================================================

# Get rid of that sodding 1px-border running along Chrome's edges
alias fit-chrome='osascript -e '"'"'tell first window of application "Google Chrome" to set bounds to {0, 0, 1440, 800}'"'"


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


# Search for substring in CWD
alias s='grep -irnw . -e '


# Hide/show files or directories in Finder
alias hide='chflags hidden'
alias show='chflags nohidden'


# ROT13 "encryption". Usage: echo "String" | rot13
alias rot13="tr 'A-Za-z' 'N-ZA-Mn-za-m'"


# Output the last shell command as a string
alias last-command='history | tail -n2 | head -n1 | sed -r "s/^\s+[0-9]+\*?\s+//"'


# Copy the last command to the clipboard. Useful for saving helpful one-liners.
alias copy-that='printf %s "$(last-command)" | pbcopy'


# Strip lines from STDIN that start with a # or ;
# Useful for ignoring comment-lines in configuration files
alias strip-comments="sed -r '/^\s*([#;].*)?$/d'"


# Highlight the current date on the calendar
# This renders the cal command's options unusable, so make sure you won't be needing -m -y or whatever
alias cal='cal | sed -r '"'"'s/(^|\s)('"'"'$(echo $(date +%d) | sed s/^0//)'"'"')(\s|$)/\1'"'"'"\x1B[38;5;76m"'"'"'\2'"'"'"\x1B[0m"'"'"'\3/'"'"''


# Clear Terminal's scrollback.
# I sometimes get confused when switching between Windows in VirtualBox and Mac OS. :c
alias cls='osascript -e '"'"'tell application "System Events" to keystroke "k" using command down'"';"


# Actually clear Terminal's history
alias clear='history -c; > ~/.bash_history'


# Search on GitHub for a filename or file extension
alias gh-ext='gh_search ext'
alias gh-name='gh_search name'
alias gh-lang='gh_search lang'


# Pull recently-taken photos off my phone
alias yoink='adb pull storage/extSdCard/DCIM/Camera ~/Desktop; dsclean;'


# Clean the clipboard's contents of impurities
alias purify='pbpaste | purify | trim | sed -E s/"\r\n"/"\n"/ | tr "\r" "\n" | pbcopy'


# Temporary workaround until I get WP-HookTracer finished
alias hooks-on='perl -pi -e '"'"'s/(static\s+\$trace_(?:filters|actions)\s*=\s*)\d/${1}1/g'"'"' hooktracer.php'
alias hooks-off='perl -pi -e '"'"'s/(static\s+\$trace_(?:filters|actions)\s*=\s*)\d/${1}0/g'"'"' hooktracer.php'
alias hooks-reset='> ~/Documents/apache.log'




#==============================================================================
#   LOOTED FROM BELGIAN FLY-KICK GUY:
#   *   https://github.com/mathiasbynens/dotfiles/blob/master/.aliases
#==============================================================================

# IP addresses
alias ip='dig +short myip.opendns.com @resolver1.opendns.com'
alias localip='ipconfig getifaddr en0'

# Clean up LaunchServices to remove duplicates in the "Open With" menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

# Empty the Trash on all mounted volumes and the main HDD.
# Also, clear Apple's System Logs to improve shell startup speed.
# Finally, clear download history from quarantine. https://mths.be/bum
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; rm -rfv ~/.emacs.d/auto-save-list && mkdir ~/.emacs.d/auto-save-list; sudo rm -rfv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"
