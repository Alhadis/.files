#==============================================================================
#   COMMON SHORTHAND
#==============================================================================
alias brake='bundle exec rake'
alias bup='brew update; [ "$(brew outdated)" ] && brew upgrade && brew cleanup --prune=0 || true;'
alias c='calc'
alias fd='git clean -fd'
alias fj='t 2>&1 | tabfix | perl -pe "s/^\t+//g; s/Expected | to equal /\n/g;" | pbcopy'
alias g='git status'
alias ga='git add . --all'
alias gb='git branch -av'
alias gc='git gc && git prune -v'
alias gd='git diff --cached -w'
alias gl='git log'
alias gp='git push origin'
alias gr='git remote --verbose'
alias gs='git show'
alias l='ls -alh'
alias m='make'
alias nah='git checkout -- .'
alias p='perl -pe '
alias PS='git commit --amend'
alias untag='xattr -d com.apple.metadata:_kMDItemUserTags 2>/dev/null'
alias yeah="git reset HEAD .;"
alias ..='cd ..'



#==============================================================================
#   PROGRAM DEFAULTS
#==============================================================================
alias bc='bc -l'
alias su='sudo su -l'
alias sudo='sudo '
alias less='less -R'
alias md5='md5 -q'
alias node='node --es_staging'
alias stat='stat -x'



#==============================================================================
#   HOMEBREW FORMULAE
#==============================================================================
alias curl='/usr/local/opt/curl/bin/curl'
alias cc='clang'
alias clang='/usr/local/opt/llvm/bin/clang'
alias ghostscript='/usr/local/bin/gs'



#==============================================================================
#   ALIASES THAT'RE LITERALLY ALIASES
#==============================================================================
alias ghext='gh-ext'
alias gh-file='gh-name'
alias woff-decode='woff2sfnt'
alias woff-decompress='woff2sfnt'
alias woff2-compress='woff2_compress'
alias woff2-decompress='woff2_decompress'
alias woff2-decode='woff2_decompress'
alias woff2-encode='woff2_compress'




#==============================================================================
#   USEFUL ONE-LINERS
#==============================================================================

# Restore Chrome's dimensions after accidentally resizing
alias fit-chrome='osascript -e '"'"'tell first window of application "Google Chrome" to set bounds to {0, 0, 1440, 800}'"'"


# Swipe the staging area clean and revert to the last commit
alias fuck-this-shit='git stash; git stash drop; git gc; git prune -v; git clean -fd;'


# Undo last commit (as long as it's not been pushed)
alias fuck-that-shit='git reset --soft HEAD~1;'


# Strip embedded metadata from a file or directory of files
alias strip-meta='exiftool $@ "-All=" -overwrite_original'


# Remove com.apple.quarantine attribute recursively
alias unquarantine='find .  -print0 -type f -o -type d | xargs -0 xattr -d com.apple.quarantine 2>/dev/null;'


# Search for substring in CWD
alias s='grep -irnw . -e '


# Copy the last command to the clipboard. Useful for saving helpful one-liners.
alias copy-that='printf %s "$(history | tail -n2 | head -n1 | sed -r '"'s/^\s+[0-9]+\*?\s+//'"')" | pbcopy'


# Highlight the current date on the calendar
# This renders the cal command's options unusable, so make sure you won't be needing -m -y or whatever
alias cal='cal | sed -r '"'"'s/(^|\s)('"'"'$(echo $(date +%d) | sed s/^0//)'"'"')(\s|$)/\1'"'"'"\x1B[38;5;76m"'"'"'\2'"'"'"\x1B[0m"'"'"'\3/'"'"''


# Clear Terminal's scrollback.
alias cls='osascript -e '"'"'tell application "System Events" to keystroke "k" using command down'"';"


# Actually clear Terminal's history
alias clear='history -c; > ~/.bash_history'


# Search on GitHub for a filename or file extension
alias gh-ext='gh-search ext'
alias gh-name='gh-search name'
alias gh-lang='gh-search lang'


# Pull recently-taken photos off my phone
alias yoink='adb pull storage/extSdCard/DCIM/Camera ~/Desktop; dsclean;'


# Crap to help macOS run smoother. Credit: https://mths.be/bum
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; rm -rfv ~/.emacs.d/auto-save-list && mkdir ~/.emacs.d/auto-save-list; sudo rm -rfv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"
