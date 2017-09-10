#==============================================================================
#   COMMON SHORTHAND
#==============================================================================
alias b='brew info'
alias B='brew home'
alias cask='brew cask'
alias brake='bundle exec rake'
alias bup='brew update; [ ! "$(brew outdated)" ] || { cask outdated; brew upgrade && brew cleanup --prune=0; cask cleanup; }'
alias dash='PS1="λ " dash'
alias dedupe='fdupes -dNI '
alias fixperm='find . -type f -exec chmod 0640 {} +; find . -type d -exec chmod 0700 {} +'
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
alias k='git branch -D --remote'
alias l='ls -alh'
alias L='l -i'
alias m='make'
alias nah='git checkout -- . && git clean -fd'
alias PS='git commit --amend'
alias umount='diskutil unmount'
alias yeah="git reset HEAD .;"
alias ?='man'
alias ..='cd ..'



#==============================================================================
#   PROGRAM DEFAULTS
#==============================================================================
alias bc='bc -l'
alias df='df -H'
alias du='du -h'
alias su='sudo su -l'
alias sudo='sudo '
alias less='less -R'
alias md5='md5 -q'
alias grep='grep --colour=auto'
alias fgrep='fgrep --colour=auto'
alias egrep='egrep --colour=auto'
alias mandoc='mandoc -aOindent=8,width=$(($(tput cols)-24))'
alias node='node --es_staging'
alias stat='stat -x'



#==============================================================================
#   HOMEBREW FORMULAE
#==============================================================================
alias curl='/usr/local/opt/curl/bin/curl -K ~/.files/.curlrc'
alias cc='clang'
alias clang='/usr/local/opt/llvm/bin/clang'
alias ghostscript='/usr/local/bin/gs'
alias python='python2'
alias pip='pip2'




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


# Chop leading or trailing blank lines from input
alias trim-start='sed -n $'"'"'/[^ \t]/,$p'"'"
alias trim-end='sed -e :a -e '"'"'/^\n*$/{$d;N;};/\n$/ba'"'"


# Convert 2-space "soft-tabs" into actual tabs
alias tabfix='perl -pi -Xe '"'"'s|^(  )+|"\t"x(length($&)/2)|ge;'"'"


# Transpile CoffeeScript into ES6 using piped input
alias cough='coffee -b --no-header -p -s | tabfix'


# Remove com.apple.quarantine attribute recursively
alias unquarantine='find .  -print0 -type f -o -type d | xargs -0 xattr -d com.apple.quarantine 2>/dev/null;'


# Search for substring in CWD
alias s='grep -irnw . -e '


# Copy the last command to the clipboard. Useful for saving helpful one-liners.
alias copy-that='printf %s "$(history | tail -n2 | head -n1 | sed -r '"'s/^\s+[0-9]+\*?\s+//'"')" | pbcopy'


# Highlight the current date on the calendar
# This renders the cal command's options unusable, so make sure you won't be needing -m -y or whatever
alias cal='cal | sed -r '"'"'s/(^|\s)('"'"'$(echo $(date +%d) | sed s/^0//)'"'"')(\s|$)/\1'"'"'"\x1B[38;5;76m"'"'"'\2'"'"'"\x1B[0m"'"'"'\3/'"'"''


# Search on GitHub for a filename or file extension
alias gh-ext='gh-search ext'
alias gh-name='gh-search name'
alias gh-lang='gh-search lang'


# Pull recently-taken photos off my phone
alias yoink='adb pull storage/extSdCard/DCIM/Camera ~/Desktop'


# Crap to help macOS run smoother. Credit: https://mths.be/bum
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"
alias emptytrash="sudo rm -rfPv /Volumes/*/.Trashes; sudo rm -rfPv ~/.Trash; rm -rfPv ~/.emacs.d/auto-save-list && mkdir ~/.emacs.d/auto-save-list; sudo rm -rfPv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"
