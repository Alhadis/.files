#==============================================================================
#   COMMON SHORTHAND
#==============================================================================
alias b='brew info'
alias B='brew home'
alias cask='brew cask'
alias brake='bundle exec rake'
alias bup='brew update; [ ! "$(brew outdated)" ] || { cask outdated; brew upgrade && brew cleanup --prune=0; cask cleanup; }'
alias dedupe='fdupes -dNI '
alias fixperm='find . -type f -exec chmod 0640 {} +; find . -type d -exec chmod 0700 {} +'
alias fixown='find . -exec chown johngardner:staff {} +'
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
alias map='xargs -n1'
alias mime='file -b --mime-type'
alias nah='git checkout -- . && git clean -fd'
alias nuke='rm -rfvP'
alias PS='git commit --amend'
alias t='run-tests'
alias umount='diskutil unmount'
alias untag='xattr -d com.apple.lastuseddate#PS'
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
alias dash='PS1="$ " dash'
alias ksh='PS1="% " ksh'
alias zsh='PS1="%m%#" ksh'



#==============================================================================
#   PROGRAM REPLACEMENTS / EXECUTABLES OUTSIDE $PATH
#==============================================================================
alias checkbashisms='PERL5LIB= /usr/local/bin/checkbashisms'
alias curl='/usr/local/opt/curl/bin/curl -K ~/.files/.curlrc'
alias cc='clang'
alias clang='/usr/local/opt/llvm/bin/clang'
alias plistbuddy='/usr/libexec/PlistBuddy'



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


# Show every xterm colour
alias rainbow='for i in {1..255}; do printf "$i\t\x1B[38;5;${i}m$(printf %s {A..Z} {a..z})\t\x1B[48;5;${i}m$(jot -nb" " -s" " 30 0)\x1B[0m\n"; done | less -R'


# Recursively delete an extended attribute
alias untag='xargs -J% xattr 2>/dev/null -d % * <<<'


# Search for substring in CWD
alias s='grep -irnw . -e '


# Copy the last command to the clipboard. Useful for saving helpful one-liners.
alias copy-that='printf %s "$(history | tail -2 | head -1 | sed s/^[[:space:]]*[[:digit:]]*[[:space:]]*//)" | pbcopy'


# Highlight the current date on the calendar
alias cal='cal -h | sed "s/\b$(date +%d)\b/\x1B[38;5;10m&\x1B[0m/"'


# Search on GitHub for a filename or file extension
alias gh-ext='gh-search ext'
alias gh-name='gh-search name'
alias gh-lang='gh-search lang'


# Pull recently-taken photos off my phone
alias yoink='adb pull storage/extSdCard/DCIM/Camera ~/Desktop'


# Codepoint and base conversion
for i in chr oct hex; do alias "$i"="perl -E 'say join $/, map $i, -t ? @ARGV : map split, <>'"; done;
alias ord='perl -mEncode=decode -E "map { printf \"%1\\\$s\tU+%1\\\$X\n\", ord decode \"UTF-8\", \$_ } -t ? @ARGV : map split, <>"'


# Crap to help macOS run smoother. Credit: https://mths.be/bum
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"
alias emptytrash="sudo rm -rfPv /Volumes/*/.Trashes; sudo rm -rfPv ~/.Trash; rm -rfPv ~/.emacs.d/auto-save-list && mkdir ~/.emacs.d/auto-save-list; sudo rm -rfPv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"
