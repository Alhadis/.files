alias b='brew info'
alias B='brew home'
alias bup='brew update; [ ! "$(brew outdated)" ] || { brew cask outdated; brew upgrade && brew cleanup --prune=0; brew cask cleanup; }'
alias cask='brew cask'
alias checkbashisms='PERL5LIB= /usr/local/bin/checkbashisms'
alias clang='/usr/local/opt/llvm/bin/clang'
alias curl='/usr/local/opt/curl/bin/curl -K ~/.files/.curlrc'
alias fixperm='find . -type f -exec chmod 0640 {} +; find . -type d -exec chmod 0700 {} +'
alias fixown='find . -exec chown johngardner:staff {} +'
alias fj='t 2>&1 | tabfix | perl -pe "s/^\t+//g; s/Expected | to equal /\n/g;" | pbcopy'
alias mandoc='mandoc -aOindent=8,width=$(($(tput cols)-24))'
alias md5='md5 -q'
alias plistbuddy='/usr/libexec/PlistBuddy'
alias su='sudo su -l'
alias umount='diskutil unmount'
alias untag='xattr -d com.apple.lastuseddate#PS'

# macOS uses an uppercased switch for displaying human-readable diskspace sizes
unalias df; alias df='df -H'

# Highlight the current date on the calendar
alias cal='cal -h | sed "s/\b$(date +%d)\b/\x1B[38;5;10m&\x1B[0m/"'

# Pull recently-taken photos off my phone
alias yoink='adb pull storage/extSdCard/DCIM/Camera ~/Desktop'

# Crap to help macOS run smoother. Credit: https://mths.be/bum
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"
alias emptytrash="sudo rm -rfPv /Volumes/*/.Trashes; sudo rm -rfPv ~/.Trash; rm -rfPv ~/.emacs.d/auto-save-list && mkdir ~/.emacs.d/auto-save-list; sudo rm -rfPv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"
