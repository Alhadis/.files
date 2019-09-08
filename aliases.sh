# Shorthand
alias ..='cd ..'
alias l='LC_COLLATE=C ls -alh'
alias p='pwd'
alias c="printf '\033[3J'; clear"
alias s='grep -irnw . -e'
alias t='run-tests'


# Git shorthand
have git && {
	alias g='git status'
	alias ga='git add --all'
	alias gb='git branch -av'
	alias gc='git gc && git prune -v'
	alias gd='git diff --cached'
	alias gh='gh-clone'
	alias gl='git log'
	alias gp='git push'
	alias gr='git remote --verbose'
	alias gs='git show'
	alias fd='git clean -fd'
	alias PS='git commit --amend'
	alias yeah='git reset HEAD .;'
	alias nah='git checkout -- . && git clean -fd'
}


# Program defaults
alias bc='bc -l'
alias df='df -h'
alias du='du -h'
alias mv='mv -i'
alias cp='cp -i'
alias diff='diff -r'
alias scp='scp -pr'


# Aliases for programs that mightn't be available/installed
have bundle   && alias brake='LANG=en_AU.UTF-8 bundle exec rake'
have fdupes   && alias dedupe='fdupes -dN'
have file     && alias mime='file --brief --mime-type'
have mysql    && alias mysql='mysql --auto-vertical-output'
have sudo     && alias sudo='sudo '
have doas     && alias doas='doas '
have less     && alias ll='l | less'
have exiftool && {
	alias stripmeta='exiftool -All= -overwrite_original'
	alias x='exiftool'
}


# Programs known by other names on other systems
have pbcopy || { alias pbcopy=clip; alias pbpaste=clip; }
have sudo && ! have doas && alias doas=sudo



# Enable coloured grep output if supported
(echo . | grep . --colour=auto 2>&1)>/dev/null && {
	alias grep='grep --colour=auto'
	alias fgrep='fgrep --colour=auto'
	alias egrep='egrep --colour=auto'
};


# Hide GNU bc(1)'s startup message
(echo quit | bc --quiet 2>&1)>/dev/null && alias bc='bc --quiet -l'


# Ksh: Emulate Bash's history expansion
case ${SHELL##*/} in ksh)
	alias !!='`history | tail -n1 | cut -f2 | tee /dev/stderr`';;
esac


# Use libarchive's `bsdtar' to shim missing extraction commands
have bsdtar && {
	have untar || alias untar='bsdtar -xf'
	have unzip || alias unzip='bsdtar -xf'
}


# Shortcut to start window manager (ignored if it's running)
[ "$DISPLAY" ] || have startxfce4 && alias desktop='startxfce4 --with-ck-launch'


# Copy a hard tab (U+0009) to the system's clipboard
alias cptab='printf "\t" | clip'


# Convert 2-space "soft-tabs" into actual tabs
alias tabfix='perl -pi -Xe '"'"'s|^(  )+|"\t"x(length($&)/2)|ge;'"'"


# Chop leading or trailing blank lines from input
alias trimstart='sed -n $'"'"'/[^ \t]/,$p'"'"
alias trimend='sed -e :a -e '"'"'/^\n*$/{$d;N;};/\n$/ba'"'"


# Delete broken symlinks in the current directory
alias prune='find -L . -name . -o -type d -prune -o -type l -exec rm -v {} +'


# Copy the last command to the clipboard. Useful for saving helpful one-liners.
alias copythat='printf %s "$(history | tail -2 | head -1 | sed s/^[[:space:]]*[[:digit:]]*[[:space:]]*//)" | clip'


# Irrevocably annihilate a file
have shred && alias nuke='shred -u' || alias nuke='rm -rfP'


# Bring up notes for things I keep forgetting
alias notes='less ~/.files/etc/random-notes.md'


# Codepoint and base conversion
for fn in chr oct hex; do alias "$fn"="perl -E 'say join $/, map $fn, -t ? @ARGV : map split, <>'"; done;
alias ord='perl -mEncode=decode -E "map { printf \"%1\\\$s\tU+%1\\\$X\n\", ord decode \"UTF-8\", \$_ } -t ? @ARGV : map split, <>"'
unset fn


# OS-specific
case `uname -s` in
	OpenBSD)
		# Reconnect WiFi
		alias reconnect='doas sh /etc/netstart iwn0'

		# Print temperature diagnostics
		alias temp='sysctl hw.sensors | grep temp | sed "s/hw.sensors.//; s/\.temp[0-9]=/: /;"'

		# Mount USB stick
		alias usb='mount -t msdos /dev/sd"`(mount | grep -q /dev/sd2) && echo 3 || echo 2`"i /mnt'

		# Run the following commands as superuser by default
		alias chown='doas chown'
		alias chgrp='doas chgrp'
		alias chmod='doas chmod'
		alias mount='doas mount'
		alias umount='doas umount'
	;;

	Darwin)
		# Update installed Homebrew formulae
		alias bup='brew update && brew upgrade && brew cleanup --prune=0'
		
		# Print power diagnostics (battery-level and charge status)
		alias pow='pmset -g batt'
		
		# Purge workspace of junk I was sidetracked into Photoshopping during work
		alias purge='rm -fP ~/.Trash/* ~/.Trash/.DS_Store; clip -c 2>/dev/null; history -c;'
		
		# Apple recommend diskutil(1) be used instead of umount(1)
		alias umount='diskutil unmount'
		
		# Remove annoying extended attributes added to downloads
		alias unquarantine='xattr -d com.apple.quarantine * 2>/dev/null || true'
		
		# Alias unreachable commands specific to macOS
		alias fit='~/.files/etc/darwin/fit-terminal.scpt'
		alias scrub='~/.files/etc/darwin/scrub.sh'
		alias PlistBuddy='/usr/libexec/PlistBuddy'
	;;
esac
