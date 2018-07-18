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


# Aliases for programs that mightn't be available/installed
have bundle   && alias brake='bundle exec rake'
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


# Enable coloured grep output if supported
(echo . | grep . --colour=auto 2>&1)>/dev/null && {
	alias grep='grep --colour=auto'
	alias fgrep='fgrep --colour=auto'
	alias egrep='egrep --colour=auto'
};


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


# Show every xterm colour
alias rainbow='for i in {1..255}; do printf "$i\t\e[38;5;${i}m$(printf %s {A..Z} {a..z})\t\e[48;5;${i}m$(jot -nb" " -s" " 30 0)\e[0m\n"; done | less -R'


# Copy the last command to the clipboard. Useful for saving helpful one-liners.
alias copythat='printf %s "$(history | tail -2 | head -1 | sed s/^[[:space:]]*[[:digit:]]*[[:space:]]*//)" | clip'


# Irrevocably annihilate a file
have shred && alias nuke='shred -u' || alias nuke='rm -rfP'


# Search on GitHub for a filename or file extension
alias ghext='gh-search ext'
alias ghname='gh-search name'
alias ghlang='gh-search lang'


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
	;;
esac
