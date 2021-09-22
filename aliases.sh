# Shorthand
alias ..='cd ..'
alias L='l -tr'
alias l='LC_COLLATE=C ls -alh'
alias p='pwd'
alias c="printf '\033[3J\033c'"
alias t='run-tests'


# Git shorthand
have git && {
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
alias scp='scp -pr'
alias nl='nl -ba'


# Aliases for programs that mightn't be available/installed
have bundle   && alias brake='LANG=en_AU.UTF-8 bundle exec rake'
have fdupes   && alias dedupe='fdupes -dN'
have file     && alias mime='file --brief --mime-type'
have mysql    && alias mysql='mysql --auto-vertical-output'
have plutil   && alias pl='convert-plist'
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



# Recursively search for a pattern in current working directory
alias s=~/.files/bin/rgrep
(echo . | grep -r . 2>&1)>/dev/null && alias s='grep -irn . -e'


# Enable coloured grep output if supported
(echo . | grep . --colour=auto 2>&1)>/dev/null && {
	alias grep='grep --colour=auto'
	alias fgrep='fgrep --colour=auto'
	alias egrep='egrep --colour=auto'
};


# Hide summary lines in tree(1) output
have tree && if tree -a --noreport ~/.files/share >/dev/null 2>&1
	then alias tree='tree -a --noreport'
	else alias tree='tree -as'
fi


# Hide GNU bc(1)'s startup message
(echo quit | bc --quiet 2>&1)>/dev/null && alias bc='bc --quiet -l'


# Prefer GNU units(1) over the older BSD-licensed version
have gunits && alias units='gunits'


# Prefer authentic vi(1) implementations over Vim's symlinks
have nvi && (vi --version | grep -iq VIM 2>&1)>/dev/null && alias vi=nvi
have nex && (ex --version | grep -iq VIM 2>&1)>/dev/null && alias ex=nex


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
[ "$DISPLAY" ] || have startxfce4 && alias desktop='startxfce4'


# Display bytes 0—255 in terminal's current encoding
have jot && alias ansi='jot -ns '\'\'' -c 256 0 | { [ -t 1 ] && LANG=C fold -bw16 && echo || tee; }'


# Copy a hard tab (U+0009) to the system's clipboard
alias tab='printf "\t" | clip'


# Chop leading or trailing blank lines from input
alias trimstart='sed -n $'"'"'/[^ \t]/,$p'"'"
alias trimend='sed -e :a -e '"'"'/^\n*$/{$d;N;};/\n$/ba'"'"


# Delete broken symlinks in the current directory
alias prune='find -L . -name . -o -type d -prune -o -type l -exec rm -v {} +'


# Copy the last command to the clipboard. Useful for saving helpful one-liners.
alias copythat='printf %s "$(history | tail -2 | head -1 | sed s/^[[:space:]]*[[:digit:]]*[[:space:]]*//)" | clip'


# Copy a JavaScript one-liner for listing globals. Sometimes needed when running certain REPLs.
fn='var pp = x => Object.getOwnPropertyNames(x).concat(Object.getOwnPropertySymbols(x))'
alias pp='printf %s '\'"$fn"'.map(n => String(n)).sort().join("\n"); pp(globalThis)'\'' | clip'
unset fn


# Order-of-operations check. Runs clipboard contents through Terser to reveal which brackets are unnecessary.
have terser && alias ooc='clip | sed '\''s/^[^=]*$/_=&/'\'' | terser -mc'


# Irrevocably annihilate a file
have shred && alias nuke='shred -u' || alias nuke='rm -rfP'


# Bring up notes for things I keep forgetting
alias notes='less ~/.files/share/misc/random-notes.md'


# Codepoint and base conversion
for fn in chr oct hex; do alias "$fn"="perl -E 'say join $/, map $fn, -t ? @ARGV : map split, <>'"; done;
alias ord='perl -mEncode=decode -E "map { printf \"%1\\\$s\tU+%1\\\$X\n\", ord decode \"UTF-8\", \$_ } -t ? @ARGV : map split, <>"'
unset fn


# Generate nicer-looking hexadecimal dumps
have xxd     && alias xxd='xxd -u -g1'
have hexdump && alias hexdump='hexdump -v \
	-e \""[2m│[22m0x%08.8_ax[2m│[22m "\" \
	-e '\''16/1 "%02X "'\'' \
	-e \"" [2m│[22m"\" \
	-e '\''16/1 "%_p" "'\''"[2m│[22m"'\''\n"'\'


# Convert a font using FontForge
have fontforge && alias convertfont='~/.files/etc/convert-font.ff 2>/dev/null'


# OS-specific
case `uname -s` in
	OpenBSD)
		# Reconnect WiFi
		alias reconnect='doas sh /etc/netstart iwn0'

		# Turn off the computer after shutting down
		alias halt='halt -p'
		
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
		alias pkg_add='doas pkg_add'
		alias pkg_delete='doas pkg_delete'
		have mount.exfat-fuse && {
			alias mount.exfat='doas mount.exfat'
			alias mount.exfat-fuse='doas mount.exfat-fuse'
		}
	;;

	Darwin)
		# Update installed Homebrew formulae
		alias bup='brew update && brew upgrade && brew cleanup --prune=0'
		
		# Resize Terminal.app to fill the screen
		alias fit='printf '\''\e[3;0;0t\e[4;0;9999t'\'
		
		# Print temperature diagnostics
		alias temp='sudo powermetrics --samplers smc -i1 -n1 | grep --colour=none "^Fan\|temp"'
		
		# Print power diagnostics (battery-level and charge status)
		alias pow='pmset -g batt'
		
		# Apple recommend diskutil(1) be used instead of umount(1)
		alias umount='diskutil unmount'
		
		# Remove annoying extended attributes added to downloads
		alias unquarantine='xattr -d com.apple.quarantine * 2>/dev/null || true'
		
		# Alias unreachable commands specific to macOS
		alias scrub='~/.files/etc/darwin/scrub.sh'
		alias PlistBuddy='/usr/libexec/PlistBuddy'
		
		# Mount temporary case-sensitive filesystem
		alias tempfs='hdiutil create -size 512m -fs "Case-sensitive APFS" -type UDIF -nospotlight -attach'
	;;
esac
