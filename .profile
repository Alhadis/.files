# $OpenBSD: dot.profile,v 1.9 2010/12/13 12:54:31 millert Exp $
#
# sh/ksh initialization

PATH=/sbin:/usr/sbin:/bin:/usr/bin:/usr/X11R6/bin:/usr/local/sbin:/usr/local/bin
PATH=~/.files/bin:$PATH
export PATH
: ${HOME='/root'}
export HOME
umask 022

# Aliases
alias @='emacs'
alias p='pwd'
alias K='kill -KILL'
alias F='cd /root/.files'
alias P='cd /home/projects'
alias E='cd /root/.files/.emacs.d'
alias desktop='startxfce4 --with-ck-launch'
alias fixown='find . -exec chown -h root:wheel {} +;'
alias make='gmake'
alias untar='bsdtar -xf'

# Aliases copied from `aliases.sh`
alias l='ls -alh'
alias ..='cd ..'
alias c='clear'
alias df='df -h'
alias du='du -h'
alias g='git status'
alias gl='git log'
alias gd='git diff --cached'
alias ga='git add --all'
alias gb='git branch -av'
alias gc='git gc && git prune -v'
alias gr='git remote --verbose'
alias gs='git show'
alias GS='/usr/local/bin/gs'
alias gp='git push'
alias yeah='git reset HEAD .;'
alias nah='git checkout -- . && git clean -fd'
alias PS='git commit --amend'
alias fuck-that-shit='git reset --soft HEAD~1'
alias s='grep -irnw . -e '
alias t='run-tests'

{
	export GPG_TTY=`tty`
	eval "$(ssh-agent -s)" >/dev/null
	ssh-add ~/.ssh/github
	gpg-agent
} 2>/dev/null;

# Add paths containing additional manual-pages
MANPATH=:~/.files/share/man
MANPATH=$MANPATH:/usr/local/heirloom-doctools/man
MANPATH=$MANPATH:/usr/local/lib/node_modules/npm/man
export MANPATH

# Path shortcuts
export F=/root/.files
export P=/root/.files/.profile
export E=/root/.files/.emacs.d/init.el

# Everything else
export LESS=-R
export EDITOR=/usr/local/bin/emacs
export VISUAL=/usr/local/bin/emacs


# Shorthand for finding files by name
f(){
	find . -type f -name "*$1*";
}


# Browse OpenPSD's ports tree
# - Usage: `openports [version]'
openports(){
	for ver in $1 $(uname -r); do
		lynx "$(cat /etc/installurl)/${ver}/packages/$(machine)/";
		break;
	done;
}


# Print temperature diagnostics
temp(){
	sysctl hw.sensors | grep temp | \
	sed 's/hw.sensors.//; s/\.temp[0-9]=/: /;'
}


# Interactive shell setup
case "$-" in *i*)
if [ -x /usr/bin/tset ]; then
	if [ X"$XTERM_VERSION" = X"" ]; then
		eval `/usr/bin/tset -sQ '-munknown:?vt220' $TERM`
	else
		eval `/usr/bin/tset -IsQ '-munknown:?vt220' $TERM`
	fi
fi ;;
esac
