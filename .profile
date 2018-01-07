# $OpenBSD: dot.profile,v 1.9 2010/12/13 12:54:31 millert Exp $
#
# sh/ksh initialization

PATH=/sbin:/usr/sbin:/bin:/usr/bin:/usr/X11R6/bin:/usr/local/sbin:/usr/local/bin
PATH=~/.files/bin:~/.files/perl5/bin:$PATH
export PATH

# Perl/CPAN environment
export PERL5LIB=~/perl5/lib/perl5:$PERL5LIB
export PERL_LOCAL_LIB_ROOT=~/perl5:$PERL_LOCAL_LIB_ROOT
export PERL_MB_OPT='--install_base "~/perl5"'
export PERL_MM_OPT="INSTALL_BASE=~/perl5"

# Aliases
alias @='emacs'
alias p='pwd'
alias K='kill -KILL'
alias F='cd ~/.files'
alias E='cd ~/.files/.emacs.d'
alias P='cd /home/projects'
alias desktop='startxfce4 --with-ck-launch'
alias fixown='find . -exec chown -h Alhadis:wheel {} +;'
alias make='gmake'
alias stat='stat -x'
alias unlink='rm'
alias unmount='umount'
alias untar='bsdtar -xf'
alias usb='doas mount -t msdos /dev/sd2i /mnt'

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

# Additional documentation
MANPATH=:~/.files/share/man
MANPATH=$MANPATH:/usr/local/heirloom-doctools/man
MANPATH=$MANPATH:/usr/local/lib/node_modules/npm/man
export MANPATH

# Commonly-referenced paths
export F=~/.files
export P=~/.files/.profile
export E=~/.files/.emacs.d/init.el

# Everything else
export LESS=-R
export LYNX_CFG=~/.files/etc/lynx.cfg
export EDITOR=/usr/local/bin/emacs
export VISUAL=/usr/local/bin/emacs
export CC=/usr/local/bin/egcc
export CXX=/usr/local/bin/eg++
umask 022


# Shorthand for finding files by name
f(){
	find . -type f -name "*$1*";
}


# Run a Google search
# - Usage: `google [query string]'
google(){
	lynx 'https://google.com/search?q='"$(printf "%s" "$@" \
	| sed -Ee 's/%/%25/g; s/\+/%2B/g; s/\?/%3F/g; s/#/%23/g; s/\&/%26/g;')";
}


# Browse OpenBSD's ports tree
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
