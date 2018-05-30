PATH=/sbin:/usr/sbin:/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/X11R6/bin
PATH=~/.files/bin:$PATH:~/Mirrors/depot_tools
export PATH

export DWBHOME=/usr/local/dwb
export GPG_TTY=`tty`
export GREP_COLORS="sl=38;5;240:mt=1;38;5;10;48;5;22:fn=38;5;242:se=38;5;237:ln=38;5;10"
export HISTFILE=
export LESS=-R
export LYNX_CFG=~/.files/etc/lynx.cfg
export NODE_REPL_HISTORY=
export PRINTER=Officejet-6500-E710n-z


# Perl/CPAN modules path
export PERL5LIB=~/perl5/lib/perl5:$PERL5LIB
export PERL_LOCAL_LIB_ROOT=~/perl5:$PERL_LOCAL_LIB_ROOT
export PERL_MB_OPT='--install_base "~/perl5"'
export PERL_MM_OPT="INSTALL_BASE=~/perl5"

# Prefer Emacs for terminal-based editing
EDITOR=`command -v emacs 2>/dev/null`
[ -x "$EDITOR" ] && {
	GIT_EDITOR=$EDITOR
	VISUAL=$EDITOR
	export EDITOR GIT_EDITOR VISUAL
}

# OpenBSD: Use GCC from ports-tree
[ -x /usr/local/bin/egcc ] && {
	CC=/usr/local/bin/egcc
	CXX=/usr/local/bin/eg++
	export CC CXX
}

# OpenBSD: Additional documentation
MANPATH=:~/.files/share/man
MANPATH=$MANPATH:/usr/local/heirloom-doctools/man
MANPATH=$MANPATH:/usr/local/lib/node_modules/npm/man
export MANPATH

# Node.js: Enable cutting-edge language features
NODE_OPTIONS="--experimental-modules"
NODE_OPTIONS="$NODE_OPTIONS --experimental-repl-await"
NODE_OPTIONS="$NODE_OPTIONS --experimental-vm-modules"
export NODE_OPTIONS

# NPM: Workaround for npm#20623
alias npm='NODE_OPTIONS= npm'
alias npx='NODE_OPTIONS= npx'
