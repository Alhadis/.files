PATH=/sbin:/usr/sbin:/bin:/usr/bin:/usr/X11R6/bin:/usr/local/sbin:/usr/local/bin
PATH=~/.files/bin:$PATH:~/Mirrors/depot_tools
export PATH
eval "`perl -I$HOME/perl5/lib/perl5 -Mlocal::lib`"

export GREP_COLORS="sl=38;5;240:mt=1;38;5;10;48;5;22:fn=38;5;242:se=38;5;237:ln=38;5;10"
export HISTFILE=
export LESS=-R
export LYNX_CFG=~/.files/etc/lynx.cfg
export NODE_REPL_HISTORY=


# Prefer Emacs for terminal-based editing
EDITOR=`command -v emacs 2>/dev/null`
[ -x "$EDITOR" ] && {
	VISUAL=$EDITOR
	export EDITOR VISUAL
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
