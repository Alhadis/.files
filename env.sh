PATH=/sbin:/usr/sbin:/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/X11R6/bin:/usr/games
PATH=~/.files/bin:$PATH
export PATH

export DWBHOME=/usr/local/dwb
export GPG_TTY=`tty`
export GREP_COLORS="sl=38;5;240:mt=1;38;5;10;48;5;22:fn=38;5;242:se=38;5;237:ln=38;5;10"
export HISTFILE=
export LESS=-R
export LYNX_CFG=~/.files/etc/lynx.cfg
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

# Prefer LLVM for building projects
export CC=clang
export CXX=clang++

# Add Google's Depot-Tools to $PATH if they're installed
[ -d ~/Mirrors/depot_tools ] && PATH=$PATH:~/Mirrors/depot_tools


# OpenBSD: Additional documentation
MANPATH=:~/.files/share/man
MANPATH=$MANPATH:/usr/local/lib/node_modules/npm/man

# Include other Troff implementations in search paths
for path in "/usr/local/heirloom-doctools" "$DWBHOME"; do 
	[ -d "$path/bin" ] && PATH="$path/bin:$PATH"
	[ -d "$path/man" ] && MANPATH="$MANPATH:$path/man"
done
unset path
export PATH MANPATH
