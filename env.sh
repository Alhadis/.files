# Configure $PATH
unset PATH
paths='
	~/.files/var/bin
	~/.files/bin
	~/bin
	/sbin
	/usr/sbin
	/bin
	/usr/local/sbin
	/usr/local/bin
	/usr/bin
	/usr/X11R6/bin
	/opt/local/sbin
	/opt/local/bin
	/opt/tools/sbin
	/opt/tools/bin
	/usr/games
	~/.wasmtime/bin
	~/.deno/bin
	~/.local/bin
	~/go/bin
	~/Forks/depot_tools
'; for path in $paths; do
	case $path in \~/*) path=~/${path#\~/};; esac
	[ -d "$path" ] && PATH="$PATH:$path";
done

# Define manual search paths
unset MANPATH
paths='
	~/.files/var/man
	~/.files/share/man
	/usr/local/share/man
	/usr/share/man
	/usr/X11/man
	/opt/X11/share/man
	/opt/local/man
	/opt/local/share/man
	/usr/local/lib/node_modules/npm/man
	/opt/tools/man
	~/perl5/man
	~/Forks/depot_tools/man
'; for path in $paths; do
	case $path in \~/*) path=~/${path#\~/};; esac
	[ -d "$path" ] && MANPATH="$MANPATH:$path"
done

export DICPATH=~/.files/share/dict
export DWBHOME=/usr/local/dwb
export GPG_TTY=`tty`
export GREP_COLORS="sl=38;5;240:mt=1;38;5;10;48;5;22:fn=38;5;242:se=38;5;237:ln=38;5;10"
export GROFF_TMAC_PATH=~/.files/var/tmac:~/Labs/Mono:/usr/local/share/groff/site-tmac
export GROFF_FONT_PATH=/usr/local/share/groff/site-font
export HISTFILE=
export LESS=-R
export LYNX_CFG=~/.files/etc/lynx.cfg
export WORDLIST=~/.files/share/dict/custom.txt
export WASMTIME_HOME=~/.wasmtime


# Perl/CPAN modules path
export PERL5LIB=~/perl5/lib/perl5:$PERL5LIB
export PERL_LOCAL_LIB_ROOT=~/perl5:$PERL_LOCAL_LIB_ROOT
export PERL_MB_OPT='--install_base "~/perl5"'
export PERL_MM_OPT="INSTALL_BASE=~/perl5"

# Prefer Emacs for terminal-based editing
EDITOR=`command -v emacs 2>/dev/null`
[ -x "$EDITOR" ] && {
	EDITOR="$EDITOR -nw"
	GIT_EDITOR=$EDITOR
	VISUAL=$EDITOR
	export EDITOR GIT_EDITOR VISUAL
}

# Node: Configure NPM and global search directories
have npm && {
	export ADBLOCK=1     # Filter useless crap when installing
	export NPM_TOKEN=0   # Stop gripes about missing tokens
	
	# Prepend linked and globally-installed modules to search path
	export NODE_PATH=~/.files/var/node:/usr/local/lib/node_modules:/usr/lib/node_modules
	
	# Force ASCII output for non-graphical displays
	[ "$DISPLAY" ] || {
		export npm_config_unicode=false
		export npm_config_heading='|'
	};
}

# Nodebrew: Include paths for currently-selected version
have nodebrew && [ -x ~/.nodebrew/current/bin/node ] && {
	PATH=~/.nodebrew/current/bin:"$PATH"
	MANPATH=~/.nodebrew/current/share/man:"$MANPATH"
}

# Yarn: Paths added to .bashrc upon installation
for path in ~/.yarn/ ~/.config/yarn/global/node_modules/.; do
	[ -d "${path}bin" ] && PATH="$PATH:${path}bin"
	[ -d "${path}man" ] && MANPATH="$MANPATH:${path}man"
done

# Include other Troff implementations in search paths
for path in "/usr/local/heirloom-doctools" "$DWBHOME"; do 
	[ -d "$path/bin" ] && PATH="$path/bin:$PATH"
	[ -d "$path/man" ] && MANPATH="$MANPATH:$path/man"
done

# SmartOS: Include `/smartdc/*' directories in search paths
[ -d /smartdc ] && {
	PATH="$PATH:/smartdc/bin:/opt/smartdc/bin:/opt/smartdc/agents/bin"
	MANPATH="$MANPATH:/smartdc/man:/opt/smartdc/man"
}

# Tcl/Tk: Add versioned installation paths to $MANPATH
[ -d /usr/local/lib/tcl ] && {
	for path in /usr/local/lib/tcl/*/man; do
		[ -d "$path" ] && MANPATH="$MANPATH:$path"
	done
}

# Darwin: Scan Apple's Developer/SDK directories for manual-pages
case `uname` in [Dd]arwin)
	paths='
		/Applications/*/share/man
		/Developer/usr/share/man
		/Developer/usr/*/share/man
		/Developer/usr/X11/share/man
		/Library/Developer/CommandLineTools/usr/share/man
		/Library/Developer/CommandLineTools/SDKs/*/usr/share/man
		/System/Library/Filesystems/*/Contents/man
		/Developer/Platforms/*/Developer/usr/share/man
		/Developer/Platforms/*/Developer/usr/*/share/man
		/Developer/Platforms/*/Developer/SDKs/*/usr/share/man
	'; for path in $paths; do
		[ -d "$path" ] && MANPATH="$MANPATH:$path"
	done
	
	# macOS's man(1) gets confused if $MANPATH starts with a colon
	MANPATH=${MANPATH#:}
;; esac 

export PATH MANPATH
unset path paths
