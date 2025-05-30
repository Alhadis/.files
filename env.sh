# Configure $PATH
unset PATH
paths='
	~/.files/var/bin
	~/.files/bin
	~/.cargo/bin
	~/.jsvu
	~/bin
	/usr/local/sbin
	/usr/local/bin
	/usr/sbin
	/usr/bin
	/sbin
	/bin
	/usr/X11R6/bin
	/opt/local/sbin
	/opt/local/bin
	/opt/tools/sbin
	/opt/tools/bin
	/usr/games
	~/.wasmtime/bin
	~/.deno/bin
	~/.local/bin
	~/perl5/bin
	~/go/bin
	~/Forks/depot_tools
'; for path in $paths; do
	case $path in \~/*) path=~/${path#\~/};; esac
	[ -d "$path" ] && PATH="${PATH#:}:$path";
done

# Define manual search paths
unset MANPATH INFOPATH
paths='
	~/.files/var
	~/.files/share
	~/.cargo/share
	/usr/local/share
	/usr/local
	/usr/share
	/usr/X11R6
	/usr/X11
	/opt/X11/share
	/opt/local
	/opt/local/share
	/usr/local/lib/node_modules/npm
	/opt/tools
	~/perl5
	~/Forks/depot_tools
'; for path in $paths; do
	case $path in \~/*) path=~/${path#\~/};; esac
	[ -d "$path/man"  ] && MANPATH="${MANPATH#:}:$path/man"
	[ -d "$path/info" ] && INFOPATH="${INFOPATH#:}:$path/info"
done

export DICPATH=~/.files/share/dict
export DWBHOME=/usr/local/dwb
export GPG_TTY=`tty`
export GREP_COLORS="sl=38;5;240:mt=1;38;5;10;48;5;22:fn=38;5;242:se=38;5;237:ln=38;5;10"
export GROFF_PLATFORM_NAME=`uname -s`
export GROFF_TYPESETTER='utf8'
export GROFF_TMAC_PATH=~/.files/var/tmac:~/Labs/Mono:/usr/local/share/groff/site-tmac
export GROFF_FONT_PATH=/usr/local/share/groff/site-font
export GS_OPTIONS='-dNOSAFER -dNOSHORTERRORS -dOSTACKPRINT -dESTACKPRINT -I.'
export HISTFILE=
export LESS=-fRS
export LYNX_CFG=~/.files/etc/lynx.cfg
export TROFFONTS=~/Fonts:~/Library/Fonts:$GROFF_FONT_PATH
export WORDLIST=~/.files/share/dict/custom.txt
export WASMTIME_HOME=~/.wasmtime



# Kludge to simplify GUI detection on macOS without XQuartz installed
[ "$DISPLAY" ] || case `uname` in [Dd]arwin) export DISPLAY=1 ;; esac

# Perl/CPAN modules path
have perl && [ -d ~/perl5 ] && {
	export PERL5LIB=~/perl5/lib/perl5:$PERL5LIB
	export PERL_LOCAL_LIB_ROOT=~/perl5:$PERL_LOCAL_LIB_ROOT
	export PERL_MB_OPT='--install_base "~/perl5"'
	export PERL_MM_OPT="INSTALL_BASE=~/perl5"
}

# Prefer Emacs for terminal-based editing
EDITOR=`command -v emacs 2>/dev/null`
[ -x "$EDITOR" ] && {
	EDITOR="NO_PKG=1 $EDITOR -nw"
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
	
	# Silence REPL session logging
	export NODE_REPL_HISTORY=/dev/null
	
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
	INFOPATH=~/.nodebrew/current/share/info:"$INFOPATH"
}

# Homebrew: Configure brew(1) for both macOS and Linux
have brew && {
	# Ensure all recently-updated formulae are reported by `brew update`
	export HOMEBREW_UPDATE_REPORT_ALL_FORMULAE=1
	
	export HOMEBREW_DEVELOPER=1           # Offer feedback more useful to a tap maintainer
	export HOMEBREW_NO_AUTO_UPDATE=1      # Don't auto-update when installing stuff
	export HOMEBREW_NO_INSTALL_CLEANUP=1  # Don't bother cleaning up downloaded files
	export HOMEBREW_NO_INSTALL_FROM_API=1 # Report outdated and recently-added formulae
	export HOMEBREW_FAIL_LOG_LINES=30     # Double the number of lines shown from error-logs
	have bat && export HOMEBREW_BAT=1     # Highlight formulae source using bat(1)
}

# Yarn: Paths added to .bashrc upon installation
for path in ~/.yarn/ ~/.config/yarn/global/node_modules/.; do
	[ -d "${path}bin" ] && PATH="$PATH:${path}bin"
	[ -d "${path}man" ] && MANPATH="$MANPATH:${path}man"
done

# JQ: Force readable indentation and configure colour palette
have jq && {
	export JQ_COLORS='1;39:0;36:0;36:0;33:0;32:2;37:2;37'
	alias jq='jq --tab'
}

# PowerShell: Disable telemetry and suppress copyright banner
have pwsh && {
	export POWERSHELL_TELEMETRY_OPTOUT=1
	alias pwsh='pwsh -NoLogo'
}

# RubyGems: Include latest version's executables in search path
[ -d /usr/local/lib/ruby/gems ] && {
	path=`printf '%s\n' /usr/local/lib/ruby/gems/* | sort --version-sort | tail -n1`
	[ -d "$path/bin" ] && PATH="$path/bin:$PATH"
	for path in "$path" "$path/doc" "$path"/gems/*; do
		[ -d "$path/man"  ] && MANPATH="$MANPATH:$path/man"
		[ -d "$path/info" ] && INFOPATH="$INFOPATH:$path/info"
	done
}

# Include other Troff implementations in search paths
for path in "/usr/local/heirloom-doctools" "$DWBHOME"; do 
	[ -d "$path/bin"  ] && PATH="$path/bin:$PATH"
	[ -d "$path/man"  ] && MANPATH="$MANPATH:$path/man"
	[ -d "$path/info" ] && INFOPATH="$INFOPATH:$path/info"
done

# SmartOS: Include `/smartdc/*' directories in search paths
[ -d /smartdc ] && {
	PATH="$PATH:/smartdc/bin:/opt/smartdc/bin:/opt/smartdc/agents/bin"
	MANPATH="$MANPATH:/smartdc/man:/opt/smartdc/man"
	INFOPATH="$INFOPATH:/smartdc/info:/opt/smartdc/info"
}

# Tcl/Tk: Add versioned installation paths to $MANPATH
[ -d /usr/local/lib/tcl ] && {
	for path in /usr/local/lib/tcl/*; do
		[ -d "$path/man"  ] && MANPATH="$MANPATH:$path/man"
		[ -d "$path/info" ] && INFOPATH="$INFOPATH:$path/info"
	done
}

# macOS-specific
case `uname` in [Dd]arwin)
	# Include “keg-only” Homebrew formulae in search paths
	for path in curl file libarchive icu4c ruby sphinx-doc sqlite tcl-tk texinfo; do
		path="/usr/local/opt/$path"
		[ -d "$path/bin"        ] && PATH="$path/bin:$PATH"
		[ -d "$path/sbin"       ] && PATH="$path/sbin:$PATH"
		[ -d "$path/share/man"  ] && MANPATH="$path/share/man:$MANPATH"
		[ -d "$path/share/info" ] && INFOPATH="$path/share/info:$INFOPATH"
	done
	
	# Scan Apple's Developer/SDK directories for manual-pages
	paths='
		/Applications/*/share
		/Applications/*/Contents/Resources
		/Developer/usr/share
		/Developer/usr/*/share
		/Developer/usr/X11/share
		/Library/Developer/CommandLineTools/usr/share
		/Library/Developer/CommandLineTools/SDKs/*/usr/share
		/System/Library/Filesystems/*/Contents
		/Developer/Platforms/*/Developer/usr/share
		/Developer/Platforms/*/Developer/usr/*/share
		/Developer/Platforms/*/Developer/SDKs/*/usr/share
	'; for path in $paths; do
		[ -d "$path/man"  ] && MANPATH="$MANPATH:$path/man"
		[ -d "$path/info" ] && INFOPATH="$INFOPATH:$path/info"
	done
	
	# Include non-brewed pip(1)-installed packages in search paths
	for path in `printf '%s\n' ~/Library/Python/* 2>/dev/null | sort -rV`; do
		[ -d "$path/bin"  ] && PATH="$PATH:$path/bin"
		[ -d "$path/man"  ] && MANPATH="$MANPATH:$path/man"
		[ -d "$path/info" ] && INFOPATH="$INFOPATH:$path/info"
	done
	
	# macOS's man(1) gets confused if $MANPATH starts with a colon
	MANPATH=${MANPATH#:}
	
	# Identify terminal emulators that don't define $TERM_PROGRAM
	[ "$TERM_PROGRAM" ] || case $__CFBundleIdentifier in
		*.contour-terminal.*|*.contour)
			export TERM_PROGRAM=${TERMINAL_NAME_:-${TERM:-contour}}
			export TERM_PROGRAM_VERSION=$TERMINAL_VERSION_STRING
		;;
		*cool-retro-term) export TERM_PROGRAM='cool-retro-term' ;;
		io.alacritty)     export TERM_PROGRAM='Alacritty' ;;
	esac
;; esac 

# Search for manual pages in nearby directories
MANPATH="$MANPATH:./man:./node_modules/.man"

# Append info(1)'s default search-paths to $INFOPATH
INFOPATH="${INFOPATH%:}:"

export PATH MANPATH INFOPATH
unset path paths
