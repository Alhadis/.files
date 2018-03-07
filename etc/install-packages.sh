#!/bin/sh

#
# --* install-package.sh *--
# Wrapper for cross-platform package installation. Used by ~/.files/install.sh,
# but should be suitable for general-purpose use (though I don't recommend it).
#
# Tested platforms:
# - Ubuntu: 17.10
#
usage="${0##*/} [-n|--noexec|--dryrun] [-h|--help] [...packages]"

cmd= # String containing resolved package-manager command
die= # String prefixing `die` message
dry= # Set by `-n` switch


# Terminate running script with an error message
die(){
	printf "${0##*/}: ${die}%s\n" "$@"
	exit 1
}

# Determine if a program exists in the user's $PATH
have(){
	while [ $# -gt 0 ]; do
		command -v "$1" 2>&1 >/dev/null || return 1
		shift
	done
}

# Print a command and execute it (unless `-n` is set)
run(){
	printf '%s\n' "$*"
	[ -n "$dry" ] || eval "$*"
}

# Wrapper to call `run` with resolved "$cmd"
get(){
	run "`printf "${cmd}\n" "$*"`"
}

# macOS: Install Homebrew
get_brew(){
	ruby='/usr/bin/ruby'

	# Basic sanity checks; better safe than sorry
	have curl      || die 'Unable to install Homebrew: curl not found'
	[ -x "$ruby" ] || die "Unable to install Homebrew: $ruby not executable"

	# Ensure script hasn't been moved
	url='https://raw.githubusercontent.com/Homebrew/install/master/install'
	(curl -qI 2>&1 "$url") >/dev/null || die \
		"Homebrew install script moved!" \
		"Please update this script with the correct URL."

	run $ruby -e "\"\`curl -fsSL $url\`\""
	unset die ruby url
}


# Parse command-line switches
while [ -n "$1" ]; do case $1 in

	# Print a brief usage summary and exit
	-h|--help|-\?)
		printf 'Usage: %s\n' "$usage"
		exit ;;

	# Dry-run: Print command that would normally be executed
	-n|--noexec | \
	-d|--dryrun )
		dry=1 ;;

	# Double-dash: Terminate option parsing
	--)
		shift
		break ;;

	# Invalid option: abort
	--*|-?*)
		>&2 printf '${0##*/}: Invalid option: "%s"\n' "$1"
		>&2 printf 'Usage: %s\n' "$usage"
		exit 1 ;;

	# Argument not prefixed with a dash
	*) break ;;

esac; shift
done

# Bail early if no package names were specified
[ "$1" ] || {
	printf 'Usage: %s\n' "$usage"
	exit 1
}


# Resolve command for installing packages
case `uname -s` in

	# macOS
	[Dd]arwin)
		have brew || get_brew
		cmd='brew install %s'
		;;

	# OpenBSD/FreeBSD
	*BSD)
		if   have pkg_add; then cmd='pkg_add %s'
		elif have pkg;     then cmd='pkg install %s'
		fi ;;

	# Linux-based
	[Ll]inux)
		if   have apt-get; then cmd='sudo apt-get install --yes %s'
		elif have yum;     then cmd='sudo yum -y install %s'
		elif have dnf;     then cmd='sudo dnf install --assumeyes %s'
		elif have pacman;  then cmd='pacman -S %s'
		elif have zypper;  then cmd='zypper install %s'
		elif have urpmi;   then cmd='urpmi %s'
		elif have nix-env; then cmd='nix-env -i %s'
		elif have tazpkg;  then cmd='tazpkg get-install %s'
		elif have pkg;     then cmd='pkg install %s'
		fi ;;

	# Solaris
	SunOS|Solaris)
		if   have pkgutil; then cmd='pkgutil -i %s'
		elif have pkg;     then cmd='pkg install %s'
		fi ;;
esac

# Bail if we couldn't resolve the package manager's name/arguments
[ "$cmd" ] || die "No package manager found or recognised for `uname -s`"

# Otherwise, go for it
while [ -n "$1" ]; do case $1 in

	# Emacs: Always install without GUI
	emacs)
		# Debian
		if have apt-get; then
			get emacs-nox
		else
			get emacs
		fi ;;

	# Node.js: Install non-outdated versions with additional build-tools
	node|nodejs)
		latest_version=9

		# Debian
		if have apt-get; then
			run "curl -sL 'https://deb.nodesource.com/setup_${latest_version}.x' | sudo -E bash -"
			get build-essential nodejs

		# Fedora/RPM-ish
		elif have yum || have dnf; then
			run "curl --silent --location https://rpm.nodesource.com/setup_${latest_version}.x | sudo bash -"
			get gcc-c++ make nodejs

		# Anything else
		else
			get node
		fi;;

	# Everything else
	*) get $1 ;;

esac; shift
done
