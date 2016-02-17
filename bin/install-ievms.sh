#!/usr/bin/env bash

#
# Run an automated installation of an MSIE App Compat VM in VirtualBox, then
# perform some additional configuration.
#
# Full credit goes to xdissent for the deployment procedure.
# See: http://xdissent.github.com/ievms
#
# Usage: install-ievms.sh [version-number]
#


# Import some useful functions
source $(realpath $(dirname "$0")/../functions.sh)


# Generate a temporary directory
TEMP=$(mktemp -d /tmp/$(basename "$0").XXXXXX) || {
	echo >&2 "Could not create temporary directory";
	exit 2;
};

BEFORE=$TEMP/before
AFTER=$TEMP/after


# Store a list of names for each existing VM
VBoxManage list vms > $BEFORE


# Download and run the IEVM installer
marker "Installing IEVMs"
SCRIPT_URL=https://raw.githubusercontent.com/xdissent/ievms/master/ievms.sh
curl -s $SCRIPT_URL | IEVMS_VERSIONS="$*" bash



# Set internal field separator to a newline.
# Prevents spaces screwing with argument handling
IFS=$'\n'


# Share a folder (relative to $HOME) with a virtual machine.
# Usage: share folder-path vm-name [readonly]
share(){
	VBoxManage sharedfolder add "$2" \
		--name $(basename "$1") \
		--automount \
		--hostpath "$HOME/$1" \
		${3+--readonly};
}


# Configure one or more VMs by name
config(){
	local format="\x1B[38;5;2m"
	local clear="\x1b[0m"
	for vm in $*; do
		echo -e "Configuring: ${format}${vm}${clear}";
		VBoxManage modifyvm "$vm" \
			--clipboard bidirectional  \
			--draganddrop hosttoguest  \
			--audio none;
		VBoxManage sharedfolder remove "$vm" --name ievms 2>/dev/null
		
		# Configure shared folders
		share Jobs "$vm" readonly
		share Labs "$vm" readonly
		share VirtualBox/Shared "$vm"
	done
}


# Configure each new virtual machine
marker "Configuring new VMs"
VBoxManage list vms > $AFTER
config $(comm -3 $BEFORE $AFTER | sed -r 's/^\t*"(.+)".+/\1/g')
rm -rf $TEMP;
echo "Done!";
