#!/usr/bin/env bash

#
# Run an automated installation of an MSIE App Compat VM in VirtualBox, then
# perform some additional configuration.
#
# Full credit goes to xdissent for the deployment procedure.
# See: http://xdissent.github.com/ievms
#
# Usage: install-ievms [version-number]
#


# Generate a temporary directory
TEMP=$(mktemp -d /tmp/$(basename "$0").XXXXXX) || {
	echo >&2 "Could not create temporary directory";
	exit 2;
};

BEFORE=$TEMP/before
AFTER=$TEMP/after


# Store a list of names for each existing VM
VBoxManage list vms > $BEFORE


# Run the IEVM installer
echo "==> Installing IEVMs"
IEVMS_VERSIONS=$(echo "$*" | sed s/[^[:digit:][:blank:]]//g) ievms



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
		share Labs "$vm" readonly
		share VirtualBox/Shared "$vm"
	done
}


# Configure each new virtual machine
echo "Configuring new VMs"
VBoxManage list vms > $AFTER
config $(comm -3 $BEFORE $AFTER | sed -r 's/^\t*"(.+)".+/\1/g')
rm -rf $TEMP;
echo "Done!";