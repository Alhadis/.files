#!/bin/sh

name=${1:-exploring-es6}
dl=$HOME/Downloads
dest=$HOME/Dropbox/Backups

# Ascertain where to move the files
ebooks=$HOME/Documents/eBooks/
case $name in
	exploring-es6)  ebooks+="Exploring ES6"  ;;
	setting-up-es6) ebooks+="Setting up ES6" ;;
esac

# Move eBooks from downloads directory to script's directory
[ -f $dl/$name.pdf ] && {
	
	# Bail if I've forgotten to unlock the files in Finder (again)
	[ ! -w "$ebooks/$name.pdf" ] && {
		>&2 echo "File not writable: $ebooks/$name.pdf";
		>&2 echo "Operation aborted. Unlock files and try again.";
		exit 3;
	};
	
	# We're all good here: proceed
	files=$dl/$name.*
	xattr -d com.apple.quarantine $files 2>/dev/null
	xattr -d com.apple.metadata:kMDItemWhereFroms $files 2>/dev/null
	mv -fv $files "$ebooks/"
} || {
	>&2 printf "File not found: $dl/$name.pdf";
	exit 3;
};


# Hard-link to Dropbox folder
cd "$ebooks"
[ $(stat -f %l $pdf) -eq 1 ] && {
	ln -f $name.pdf $dest/$name.pdf;
};

# Upload with Dropbox
open /Applications/Dropbox.app
