#!/bin/sh

set +o posix

while getopts v option; do
	case $option in
		v) verbose=1;; # Enable verbose logging
	esac
done


# Nab current version number of File-Icons package
cd "$FILE_ICONS"
tag=$(git tag | tail -n1)


# Extract downloaded IcoMoon folder and update files
cd ~/Downloads
name=file-icons
zipfile=$name.zip

# Check we've got the right folder
[ -f $zipfile ] && {
	
	# Extract zip folder
	[ $verbose ] && echo "Extracting $name.zip"
	unzip -qo -d $name $zipfile || exit;
	
	# Copy the relevant files to the package folder
	[ $verbose ] && echo "Updating files in $FILE_ICONS/"
	mv $name/selection.json "$FILE_ICONS/selection.json"
	mv $name/fonts/file-icons.woff2 "$FILE_ICONS/resources/fonts/file-icons-$tag.woff2"
	
	# Update the installed version of the font if there is one
	installed_font=~/Library/Fonts/$name.zip
	[ -f $installed_font ] && {
		[ $verbose ] && echo "Updating $installed_font"
		mv $name/fonts/file-icons.ttf $installed_font
	}
	
	# Clean up
	[ $verbose ] && echo "Cleaning up files"
	rm -rf $name $zipfile
	[ $verbose ] && echo "Done!"
	exit 0;
	
} || {
	>&2 echo 'ERROR: Could not locate "'$zipfile'" in ~/Downloads - wrong filename?';
	exit 1;
}
