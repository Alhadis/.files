#!/bin/sh

IFS=

cd ~/Music/iTunes

files=('iTunes Library Extras.itdb' 'iTunes Library Genius.itdb' 'iTunes Library.itl' 'iTunes Music Library.xml')
backup='MacBook iTunes Library Backup II'
dropbox=~/Dropbox/Backups/

mkdir $backup
for i in ${files[@]}; do
	cp $i $backup/$i
done;


zip -r "$backup.zip" $backup -x "*.DS_Store"
mv $backup.zip $dropbox
rm -rf $backup


# Upload with Dropbox
open /Applications/Dropbox.app


# Copy to portable hard drive if it's plugged in
[ -d /Volumes/"Amra Helion" ] && {
	cp "$dropbox/$backup.zip" "/Volumes/Amra Helion/MacBook/";
};
