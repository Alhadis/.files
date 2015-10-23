#!/bin/sh

IFS=

cd ~/Music/iTunes

files=('iTunes Library Extras.itdb' 'iTunes Library Genius.itdb' 'iTunes Library.itl' 'iTunes Music Library.xml')
tmpdir='MacBook iTunes Library Backup II'

mkdir $tmpdir
for i in ${files[@]}; do
	cp $i $tmpdir/$i
done;


zip -r "$tmpdir.zip" $tmpdir -x "*.DS_Store"
mv $tmpdir.zip ~/Dropbox/Backups/
rm -rf $tmpdir


# Upload with Dropbox
open /Applications/Dropbox.app
