Random, Apple-specific weird shit
=================================


Curiosities
-----------

	/usr/share/misc/birthtoken
	/usr/share/sandbox/*.sb
	/opt/X11/bin/xkeystone



Resource fork access
--------------------

…is achieved by affixing `/..namedfork/rsrc` to the subject's filename.

	RezWack -d /dev/null -r Palatino/..namedfork/rsrc -o Palatino.dfont
	cp icon.rsrc file.txt/..namedfork/rsrc && SetFile -a C file.txt

As explained by RezWack(1), this only works on HFS or HFS+ filesystems.



Location of system dictionaries
-------------------------------

	/System/Library/AssetsV2/com_apple_MobileAsset_DictionaryServices_dictionaryOSX/*.asset/AssetData/*.dictionary/Contents/Info.plist

Source: https://apple.stackexchange.com/a/404468

Convert them to usable formats using [`pyglossary`](https://github.com/ilius/pyglossary):

~~~console
λ pip3 install pyglossary
λ pyglossary /path/to/*.dictionary/ converted.tsv
λ pyglossary --help # To display other formats to convert to
~~~



Location of downloaded podcasts
-------------------------------

	~/Library/Group\ Containers/*.groups.com.apple.podcasts/Library/Cache/*.mp3
	~/Library/Group\ Containers/*.groups.com.apple.podcasts/Library/Cache/IMImageStore-Default/*



Xcode-related stuff
-------------------

Project builds:

	~/Library/Developer/Xcode/DerivedData/*/Build/Products/Development/

Keybinding configs and syntax highlighting themes:

	~/Library/Developer/Xcode/UserData/KeyBindings/*.idekeybindings
	~/Library/Developer/Xcode/UserData/FontAndColorThemes/*.xccolortheme



Location of notes and reminders
-------------------------------

	~/Library/Group Containers/group.com.apple.notes/NoteStore.sqlite
	~/Library/Reminders/Container_v1/Stores/*.sqlite

Decode `NotesStore.sqlite` with [`apple_cloud_notes_parser`](https://github.com/threeplanetssoftware/apple_cloud_notes_parser).



Apple's transition from man-1.6 to mandoc
-----------------------------------------

	https://github.com/apple-oss-distributions/man/compare/d1a6cf2..34c45d6

~~~console
$ cd ~/Forks/Apple-Man && git show d1a6cf2...34c45d6
~~~



Exclude specific apps from Spotlight search results
---------------------------------------------------

1.	Open _“System Preferences”_ → _“Spotlight”_ pane → _“Privacy”_ tab.
2.	Drag-and drop the *specific* `.app` bundle onto the locations list.

The selection dialogue produced by the pane's _“Add (+)”_ button will *not*
allow you to select an Application bundle… or anything Finder pretends isn't
a directory. This procedure must be performed manually as no facility exists
with which exclusions can be programmatically added, queried, or removed.

Sources:
* https://apple.stackexchange.com/a/191049
* https://apple.stackexchange.com/a/428081
