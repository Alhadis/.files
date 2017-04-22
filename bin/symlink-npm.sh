#!/bin/sh

modules=(alhadis.utils atom-fs atom-mocha get-options print)

for i in "${modules[@]}"; do
	target="node_modules/$i"
	labs="$HOME/Labs"
	
	case $i in
		alhadis.utils)    path="$labs/Utils";;
		atom-fs)          path="$labs/Atom-FS";;
		atom-mocha)       path="$labs/Atom-Mocha";;
		get-options)      path="$labs/GetOptions";;
		print)            path="$labs/Print";;
		*)                path=;;
	esac
	
	[ -d "$path" ] && [ -d "$target" ] && [ ! -L "$target" ] && {
		rm -rf "$target"
		ln -s "$path" "$target"
		echo "Linked: $i -> $path"
	}
done;

true;
