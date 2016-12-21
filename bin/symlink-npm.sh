#!/bin/sh

modules=(accordion atom-mocha basic-pagination \
	domtokenlist fix-ie get-options nag-window \
	ppjson print record-jar)

for i in "${modules[@]}"; do
	target="node_modules/$i"
	labs="$HOME/Labs"
	
	case $i in
		accordion)        path="$labs/Accordion";;
		atom-mocha)       path="$labs/Atom-Mocha";;
		basic-pagination) path="$labs/Pagination";;
		domtokenlist)     path="$labs/DOMTokenList";;
		fix-ie)           path="$labs/Fix-IE";;
		get-options)      path="$labs/GetOptions";;
		nag-window)       path="$labs/Nag";;
		ppjson)           path="$labs/PPJSON";;
		print)            path="$labs/Print";;
		record-jar)       path="$labs/RecordJar";;
		*)                path=;;
	esac
	
	[ -d "$path" ] && [ -d "$target" ] && [ ! -L "$target" ] && {
		rm -rf "$target"
		ln -s "$path" "$target"
		echo "Linked: $i -> $path"
	}
done;

true;
