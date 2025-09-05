#!/bin/sh
set -e
cd "${0%/*}"

# Update English dictionaries from upstream
[ "$1" = '-q' ] || for ext in aff dic; do
	url="https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_AU.$ext"
	curl -I "$url" > /dev/null
	curl -# "$url" > en_AU.$ext
done

# Keep custom dictionaries synced with ours
custom=`cat custom.txt`; before="$custom"
for path in \
	~/"Library/Application Support/Firefox/Profiles"/*/persdict.dat \
	~/"Library/Application Support/Google/Chrome/Custom Dictionary.txt" \
	~/"Library/Application Support/Google/Chrome/Default/Custom Dictionary.txt" \
	~/"Library/Spelling/LocalDictionary" \
	~/"Library/Spelling/en_AU" \
	~/".config/google-chrome/Default/Custom Dictionary.txt" \
	~/".config/chromium/Default/Custom Dictionary.txt";
do if [ -f "$path" ]; then custom="${custom}`printf '\n'; cat "${path}"`"; fi; done

custom=`printf '%s\n' "$custom" | sort | grep . | grep -v '^checksum' | uniq`
[ "$custom" = "$before" ] || printf '%s\n' "$custom" > custom.txt
