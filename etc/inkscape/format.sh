#!/bin/sh
set -e

cd "${0%/*}"

prefs=preferences.xml
src=`xmllint --format $prefs | perl -pXe 's|^(  )+|"\t"x(length($&)/2)|ge;'`
printf '%s\n' "$src" > $prefs
