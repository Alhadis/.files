#!/bin/sh

# Locate /devps
[ ! "$devps" ] && {
	devps=$(find $(realpath $(brew --prefix groff)) -type d -name devps | head -n1);
	echo $devps;
	echo $?;
	
	[ "$?" != 0 ] && {
		>&2 echo "ERROR: Unable to locate devps in Homebrew-installed Groff";
		exit 2;
	}
};

# /devps isn't a directory
[ ! -d "$devps" ] && {
	>&2 echo "Designated \$devps isn't a directory: $devps";
	exit 2;
};


# Copy specified font to /devps
cp "$1" "$devps";
cd "$devps"

noext=$(echo "$1" | sed -r 's/\.\w+$//')

# Convert to AFM/PFA formats
fontforge -nosplash -lang=ff -c '
	fonts = FontsInFile($1);
	count = SizeOf(fonts);

	if(count != 0)
		i = 0;
		while(i < count)
			Open($1 + "(" + i + ")");
			Generate(fonts[i] + ".pfa");
			Close();
			i++;
		endloop
	else
		Open($1);
		Generate($1:r + ".pfa");
	endif
' "$1" 2>/dev/null;

name=$(echo "$noext" | tr "[a-z]" "[A-Z]" | sed -r "s/[^A-Z]//g");

# Create Groff font
afmtodit -d DESC -e text.enc "$noext.afm" generate/textmap $name

# Grab internal PostScript name
perl -ne 'print "$1" if /internalname\s+(\S+)/' $name >> download
printf "\t\t$noext.pfa\n" >> download
