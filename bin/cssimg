#!/bin/sh

#
# Print the width (in ems) of an image proportional to its height.
# Useful for setting the measurements of replaced images in CSS.
#
# Usage:
#    $ cssimg /path/to/img.png
#    1.5em
#

length=3
while getopts il: option; do
	case $option in
	i)  invert=1;;          # Use height instead of width
	l)  length=$OPTARG;;    # Set number of digits after decimal
	esac;
done;
shift $((OPTIND - 1))


image="$*";

# Bail if no argument was given
[[ -z $image ]] && {
	>&2 echo 'No image specified';
	exit 1;
};


# Grep this bitch.
size=$(file $image | grep -oE '[0-9]+ x [0-9]+');


# We weren't given a valid image
[[ $? -ne 0 ]] && {
	>&2 echo 'Not a valid image file';
	exit 2;
};


# Extract the image's measurements
IFS='x ' read -a array <<< "$size";
width=${array[0]};
height=${array[1]};


# Base the measurement on width, unless the -i flag was passed
divide=$width;
by=$height;
[[ $invert ]] && { divide=$height; by=$width; }


# Calculate the result
result=$(printf "scale=$length;$divide / $by\n" | bc);


# Remove superfluous zeroes
result=$(printf "$result" | perl -pe 's/\.0+$|(\.\d*?)0+$/$1/g')


# Then copy it to the clipboard, stripping unneeded zeroes
printf "$result"em | pbcopy;


# Send a copy of it to STDOUT for the user
pbpaste;
printf '\n';
