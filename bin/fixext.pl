#!/usr/bin/env perl
use strict;
use warnings;
use autodie;
use feature "say";
use File::Copy "mv";

my $ext = shift;

for my $file (@ARGV) {
	if($file =~ m/^(.*?)\.($ext)\.(\d+)$/i){
		my ($base, $extstr, $index) = ($1, $2, $3);
		my $rename = "$base.$index.$extstr";
		while(-f $rename){
			$index++;
			$rename = "$base.$index.$extstr";
		}
		
		say "Renamed: $file -> $rename";
		mv($file, $rename);
	}
}
