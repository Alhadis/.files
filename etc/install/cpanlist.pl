#!/usr/bin/env perl
use strict;
use warnings;
use autodie;
use v5.14;
use utf8;
$| = 1;

use warnings  qw< FATAL utf8 >;
use open      qw< :std :utf8 >;
use Carp      qw< confess >;
use Module::CoreList;

END { close STDOUT }
local $SIG{__DIE__} = sub {
	confess "Uncaught exception: @_" unless $^S;
};

# Inform user that lengthy execution is natural and expected.
print STDERR "Collating package data. Please be patient.\n";


# Determine which CORE modules shipped with this Perl version.
my %coreModules   = ();
my $perlVersion   = version->new($^V)->numify;
my %modsByVersion = %{Module::CoreList->find_version($perlVersion)};
while ((my $key) = each %modsByVersion) {
	$coreModules{$key} = 1 if $key;
};


# Generate a filtered autobundle for the currently-running version of Perl
my @allModules = ();
my $bundleData = `cpan -a 2>/dev/null`;
if($bundleData =~ m~\nWrote bundle file\n\h+(/.+\.pm)\s*\z~gmi){
	my $bundlePath = $1;
	open(my $fh, "+< :encoding(UTF-8)", "$bundlePath");
	while(<$fh>){
		if ((/^=head1 +CONTENTS\h*$/m .. m/^=head1 +CONFIGURATION\h*$/mg)
		and ($_ !~ m/^\h*$|^=head1 +[A-Z]+/ms)){
			$_ =~ s/\s+(?:\S+\s*)?$//g;
			push @allModules, $_ unless defined $coreModules{$_};
		}
	}
	close($fh);
	unlink $bundlePath;
}


# Write resultant list to standard output
print join "\n", sort @allModules;
print "\n";


__END__

#=======================================================================

=head1 NAME

C<filename> - print CPAN modules installed by user


=head1 SYNOPSIS

B<C<filename>> E<gt> C<module-list.txt>


=head1 DESCRIPTION

Generates a list of CPAN modules which the user chose to install, as
well as any dependencies the installed modules required. The output
is an alphabetised, line-delimited list of qualified module-names:

	Algorithm::Diff
	Archive::Zip
	...
	Date::Parse
	Digest::SHA1
	ExtUtils::MakeMaker::version::vpp
	Image::ExifTool
	...
	YAML::Types
	inc::latest
	lib::core::only
	local::lib

This performs a task similar to C<cpan -a>, except the generated lists
exclude core modules and builtins. Since the output is plain-text, one
may reinstall their library without an intermediate C<install::Bundle>
call:

	# Save list of CPAN modules
	$ C<filename> > list.txt

	# Reinstall everything later on
	$ xargs < list.txt cpan -i
	$ cpan -i $(cat list.txt)


=head1 NOTES

This module is neither exciting, nor original. It does what autobundling
does, albeit cleaner and simpler. C< CPAN::Shell-E<gt>autobundle > emits
I<every> module visible in one's C<@INC> paths, making it more difficult
to recollect which modules were user-installed, and which are defaults.


=head1 TODO

Stuff I haven't done, and probably never will:

=over
=item *
Publish this garbage to CPAN?
=item *
If we do, add an option to include version number of each package
=item *
Generate output faster. Preferably without relying on L<CPAN::Shell> to
take care of all the heavy lifting for us.
=back


=head1 SEE ALSO

The L<PerlMonks thread|http://www.perlmonks.org/?node_id=909966> which
convinced me there was no simple way to generate a "clean" module list
using L<CPAN::Shell>.


=head1 AUTHOR

John Gardner <gardnerjohng@gmail.com>
https://github.com/Alhadis

=cut
