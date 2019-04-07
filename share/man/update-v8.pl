#!/usr/bin/env perl
use strict;
use warnings;
use autodie;
use v5.14;
use utf8;
$| = 1;

# Generate Roff from extracted V8 options
sub parseOpts {
	my $output = "";
	my %opts = ($_ = $1) =~ m/
		^ \h* --([-\w]+)
		(
			(?:\s+[^\n]+)?
			\h* \n
			\h+(?!--)\S+[^\n]+
		) \n
	/xsmg;
	
	# Generate a RegExp to match a switch reference
	my $matchKeys = join "|", sort {
		0 == index($b, $a) ? 1 : (0 == index($a, $b)) ? -1 : ($a cmp $b)
	} keys %opts;
	$matchKeys =~ s/[-_]/[-_]/g;
	$matchKeys = "--(?:no[-_])?(?:$matchKeys)";


	my $matchURL = qr~ \b
		(?:https?|s?ftp|ftps|file|smb|git|ssh|rsync|afp|nfs|(?:x-)?man(?:-page)?|gopher|txmt|issue|atom)
		://(?:(?!\#\w*\#)(?:[-:\@\w.,\~\%+_/?=\&\#;|!]))+
		(?<![-.,?:\#;])
		|mailto:(?:(?!\#\w*\#)(?: [-:\@\w.,\~\%+_/?=\&\#;|!]))+(?<![-.,?:\#;])
	~aix;

	for my $key (sort keys %opts) {
		$_ = $opts{$key};
		next if m/^\(deprecated/i or $key =~ m/^(help|testing-bool-flag)$/i;
		
		
		# Strip brackets enclosing description
		s/^\s*\(\s*//;
		s/\)(?!\Z)(?=\n)//;
		
		# Capitalise first word of each sentence
		s/^\s*(\w)/\U$1/;
		
		# Break sentences across lines
		s/(?<!\be\.g|\bi\.e)\.\s+/.\n/gi;
		
		# Replace backslashes with \e sequence
		s/\\/\\e/g;
		
		# Extract metadata used by .V8 macro
		my $flag = "";
		$flag = " (WIP)"       if ($_ =~ s/\h*\(in progress\)\.?\h*$/./m);
		$flag = " (INTERNAL)"  if ($_ =~ s/\h*\(for internal use only\)\.?\h*$/./m);
		$flag = " (TEST)"      if ($_ =~ s/\h* - testing only(?=\)?\h*$)//m);


		# Typos and ad-hoc formatting fixes
		s/alingment/alignment/gi;
		s/^enable "harmony ([^"]+)"/Enable $1/gi;
		s/^(Max|Min) /${1}imum /gi;
		s/reducing it's size/reducing its size/;
		s/only smi values/only SMI values/g;

		# Format the switch's type and default value
		my $desc = $_;
		my $typeArgs = "";	
		my %attr = $_ =~ m/
			\h+ (type):    \h+ (\S.*?)
			\h+ (default): \h+ (\S+.*?)
			\h* \Z
		/x;
		
		if(defined($attr{"type"})){
			my $type = $attr{"type"};
			my $default = $attr{"default"};
			my $hideType = 0;
			
			# Boolean-type switch
			if("bool" eq $type){
				$hideType = 1;
				if($default eq "true"){
					$desc =~ s/^Shares(?=\h)/Share/i;
					
					# Try to negate sentence, but don't try TOO hard
					unless(
						$desc =~ s/^Enable/Disable/i  or
						$desc =~ s/^Include/Exclude/i or
						$desc =~ s/^(
							Abort|Automatically|Analy[zs]e|Cache|Compact|Elide|Expose|Filter|
							Free|Generate|Get|Inline|Intrinsify|Optimi[sz]e|Pretenure|Promote|Randomi[zs]e|
							Rehash|Run|Rewrite|Share|Split|Trace|Track|Trigger|Use|Validate|Write
						)(?=\h)/Don't \l$1/xi
					){
						my %replacements = (
							"concurrent-recompilation" => "Force synchronous optimisation of hot functions.",
							"fast-math" => "Don't enable faster, potentially less accurate, math functions.",
							"parallel-scavenge" => "Disable parallel scavenging.",
							"polymorphic-inlining" => "Disable polymorphic inlining.",
							"trace-maps-details" => "Don't log map details.",
							"turbo-allocation-folding" => "Disable TurboFan allocation folding.",
							"turbo-loop-peeling" => "Disable TurboFan loop peeling.",
							"turbo-loop-rotation" => "Disable TurboFan loop rotation.",
							"turbo-loop-variable" => "Disable TurboFan loop variable optimisation.",
							"use-verbose-printer" => "Disable verbose printing",
						);
						if(defined($replacements{$key})){
							$desc = $replacements{$key};
						}
						else{
							warn "Unable to negate sentence for --$key: $desc";
							exit 1;
						}
					}
					$key = "no-$key";
				}
			}
			
			# String-type switch (might be a null-pointer)
			elsif("string" eq $type){
				unless($default =~ s/^nullptr$/NULL/i){
					$default =~ s/\\/\\e/g;
					$default =~ s/ /\\~/g;
					$default = "\\(lq${default}\\(rq";
				}
			}
			
			unless($hideType){
				$typeArgs = $type =~ /_/ ? $type : ucfirst($type);
				$typeArgs = "| $typeArgs $default";
			}
		}
		$desc =~ s/\n\h*(?:type|default):.+\Z//;
		$desc =~ s/(?<!\.)\Z/./;
		
		# Stupid ad-hoc fixes which will need updating
		{
			no warnings qw<uninitialized>;
			my $punct = '([.,!?]\)|\)[.,!?]|[.,;:!?])?';
			$desc =~ s/ wasm( |\.(?:$|\h))/ WASM$1/g;
			$desc =~ s/\bmksnapshot\b/\\*(C!$&\\fP/g;
			$desc =~ s/^Turbofan /TurboFan /gm;
			$desc =~ s/\ADisable \Kglobal\.\Z/\n.JS global .\n/;
			$desc =~ s/\((\d+) \+ (heap_growing_percent)\/100\)\.?/\n.EQ\n( $1 + $2 \/ 100 ).\n.EN\n/;
			$desc =~ s/(?<=Disallow )eval\h+(?=and friends\.?$)/\n.`` eval\n/mi;
			$desc =~ s/(?<=Expose )(async_hooks|freeBuffer|gc|injected-script-source\.js)\h+/\n.`` $1\n/g;
			$desc =~ s/(?<=Enable )optimization for specific cpu/optimisation for a specific CPU/;
			$desc =~ s/(?<=Disable )(await) (?=taking 1 tick)/\n.JS $1\n/;
			$desc =~ s/(?<= )l(?=inux profiler)/L/;
			$desc =~ s/(?<=Dump )elf(?= objects)/ELF/;
			$desc =~ s/(?<= )h(?=armony )/H/;
			$desc =~ s/
				(StubName,NodeId|ll_prof|ASM_UNIMPLEMENTED_BREAK)\b
				$punct \h*
			/\n.`` $1 $2\n/gx;
			$desc =~ s/\b
				( Error\.stack
				| ArrayBuffer
				| RangeError
				| Promise
				| BigInt
				| WebAssembly\.compile
				| (?i:sharedarraybuffer)
				| (?:[A-Z]\w+)\.prototype (?:\.\w+)* (?:\.\{[^}]+\})?
				| Object\.fromEntries(?:\(\))?
				| import\.meta(?:\.\w+)*
				) $punct \h*
			/\n.JS $1 $2\n/gx;
			$desc =~ s/"(Intl\.\w+)"(\.?)/\n.JS $1 $2\n/g;
			$desc =~ s/sharedarraybuffer/SharedArrayBuffer/g;
			$desc =~ s/"(well-formed) (JSON\.stringify)"\./$1\n.JS $2 ./;
			$desc =~ s/"(RegExp Unicode sequence properties)"/$1/;
			$desc =~ s/\(0 means random\)\K\((with snapshots[^()]+)\)/.\n\u$1/;
			$desc =~ s/ <([a-z])> /\n.VAR \u$1\n/gi;
			$desc =~ s/ ([xX]) /\n.VAR \u$1\n/gi;
			$desc =~ s/ <([Nn])> times\b/\n.VAR \u$1\ntimes/gi;
			$desc =~ s/$matchKeys/\\*(C!$&\\fP/g;
			$desc =~ s/($matchURL)$punct/\n.LK "$1" $2\n/g;
			$desc =~ s/(?<=\h)(gc_interval|stress_compaction)(?!-)\b/\\*(C!$&\\fP/g;
			$desc =~ s/code-<pid>-<isolate id>(\.asm\.?)/\n.RI \\(lqcode- pid - isolate-id $1\\(rq/g;
			$desc =~ s/(?<=\h)(random)\(0,\h*([xX])\)\h*/\\*(CB$1\\fP\\*(CW(0,\\fP\n.VAR $2 )\n/g;
			$desc =~ s/\bC\+\+/\\*(C+/g;
			$desc =~ s/(?<=Disable namespace exports \()[^)\n]+(?=\))/"\\f(CW" . ($& =~ tr|'"|"'|r) . "\\fP"/e;
			$desc =~ s/(?:Can|Don|Won|Shouldn|Wouldn)\K'(?=t )/\\(cq/gi;
			$desc =~ s/(?<=\w)'(?=s )/\\(cq/g;
			$desc =~ s/(\n\.(?:``|JS).+)\n+/$1\n/g;
			$desc =~ s/\n+$//;
		}
		
		$output .= ".V8 ${key}${flag} $typeArgs\n";
		$output .= "$desc\n\n";
	}
	$output =~ s/\h+$//gm;
	$output =~ s/\n+$//;
	return $output;
}

$_ = `d8 --help`;
s/\A(\w+=\d+\s*)+\n//;
s/^\s*Synopsis:(.*?)\n(?=Options:)//si;
s/\s*Options:(.+?)\s*\Z//si;
my $opts = parseOpts($_);

# Write the updated result to the man-page file
(my $pagePath = `man -w v8`) =~ s/\s+$//;
my $source = do {{
	local $/ = undef;
	open(my $fh, $pagePath);
	join "", <$fh>
}};

(my $head) = ($source =~ m/(\A.+\n\.\\" BEGIN SCRAPE\n)/s);
(my $foot) = ($source =~ m/(\n\.\\" END SCRAPE\n.+\Z)/s);
open(my $fh, ">", $pagePath) or die("Can't reopen man-page: $!");
print $fh $head . $opts . $foot;
close($fh);
