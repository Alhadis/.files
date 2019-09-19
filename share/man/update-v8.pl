#!/usr/bin/env perl
use strict;
use warnings;
use autodie;
use v5.14;
use utf8;
use POSIX;
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
		next if m/^\(deprecated/i or $key =~ m/^
			( help
			| testing-d8-test-runner
			| testing-bool-flag
			) $
			/ix;
		
		
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
		s/^Enable "Add ([^"\s]+)\s+(option to DateTimeFormat)"[,.\h]*$/Add\n.`` $1\n$2\n/gim;
		s/^(?:Increase |Decrease )?\K(Max|Min) /${1}imum /gi;
		s/reducing it's size/reducing its size/;
		s/only smi values/only SMI values/g;
		s/(?<=\s)an unit/a unit/g;
		s/(?<=\w with)\K(?=the \w)/ /g;
		s/\(in M\Kb(?=ytes\))/B/gi;

		# Format the switch's type and default value
		my $desc = $_;
		my $typeArgs = "";
		my %attr = $_ =~ m/
			\s+ (type):    \h+ (\S.*?)
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
					$desc =~ s/^Allocate\Ks(?=\h)//i;
					
					# Try to negate sentence, but don't try TOO hard
					unless(
						$desc =~ s/^Enable/Disable/i  or
						$desc =~ s/^Include/Exclude/i or
						$desc =~ s/^(
							Abort|Allocate|Automatically|Analy[zs]e|Cache|Compact|Elide|Expose|Filter|
							Free|Generate|Get|Inline|Intrinsify|Optimi[sz]e|Pretenure|Promote|Randomi[zs]e|
							Rehash|Run|Rewrite|Share|Skip|Split|Trace|Track|Trigger|Use|Validate|Write
						)(?=\h)/Don't \l$1/xi
					){
						my $replacements_line = __LINE__ + 1;
						my %replacements = (
							"concurrent-recompilation" => "Force synchronous optimisation of hot functions.",
							"idle-time-scavenge" => "Don't perform scavenges in idle time.",
							"log-colour" => "Don't use coloured output when logging.",
							"logfile-per-isolate" => "Use a single log-file for each isolate.",
							"fast-math" => "Don't enable faster, potentially less accurate, math functions.",
							"flush-bytecode" => "Don't flush bytecode that hasn't executed recently.",
							"parallel-scavenge" => "Disable parallel scavenging.",
							"polymorphic-inlining" => "Disable polymorphic inlining.",
							"prof-browser-mode" => "Turn off browser-compatible mode when profiling with --prof.",
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
							print STDERR "Unable to negate sentence for \e[4m--$key\e[24m: \e[7m$desc\e[27m\n";
							print STDERR "Please update \`\%replacements\` list in " . __FILE__ . " (line $replacements_line)\n";
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
			$desc =~ s/^Print\Ks(?=\s)//gm;
			$desc =~ s/^Print number of allocations and enable\Ks (analysis mode for) gc fuzz (?=testing)/ $1 GC fuzz-/i;
			$desc =~ s/\ADisable \Kglobal\.\Z/\n.JS global .\n/;
			$desc =~ s/\((\d+) \+ (heap_growing_percent)\/100\)\.?/\n.EQ\n( $1 + $2 \/ 100 ).\n.EN\n/;
			$desc =~ s/(?<=Disallow )eval\h+(?=and friends\.?$)/\n.`` eval\n/mi;
			$desc =~ s/(?<=Maximum size of the heap \(in Mbytes\))\h+b(?=.+?semi.space.size\b)/.\nB/i;
			$desc =~ s/(?<=\s)max_(semi|old)_space_size(?=\s)/--max-$1-space-size/g;
			$desc =~ s/(?<=Expose )(async_hooks|freeBuffer|gc|injected-script-source\.js)\h+/\n.`` $1\n/g;
			$desc =~ s/Enables? optimi\Kz(ations?)(?=\h|$)/s$1/gm;
			$desc =~ s/for \Kspecific CPU\.$/a specific CPU\./im;
			$desc =~ s/ favo\Kr /u$&/gi;
			$desc =~ s/(?<=Print mutator utili)\Kzation\b/sation/;
			$desc =~ s/behavio\K(rs to ease correctness fuzzing:)\h+(A)/u$1\n\l$2/i;
			$desc =~ s/, gc(?= speed\.)|(?<=target )os\./\U$&/gi;
			$desc =~ s/(?<=target arch)(?=[.,])/itecture/gi;
			$desc =~ s/during initial compile\K(?= but regenerate)/,/i;
			$desc =~ s/(?<=Disable )(await) (?=taking 1 tick)/\n.JS $1\n/;
			$desc =~ s/ lazily\K (?=compiled\b)/-/gi;
			$desc =~ s/ to track in \KPOLYMORPHIC(?= state)/\\*(CW$&\\fP/g;
			$desc =~ s/Stress\K (?=test)\b/-/gi;
			$desc =~ s/(?<= )l(?=inux profiler)/L/;
			$desc =~ s/(?<=Dump )elf(?= objects)/ELF/;
			$desc =~ s/(?<= )h(?=armony )/H/;
			$desc =~ s/(?<= after lazy compil)e\b/ation/;
			$desc =~ s/
				(StubName,NodeId|ll_prof|ASM_UNIMPLEMENTED_BREAK|cputracemark)\b
				$punct \h*
			/\n.`` $1 $2\n/gx;
			$desc =~ s/\b
				( Error\.stack
				| ArrayBuffer
				| RangeError
				| JSON\.stringify
				| Promise(?:\.allSettled)?
				| BigInt(?:\.[\$\w]+)*+
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
			$desc =~ s/$matchKeys(?![-_\w])/"\\*(C!".($& =~ y|_|-|r)."\\fP"/eg;
			$desc =~ s/($matchURL)$punct/\n.LK "$1" $2\n/g;
			$desc =~ s/(?<=\h)(gc[-_]interval|stress[-_]compaction)(?!-)\b/"\\*(C!--".($& =~ tr#_#-#r)."\\fP"/eg;
			$desc =~ s/code-<pid>-<isolate id>(\.asm\.?)/\n.RI \\(lqcode- pid - isolate-id $1\\(rq/g;
			$desc =~ s/(?<=\h)(random)\(0,\h*([xX])\)\h*/\\*(CB$1\\fP\\*(CW(0,\\fP\n.VAR $2 )\n/g;
			$desc =~ s/\bC\+\+/\\*(C+/g;
			$desc =~ s/(?<=Disable namespace exports \()[^)\n]+(?=\))/"\\f(CW" . ($& =~ tr|'"|"'|r) . "\\fP"/e;
			$desc =~ s/^Disable \Khashbang(?= syntax\.$)/support for interpreter directive (hashbang)/m;
			$desc =~ s/(?:Can|Don|Hasn|Won|Shouldn|Wouldn)\K'(?=t )/\\(cq/gi;
			$desc =~ s/^(?=New background|Less compaction)./Use \l$&/im;
			$desc =~ s/ease correctness fuzzing: \KAbort/\L$&/i;
			$desc =~ s/^(Include|Exclude)\Ks(?=\h)//gmi;
			$desc =~ s/^(Disable|Enable) "(Add calendar and numberingSystem to DateTimeFormat)"/$2/mi;
			$desc =~ s/^(Disable|Enable) "(Unified Intl.NumberFormat )(Features)"/$1 \l$2\l$3/mi;
			$desc =~ s/^(Disable|Enable)s /$1 /gmi;
			$desc =~ s/^(?:Disable|Enable) \K"(\n\.JS[^\n]+)\n"([.,])/$1 $2/gm;
			$desc =~ s/^(?:Disable|Enable) \K"(DateTimeFormat) (formatRange)"/\n.JS $1.$2 /m;
			$desc =~ s/^(?:Disable|Enable) \K"(dateStyle) (timeStyle)( for DateTimeFormat)"/\\f(CW$1\\fP and \\f(CW$2\\fP$3/m;
			$desc =~ s/^Add \K(calendar) and (numberingSystem)(?= to DateTimeFormat\.)/\\f(CW$1\\fP and \\f(CW$2\\fP/m;
			$desc =~ s/^(?!\.).*?\s\K(Intl\.NumberFormat|DateTimeFormat)([.,]|(?:\h+|$))/\n.JS $1 $2\n/gm;
			$desc =~ s/^\.JS +\S+\K\h+(?=[^.,\s])//gm;
			$desc =~ s/^\.JS.+?\K\h{2,}(?=[.,]\h*$)/ /gm;
			$desc =~ s/(?<=\w)'(?=s )/\\(cq/g;
			$desc =~ s/(\n\.(?:``|JS).+)\n+/$1\n/g;
			$desc =~ s/\.\nU(?=se a fixed suppression string)/,\nand u/is;
			$desc =~ s/^Freelist strategy to use\K:\h*/.\n/m;
			$desc =~ s/^([12])=([^\s.,]+)[.,]?$/\\*(C?$1\\fP selects \\*(C!$2\\fP,/igm;
			$desc =~ s/^Anything else=([^\s.,]+)[.,]?$/and any other value selects \\*(C!$1\\fP.\n/igm;
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

# Load man-page
(my $pagePath = `man -w v8`) =~ s/\s+$//;
my $source = do {{
	local $/ = undef;
	open(my $fh, $pagePath);
	join "", <$fh>
}};

# Extract the document's header and footer
(my $head) = ($source =~ m/(\A.+\n\.\\" BEGIN SCRAPE\n)/s);
(my $foot) = ($source =~ m/(\n\.\\" END SCRAPE\n.+\Z)/s);

# Update revision date and version string
(my $version) = (`echo exit | d8 --version` =~ /^V8 version v?([\d.]+)$/mi);
if($version){
	my ($day, $month, $year) = (localtime())[3..5];
	$month = POSIX::strftime("%B", 0, 0, 0, $day, $month, $year);
	$year += 1900;
	$head =~ s/^\.TH V8 1 \K"[^"]*" "[^"]*"/"$month $day, $year" "V8 $version"/m;
	$foot =~ s/\\\(co 2016-\K\d+(?=,\n\.MT\h+gardnerjohng)/$year/;
}

# Piece it back together
open(my $fh, ">", $pagePath) or die("Can't reopen man-page: $!");
print $fh $head . $opts . $foot;
close($fh);
