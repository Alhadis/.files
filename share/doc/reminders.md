Random self-reminders
=====================

Crap I can never manage to remember.


Tab-width setting names
-----------------------

	Atom:          tabLength: 4
	
	VSCode:        tabSize: 4
	
	EditorConfig:  indent_size: 4
	               tab_width: 4

	Emacs:         -*- tab-width: 4 -*-

	Vim:           vim: tabstop=4
	               vim: ts=4
	               vim: set ts=4:
Altogether now:

~~~regexp
/(indent|tab)[-_]?(length|size|width|stop)/i
~~~



Copy a file from another computer
---------------------------------

	scp -pr Alhadis@10.0.0.74:/copy/this /paste/it/here
	
	        └────────────┬─────────────┘ └─────┬──────┘
	        Remote machine we're copying    This one


Mirror local directory on remote machine
----------------------------------------

	rsync -ave ssh ~/.atom Alhadis@10.0.0.74:/home/Alhadis/.atom
	rsync -avic --delete ~/Documents/Logs/ Alhadis@10.0.0.74:/tmp/Logs



Reference a range of commits
----------------------------

	AAAAA...FFFFFF
	  │        └─────── Last commit in referenced range
	  └─── NOT included

Expanded, the above becomes:

	    [AAAAA]   BBBBB  CCCCC DDDDD EEEEE FFFFFFF
	     └─┬─┘    └─┬─┘  └───────┬───────┘ └──┬──┘
	    Ignored   First         ...          Last

Test using `git log 30837e3...b51fe81`.

2-dot ellipses have *NEVER* been used. Neither online or in the terminal.



Convert PostScript to PNG
-------------------------

	\gs -r300 -dTextAlphaBits=4 -sDEVICE=png16m -o out-%d.png input.ps



Deep-linking
------------

Text fragments:

	https://url.com/file.html#:~:text=[fragment]

PDFs:

	https://url.com/file.pdf#[named-destination]
	https://url.com/file.pdf#page=[number]

Exempli gratia: [`https://troff.org/54.pdf#page=29`](https://troff.org/54.pdf#page=29)



Encrypt/decrypt shit with GPG
-----------------------------

	gpg -aer Alhadis
	gpg -adr Alhadis



The “GNUmake idiom” OpenBSD hates is `$<`
-----------------------------------------

	Using $< in a non-suffix rule context is a GNUmake idiom



Generate disassembly
--------------------

	clang -S -mllvm --x86-asm-syntax=intel -O0 test.c
	d8 --print-bytecode test.js



“Hello, world” in x86-64 assembly (macOS only)
----------------------------------------------

	nasm -f macho64 -o input.o input.asm
	ld -o input input.o -macosx_version_min 12.7 -static

~~~nasm
; input.asm
global  start
section .text

start:
	mov     rax, 0x2000004 ; write
	mov     rdi, 1         ; stdout
	mov     rsi, qword msg
	mov     rdx, msg.len
	syscall

	mov     rax, 0x2000001 ; exit
	mov     rdi, 0
	syscall

section .data
	msg:    db      "Hello, world!", 10
	.len:   equ     $ - msg
~~~



Clone or update Git submodules
------------------------------

	git submodule init
	git submodule sync --quiet
	git submodule update --recursive



Control characters that're legal in Roff
----------------------------------------

	U+0002 STX ^B
	U+0003 ETX ^C
	U+0005 ENQ ^E
	U+0006 ACK ^F
	U+0007 BEL ^G
	U+007F DEL ^?



Giant terminal text
-------------------

	\e#3    Double-size, top half
	\e#4    Double-size, bottom half
	\e#6    Double-length (stretched)



Configuration priority
----------------------

	1. Command-line
	2. Environment variables
	3. Local configuration
	4. System configuration
	5. Hardcoded defaults



Traditional tab-sizes
---------------------

_De facto_ standard historically adopted for fixed tab-stop sizes in printers:

	HT = 8 columns
	VT = 6 lines

Source: https://w.wiki/Ay8b



VirtualBox logins
-----------------

<!--------------------------------------------------->
| VM            | Username    | Password             |
|---------------|-------------|----------------------|
| IEVMs         |             | `Passw0rd!`          |
| Solaris 11.3  | `vagrant`   | `1vagrant`           |
| Ubuntu        | `vagrant`   | `vagrant`            |
<!--------------------------------------------------->



Bit manipulation
----------------

	bit  = int >> 𝑁 & 1    Get
	int |=   1 << 𝑁        Set
	int &= ~(1 << 𝑁)       Unset
	int ^=   1 << 𝑁        Toggle



Convert CD-R rip in BIN/CUE format to ISO
-----------------------------------------

Use [BinChunker](http://he.fi/bchunk/) to generate mount(1)-able images:

	bchunk cd-rip.{bin,cue} image.iso

Install with:

	apt-get install bchunk  # Linux
	brew install bchunk     # macOS
	pkg_add bchunk          # OpenBSD

Source: https://wp.me/p29O5-15



Modify `$ARGV` whilst keeping first `N` arguments
-------------------------------------------------

	eval "shift 2 && set -- \"$1\" \"\$@\""



Work around overzealous comment censors
---------------------------------------

	U+202C POP DIRECTIONAL FORMATTING
	U+202D LEFT-TO-RIGHT OVERRIDE
	U+202E RIGHT-TO-LEFT OVERRIDE
	U+00A0 NO-BREAK SPACE
	(Followed by characters in reverse order)
