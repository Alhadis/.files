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
	ld -macosx_version_min 10.7.0 -lSystem -o input input.o

~~~asm
; input.asm
global start
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



Resource fork access on macOS
-----------------------------

…is achieved by affixing `/..namedfork/rsrc` to the subject's filename.

	RezWack -d /dev/null -r Palatino/..namedfork/rsrc -o Palatino.dfont
	cp icon.rsrc file.txt/..namedfork/rsrc && SetFile -a C file.txt

As explained by RezWack(1), this only works on HFS or HFS+ filesystems.



Random weird shit on macOS
--------------------------

	/usr/share/misc/birthtoken
	/usr/share/sandbox/*.sb
	/opt/X11/bin/xkeystone



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



Archive.org
-----------

List everything captured for a domain:

	https://web.archive.org/web/*/http://site.domain/*

Direct link to original (unmodified) file:

	date=`date +%Y%m%d`
	https://web.archive.org/web/${DATE}id_/http://site.domain/page.html

Newest and oldest versions of an archived page, respectively:

	Latest: https://web.archive.org/web/${URL}
	Oldest: https://web.archive.org/web/1000/${URL}

JSON API for interrogating a resource's archive status:

	curl "https://archive.org/wayback/available?url=$URL" \
	| JQ_COLORS= jq -Mre '.archived_snapshots.closest.url | select(. != null)'



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
