Random self-reminders
=====================

Crap I can never manage to remember.


1․ Tab-width setting names
--------------------------

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



2․ To copy a file from another computer:
----------------------------------------

	scp -pr Alhadis@10.0.0.74:/copy/this /paste/it/here
	
	        └────────────┬─────────────┘ └─────┬──────┘
	        Remote machine we're copying    This one



3․ Commit-ranges are referenced like this:
------------------------------------------

	AAAAA...FFFFFF
	  │        └─────── Last commit in referenced range
	  └─── NOT included

Expanded, the above becomes:

	    [AAAAA]   BBBBB  CCCCC DDDDD EEEEE FFFFFFF
	     └─┬─┘    └─┬─┘  └───────┬───────┘ └──┬──┘
	    Ignored   First         ...          Last

Test using `git log 30837e3...b51fe81`.

2-dot ellipses have *NEVER* been used. Neither online or in the terminal.



4․ Convert PostScript to PNG:
-----------------------------

	\gs -r300 -dTextAlphaBits=4 -sDEVICE=png16m -o out-%d.png input.ps



5․ Link to a specific page in a PDF:
------------------------------------

	https://url.com/file.pdf#page=[number]

Exempli gratia: [`https://troff.org/54.pdf#page=29`](https://troff.org/54.pdf#page=29)
