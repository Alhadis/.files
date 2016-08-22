# Generate a purdy PostScript of V8's glossy new manpage
v8-src := share/man/man1/v8.1
v8-obj := $(v8-src:.1=.ps)
$(v8-obj): $(v8-src)
	groff -man -eC -Tps $^ > $@
v8: $(v8-obj)
