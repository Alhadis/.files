# Generate a purdy PostScript of V8's glossy new manpage
v8-src := share/man/man1/v8.1
v8-obj := $(v8-src:.1=.ps)
$(v8-obj): $(v8-src)
	groff -man -eC -Tps $^ > $@
v8: $(v8-obj)


list := etc/brew-list.txt

# Dump a list of currently-installed Homebrew formulae
brew-list:
	@echo "Casks" > $(list)
	@brew cask list 2>/dev/null | sed -r "s/^/\t/g" >> $(list)
	@echo "Taps" >> $(list)
	@brew tap  | sed -r "s/^/\t/g" >> $(list)
	@echo "Formulae" >> $(list)
	@brew list | sed -r "s/^/\t/g" >> $(list)
