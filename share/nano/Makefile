all: install

install: $(HOME)/.config/nano syntaxes/default syntaxes/extra


# Link user's nanorc(5) directory
$(HOME)/.config/nano:
	mkdir -p "$(@D)"
	[ -d "$@" ] || ln -s ~/.files/share/nano ~/.config/


# Link syntax-related resource directories
NANO = /usr/local/share/nano
syntaxes/default: $(NANO)
	rm -f $@
	ln -s "$?" $@

syntaxes/extra: $(NANO)/extra
	rm -f $@
	ln -s "$?" $@

syntaxes/user:
	touch -ch $@

syntaxes/%: $(NANO)/%
	[ -h $@ ] && rm -f $@
	ln -s "$?" $@


# Wipe generated symlinks to nanorc(5) directories
clean:
	find syntaxes -type l -maxdepth 1 -print -delete
	rm -f ~/.config/nano
	rmdir ~/.config >/dev/null 2>&1 || :

.PHONY: clean


# Portability hack for Debian-based systems
/usr/local/share/nano:
	[ -w /usr/local/share ] || sudo='sudo'; \
	if test -d /usr/share/nano; \
		then $$sudo ln -s /usr/share/nano $@; \
		else [ -d "$@" ] || $$sudo mkdir -p "$@"; \
	fi
