OS!= uname -s | tr [A-Z] [a-z]
HDD!= if uname -s | grep -q Darwin; \
	then printf /Volumes/Films; \
	else printf /mnt; \
fi
SUDO!= if uname -s | grep -q OpenBSD; \
	then command -v doas; \
	else command -v sudo; \
fi
# Shims for GNU Make
OS   ?= $(shell $(OS!))
HDD  ?= $(shell $(HDD!))
SUDO ?= $(shell $(SUDO!))

# Shim for BSD make
^ ?= $?

# Use modification times of symlinks directly
MAKEFLAGS += -L


all: install links


# Install Soulseek or Nicotine+, whichever's available
install:         install-$(OS)
install-darwin:  /Applications/SoulseekQt.app
install-openbsd: /usr/local/bin/nicotine

/Applications/SoulseekQt.app:; brew install --cask soulseek
/usr/local/bin/nicotine:;      doas pkg_add nicotine-plus


# Link directories to share on Soulseek
SHARED = $(HOME)/.files/var/share/soulseek
links: \
	/usr/local/share/soulseek \
	$(SHARED)/Music \
	$(SHARED)/Books \
	$(SHARED)/Bandcamp \
	$(SHARED)/Movies \
	$(SHARED)/TV

/usr/local/share/soulseek: $(SHARED)
	$(resolve_sudo) "$(@D)"; \
	$$sudo rm -f $@; \
	$$sudo ln -s $^ $@

$(SHARED):; mkdir -p $@
$(SHARED)/Music:    $(SHARED); @$(link) ~/Music
$(SHARED)/Books:    $(SHARED); @$(link) ~/Documents/Books
$(SHARED)/Bandcamp: $(SHARED); @$(link) "$(HDD)/Lossless"
$(SHARED)/Movies:   $(SHARED); @$(link) "$(HDD)/Movies"
$(SHARED)/TV:       $(SHARED); @$(link) "$(HDD)/TV"


# Wipe generated symlinks
clean:
	$(resolve_sudo) /usr/local/share/; \
	$$sudo rm -f /usr/local/share/soulseek; \
	$$sudo rm -rf $(SHARED)

.PHONY: clean


# Create a symbolic link to target if one doesn't exist
link = \
	link(){ \
		if ! test -L "$@"; then \
			ln -s "$$1" "$@" && \
			printf >&2 'Linked: %s -> %s\n' "$$1" "$@"; \
		fi; \
		if touch -ch "$@" 2>&1 | grep -q 'invalid option'; \
			then touch -cr "$$1" "$@"; \
		fi; \
	}; link

# Resolve the utility for running commnds as root, if needed
resolve_sudo = \
	resolve_sudo(){ \
		if test -w "$$1"; \
			then unset sudo; \
			else case `uname -s` in \
				OpenBSD) sudo='doas';; \
				*) sudo='sudo';; \
			esac; \
		fi; \
	}; resolve_sudo
