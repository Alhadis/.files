# Shim for BSD make
^ ?= $?

all: profile links

# Symlink to resolved location of Firefox's user settings directory
app-data:
	@ $(require) readlink
	@ if uname -s | grep -qi Darwin >/dev/null 2>&1; \
		then root=~/'Library/Application Support/Firefox'; \
		else root=~/'.mozilla/firefox'; \
	fi; \
	if ! test -d "$$root"; then \
		echo >&2 "Failed to locate Firefox preference directory; aborting"; \
		exit 1; \
	elif ! test -s "$$root/profiles.ini"; then \
		echo >&2 "Expected 'profiles.ini' file not to be empty"; \
		exit 1; \
	fi; \
	if test -L "$@"; then \
		case `readlink "$@" | tr -d '\n'` in \
			"$$root") touch -h "$@";; \
			*) rm -f "$@";; \
		esac; \
	elif test -d "$@"; then \
		echo >&2 "Symlink target exists and is a directory"; \
		exit 1; \
	else \
		$(symlink) "$$root"; \
	fi

# Symlink pointing to user's main Firefox profile
profile: app-data
	@ $(require) mozinstallhash
	@ list=$^/profiles.ini; \
	if uname -s | grep -qi Darwin >/dev/null 2>&1; \
		then install_dir='/Applications/Firefox.app/Contents/MacOS'; \
		else install_dir='/usr/local/lib/firefox'; \
	fi; \
	hash=`mozinstallhash "$$install_dir"`; \
	keys=`git config -f "$$list" --list | grep -io '^Install[^=.]*' | sort | uniq`; \
	profile=`printf '%s' "$$keys" | grep -xiF "Install$$hash"`; \
	profile=`git config -f "$$list" --get "$$profile.default"`; \
	if test -d "$^/$$profile"; then $(symlink) "$^/$$profile"; fi

# HACK: Resolve profile location without marking it as a prerequisite
link_profile = \
	test -h profile || $(MAKE) profile


# Create symbolic and hard-links in Firefox user profile
links: \
	profile/user.js \
	profile/handlers.json \
	profile/chrome/userChrome.css \
	profile/chrome/userContent.css \
	profile/search.json.mozlz4

# Symlinked list of persistent user preferences
profile/user.js: user.js
	@ $(link_profile)
	@ $(symlink) ~/.files/etc/firefox/$^

# Hard-linked JSON directory of scheme/MIME-type handlers
profile/handlers.json: handlers.json
	@ $(link_profile)
	@ if ! test $^ -ef $@; then $(hardlink) $^; fi

# User-defined styling tweaks for browser UI and webpage content, respectively
# FIXME: Firefox refuses to load a profile's “chrome” directory that's symlinked
profile/chrome/userChrome.css:  chrome/userChrome.css;  @ $(link_user_stylesheet)
profile/chrome/userContent.css: chrome/userContent.css; @ $(link_user_stylesheet)
link_user_stylesheet = \
	$(link_profile); \
	mkdir -p profile/chrome; \
	src="$@"; src="$${src\#profile/}"; \
	if ! test "$$src" -ef $@; then $(hardlink) "$$src" && touch -c $^ $(@D); fi

# Compressed JSON listing of configured search engines.
# See: https://support.mozilla.org/en-US/questions/1436443
profile/search.json.mozlz4: search.json
	@ $(require) mozlz4 jq
	jq -c . "$^" | mozlz4 --compress - "$^.mozlz4"
	@ $(link_profile)
	@ $(abort_if_running) Firefox 'browser windows'
	mv "$^.mozlz4" "$@"


# Regenerate uBlock settings backup
ublock/settings.json: ublock/filters.txt ublock/whitelist.txt
	@ $(link_profile)
	@ $(require) unzip jq
	@ echo >&2 "Updating uBlock settings file…"; set -e; \
	xpi='profile/extensions/uBlock0@raymondhill.net.xpi'; \
	expected_version=`jq -r .version < $@`; \
	installed_version=`unzip -p "$$xpi" manifest.json | jq -r .version`; \
	if ! test "$$expected_version" = "$$installed_version"; then \
		echo >&2 'uBlock version mismatch (on-file: $$expected_version, installed: $$installed_version)'; \
		exit 1; \
	fi; \
	unset file_changed; \
	whitelist=`sed -n '/^[^[:blank:]#]/p' ublock/whitelist.txt | sort | uniq | sed '\
		s/..*/\t\t"&",/; \
		$$ s/,$$/\n\t],/; \
		1 s/^/\t"whitelist": [\n/; '`; \
	set -- '/^\t"whitelist":[ \t]*\[$$/' '/^\t\],$$/'; \
	head=`sed -n "1,   $$1 p" $@`; \
	body=`sed -n "$$1, $$2 p" $@`; \
	head=`printf '%s' "$$head" | sed "$$ s$${1}/"`; \
	if test "$$whitelist" = "$$body"; then \
		echo >&2 "Whitelist already up-to-date."; \
	else \
		head_lines=`printf '%s\n' "$$head" | wc -l | sed 's/^[[:blank:]]*//'`; \
		body_lines=`printf '%s\n' "$$body" | wc -l | sed 's/^[[:blank:]]*//'`; \
		tail_lines="$$((head_lines+body_lines+1))"; \
		tail=`sed -n "$$tail_lines, $$ p" $@`; \
		echo >&2 "Updating whitelist…"; \
		printf > $@ '%s\n%s\n%s\n' "$$head" "$$whitelist" "$$tail"; \
		file_changed=1; \
	fi; \
	old_filters=`jq .userFilters $@`; \
	new_filters=`sed -n '/[^ \t]/,$$p' ublock/filters.txt | sed -e :a -e '/^\n*$$/{$$d;N;};/\n$$/ba'`; \
	new_filters=`printf %s "$$new_filters" | jq -Rs .`; \
	if test "$$old_filters" = "$$new_filters"; then \
		echo >&2 "Filters already up-to-date."; \
	else \
		set -- '/^\t*"userFilters":[[:blank:]]*".*"$$/'; \
		head=`sed -n "1, $$1 p"  $@ | sed "$$1 d"`; \
		tail=`sed -n "$$1, $$ p" $@ | sed "$$1 d"`; \
		body="\"userFilters\": $$new_filters"; \
		printf > $@ '%s\n\t%s\n%s\n' "$$head" "$$body" "$$tail"; \
		file_changed=1; \
	fi; \
	if test "$$file_changed" = 1; then \
		echo >&2 'Settings file updated.'; \
		mtime=`jq -n 'now * 1000 | floor'`; \
		sed -i.bak 's/^\(\t"timeStamp":[[:blank:]]*\)[0-9]\{1,\},$$/\1'"$$mtime,/" "$@"; \
		touch -cr "$@.bak" "$@"; \
		rm -f "$@.bak"; \
		echo >&2 "Timestamp set to $$mtime"; \
	else \
		echo >&2 "Settings file unchanged; updating timestamp."; \
		touch -c $@; \
	fi


# Delete generated symbolic links
clean:
	rm -f app-data
	rm -f profile
	rm -f search.json.mozlz4
.PHONY: clean


# Delete both generated files *and* linked profile resources
clobber:
	rm -f profile/user.js
	rm -f profile/handlers.json
	rm -f profile/search.json.mozlz4
	rm -f profile/chrome/*.css
	rmdir profile/chrome >/dev/null 2>&1 || :
	$(MAKE) clean
.PHONY: clobber


# Declare a list of programs as recipe dependencies
require = set -e; $(require__fn); require
require__fn = \
	require(){ \
		while [ $$\# -gt 0 ]; do command >/dev/null 2>&1 -v "$$1" || { \
			echo >&2 "Required command '$$1' not found; aborting"; \
			exit 1; \
		}; shift; done; \
	}

# Verbosely create a symbolic link
symlink = $(symlink__fn) && symlink
symlink__fn = \
	symlink(){ \
		test -d "$$2" || rm -f "$$2"; \
		case $$\# in 0) set -- $^ $@;; 1) set -- "$$1" $@;; esac; \
		ln -fs "$$1" "$$2" || return $$?; \
		case $$2 in /*|../*|./*);; *) \
			set -- "$$1" "~/.files/etc/firefox/$$2";; \
		esac; echo >&2 "Linked: $$2 -> $$1"; \
	}

# Verbosely create a hard-link
hardlink = $(hardlink__fn) && hardlink
hardlink__fn = \
	hardlink(){ \
		test -d "$$2" || rm -f "$$2"; \
		case $$\# in 0) set -- $^ $@;; 1) set -- "$$1" $@;; esac; \
		ln -f "$$1" "$$2" || return $$?; \
		case $$2 in /*|../*|./*);; *) \
			set -- "$$1" "~/.files/etc/firefox/$$2";; \
		esac; echo >&2 "Linked: $$1 => $$2"; \
	}

# Check if an application is currently running
# XXX: Only tested on macOS; needs testing on OpenBSD/Linux
is_app_running = $(is_app_running__fn); is_app_running
is_app_running__fn = \
	is_app_running(){ \
		if uname -s | grep -qi Darwin >/dev/null 2>&1; \
			then lsappinfo info -only pid -app "$$1" | grep -qiE '^("pid"=)?[0-9]+$$'; \
			else ps aux | grep -qiE "[ /]$$1\\$$"; \
		fi; \
	}

# Terminate with an error code if a process named $1 is currently running
abort_if_running = set -e; $(abort_if_running__fn); abort_if_running
abort_if_running__fn = \
	$(is_app_running__fn); \
	abort_if_running(){ \
		if is_app_running "$$1"; then \
			echo >&2 "Cannot proceed with an active $$1 process."; \
			echo >&2 "Close all $${2:-application instances}, then try again."; \
			exit 1; \
		fi; \
	}
