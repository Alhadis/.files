AI_CURRENT = illustrator/current
AI_ACTIONS = John's Actions.aia
AI_CLOUD   = Adobe Illustrator Cloud Prefs
AI_PREFS   = Adobe Illustrator Prefs
AI_KEYSET  = John's Keyset.kys
AI_ASSEXP  = Last Used Asset Export Settings
AI_VECTOR  = Vectorizing Presets
AI_WORKSPC = Workspaces/John's Workspace

all: \
	$(AI_CURRENT)

illustrator/current:
	@ ${header}; \
	app=`locateApp com.adobe.illustrator | sort -n | tail -n1`; \
	version=`mdls -name kMDItemVersion -raw "$$app"`; \
	prefs=~/Library/Preferences/"Adobe Illustrator $${version%%.*} Settings"; \
	for lang in "$${LANG%%.*}" en_AU en_GB en en_US; do \
		dir="$$prefs/$$lang"; \
		test -d "$$dir" && { prefs="$$dir"; break; } || unset dir lang; \
	done; \
	test -d "$$prefs" || die 'No such directory: %s' "$$1"; \
	if test -h $@ && [ "$$prefs" = "`realpath $@`" ]; \
	then log 'Already linked: %s' "$@"; exit; \
	else rm -f "$@"; ln -s "$$prefs" "$@" && log 'Linked: %s -> %s' "$@" "$$prefs"; \
	fi

.PHONY: illustrator/current

# Shell functions
header    = ${die}; ${log}; ${warn}; ${locateApp}
log       = \
	log(){ \
		case $$\# in \
			0) return;; \
			1) set -- '%s' "$$1";; \
		esac; \
		set -- "$$1\n" "$$@"; \
		eval "shift 2 && set -- \"$$1\" \"\$$@\""; \
		printf "$$@"; \
	}
die       = die(){ warn "$${1:-Fatal error}"; exit 1; }
warn      = warn(){ log "$$@" >&2; }
locateApp = locateApp(){ mdfind "kMDItemCFBundleIdentifier == \"$$1\" && kMDItemContentType == 'com.apple.application-bundle'"; }

# Declare a list of programs as recipe dependencies
require = $(require__fn); require
require__fn = \
	require(){ \
		while [ $$\# -gt 0 ]; do command >/dev/null 2>&1 -v "$$1" || { \
			 \
			return 1; \
		}; shift; done; \
	}
