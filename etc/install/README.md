Workstation setup reminders
========================================================================

__Intended audience:__  
Myself, probably long after the following steps have left my memory.



Steps
------------------------------------------------------------------------

1. Install [Homebrew](https://brew.sh).

2. Run `install.sh` in this directory.

3. Update `/private/etc/apache2/extra/httpd-vhosts.conf`:

~~~apache
<Directory "/Users/johngardner/Labs">
	Options All
	AllowOverride All
	Order allow,deny
	Allow from all
</Directory>

<VirtualHost *:80>
	DocumentRoot "/Users/johngardner/Labs"
	ServerName labs
</VirtualHost>
~~~


4. Update `/private/etc/apache2/httpd.conf`:

~~~diff
-#LoadModule userdir_module libexec/apache2/mod_userdir.so
+LoadModule userdir_module libexec/apache2/mod_userdir.so

-#LoadModule rewrite_module libexec/apache2/mod_rewrite.so
+LoadModule rewrite_module libexec/apache2/mod_rewrite.so

-User _www
-Group _www
+User johngardner
+Group staff

-#ServerName www.example.com:80
+ServerName localhost

-<Directory "/Library/WebServer/Documents">
+<Directory />

-    Options FollowSymLinks Multiviews
-    MultiviewsMatch Any
+    Options FollowSymLinks

-    DirectoryIndex index.html
+    DirectoryIndex index.htm index.html index.php

-#Include /private/etc/apache2/extra/httpd-languages.conf
+Include /private/etc/apache2/extra/httpd-languages.conf

-#Include /private/etc/apache2/extra/httpd-userdir.conf
+Include /private/etc/apache2/extra/httpd-userdir.conf

-#Include /private/etc/apache2/extra/httpd-vhosts.conf
+Include /private/etc/apache2/extra/httpd-vhosts.conf
~~~


5. Update `/etc/hosts`, ensuring *tabs* are used to delimit columns:

~~~~~~~~~~~~~~~~~~~~
127.0.0.1	localhost labs
255.255.255.255	broadcasthost
::1	localhost
fe80::1%lo0	localhost
~~~~~~~~~~~~~~~~~~~~

Confirm correct entry by running:

~~~shell
sudo killall -HUP mDNSResponder
sudo apachectl -k graceful
open http://labs/
~~~


6. Add `.profile` for superuser:

~~~shell
# Run this:
sudo su -l
vim ~/.profile

# Then add:
export PS1='# \W: '
alias l='ls -alh'
alias ..='cd ..'
~~~


7. Update `/private/etc/man.conf` to enable UTF8 output in `man(1)`:

~~~
TROFF		/usr/local/bin/groff -Tps -mandoc
NROFF		/usr/local/bin/groff -Tutf8 -mandoc
JNROFF		/usr/local/bin/groff -Tnippon -mandocj
EQN			/usr/local/bin/eqn -Tps
NEQN		/usr/local/bin/eqn -Tutf8
JNEQN		/usr/local/bin/eqn -Tnippon
TBL			/usr/local/bin/tbl
# COL		/usr/local/bin/col
REFER		/usr/local/bin/refer
PIC			/usr/local/bin/pic
VGRIND		
GRAP		/usr/local/bin/grap
PAGER		/usr/bin/less -Ris
BROWSER		/usr/bin/less -Ris
HTMLPAGER	/bin/cat
CAT			/bin/cat
~~~




Notes
------------------------------------------------------------------------

* If MacBook overheats, [enable iCloud keychain][1].

[1]: https://discussions.apple.com/thread/7675366?start=0&tstart=0


* NPM may require [tokens][2] to be re-authenticated.

[2]: https://www.npmjs.com/settings/tokens


* There might be junk in `/private/etc` after upgrading macOS:  
~~~console
ls -a /private/etc | grep -E '~(orig|previous)$'
~~~
Most of these files *should* be safe to delete.


* When reinstalling Groff, some fonts may need reconverting:

| Groff name  | PostScript name      | PFA filename              |
|-------------|----------------------|---------------------------|
| `CL`        | `ClarendonBT-Heavy`  | `clarendon.pfa`           |
| `CAMBRIA`   | `CambriaMath`        | `cambria.pfa`             |
| `CAMBRIABI` | `Cambria-BoldItalic` | `cambria-bold-italic.pfa` |
| `CAMBRIAB`  | `Cambria-Bold`       | `cambria-bold.pfa`        |
| `CAMBRIAI`  | `Cambria-Italic`     | `cambria-italic.pfa`      |


* These repositories have been mirrored locally in `~/Mirrors`:
~~~
ATT-DFORMAT         sathlan/dformat
ATT-Research        att/ast
ATT-UnixHistory     dspinellis/unix-history-repo
Adobe-AGL-Lists     adobe-type-tools/agl-aglfn
FFmpeg              https://git.ffmpeg.org/ffmpeg.git
FontForge           fontforge/fontforge
GNU-APL             svn://svn.savannah.gnu.org/apl/trunk
GNU-Emacs           git://git.savannah.gnu.org/emacs.git
GNU-Groff           git://git.savannah.gnu.org/groff.git
GOW                 bmatzelle/gow
Git                 git/git
Google-Brotli       google/brotli
Google-DepotTools   https://chromium.googlesource.com/chromium/tools/depot_tools.git
Google-Fonts        google/fonts
Google-Noto         googlei18n/noto-fonts
Google-NotoCJK      googlei18n/noto-cjk
Google-NotoEmoji    googlei18n/noto-emoji
Google-WOFF2        google/woff2
IEVMS               xdissent/ievms
Logos               gilbarbara/logos
Roff-9Front         n-t-roff/9front_troff
Roff-Classical      bwarken/roff_classical
Roff-DWB3.3         n-t-roff/DWB3.3
Roff-Heirloom       n-t-roff/heirloom-doctools
Roff-History        bwarken/RUNOFF_historical
Roff-Plan9Troff     n-t-roff/Plan9_troff
Roff-RUNOFF         bwarken/runoff
Roff-Solaris10      n-t-roff/Solaris10-ditroff
URW-Core35-Fonts    git://git.ghostscript.com/urw-core35-fonts
Watchman            facebook/watchman
WebAssembly-WABT    WebAssembly/wabt
ansicon             adoxa/ansicon
libuv               libuv/libuv
re2c                skvadrik/re2c
v8                  https://chromium.googlesource.com/v8/v8.git
~~~
