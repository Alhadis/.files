Upgrade/Reinstall Checklist
===========================

Self-reminders of what I need to do after upgrading (or reinstalling) macOS.

Not bothering to write a script, because I don't envision this will need to be done often.


1. Reconnect symlinks
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

================= ==========================
Point these paths to these files:
================= ==========================
``~/.emacs.d``    → `<.emacs.d>`_
``~/.gitconfig``  → `<.gitconfig>`_
``~/.vimrc``      → `<.vimrc>`_
``~/.ssh/config`` → `<ssh-config>`_
|httpd-vhosts|    → `<httpd-vhosts.conf>`_
|httpd|           → `<httpd.conf>`_
``php.ini``       → ``$PHP_DIR``
================= ==========================

.. NOTE:: Repeat the steps in `8dbf255...51f49c9`__ to preserve the unmodified ``httpd-`` files.

To relocate ``$PHP_DIR``, run ``brew info php## | grep /php.ini``. Swap ``##`` with the PHP version we're using (e.g., ``php71``).

Some INI settings will also need updating::

	error_log = "/Users/johngardner/Documents/apache.log"
	max_input_vars = 99999
	post_max_size = 32M
	serialize_precision = 17
	upload_max_filesize = 90M

Finally, symlink Apache's logfile somewhere more visible::

	sudo rm -f /private/var/log/apache2/error_log
	ln -s $_ ~/Documents/apache.log
	chown johngardner:staff $_

.. Referenced links: ========================================================
.. |httpd-vhosts| replace:: ``/private/etc/apache2/extra/httpd-userdir.conf``
.. |httpd|        replace:: ``/private/etc/apache2/httpd.conf``
__ https://github.com/Alhadis/.files/compare/8dbf255...51f49c9



2. Restore ``/etc/hosts`` file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Copy this, and ensure *tabs* are used to delimit columns::

	127.0.0.1	localhost jobs labs bowstring.labs apache.labs
	255.255.255.255	broadcasthost
	::1	localhost
	fe80::1%lo0	localhost

Confirm by running::

	sudo killall -HUP mDNSResponder
	sudo apache -k graceful
	open http://labs/


3. Add shell-profile for root user
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Run::
	
	sudo su -l
	cd ~
	vim .profile

Then add::
	
	export PS1='# \W: '
	alias l='ls -alh'
	alias ..='cd ..'



4. Clean up ``/private/etc``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There'll be ``~orig`` and ``~previous`` junk littered everywhere, which is safe to delete.

If we happen to have made important changes to files that aren't symlinked back here, transfer them before culling.



5. Other
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* **Hide ~/perl5**::

	cd ~
	chflags hidden perl5

* **Add line to ~/bash_profile**::

	eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"

* **Convert fonts for Groff**:

==========  ==================  =======================
Groff file  PostScript name     PFA file
==========  ==================  =======================
CL          ClarendonBT-Heavy   clarendon.pfa
CAMBRIA     CambriaMath         cambria.pfa
CAMBRIABI   Cambria-BoldItalic  cambria-bold-italic.pfa
CAMBRIAB    Cambria-Bold        cambria-bold.pfa
CAMBRIAI    Cambria-Italic      cambria-italic.pfa
==========  ==================  =======================

Usually located in ``$GROFF_FONT`` (``/usr/local/share/groff/1.22.3/font/devps``).
