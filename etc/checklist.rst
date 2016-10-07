Upgrade/Reinstall Checklist
===========================

Self-reminders of what I need to do after upgrading (or reinstalling) macOS.

Not bothering to write a script, because I don't envision this will need to be done often.


1. Reconnect symlinks
~~~~~~~~~~~~~~~~~~~~~
Relink these files to their relevant directories:

* `<httpd-vhosts.conf>`_: ``/private/etc/apache2/extra/httpd-userdir.conf``
*        `<httpd.conf>`_: ``/private/etc/apache2/httpd.conf``
*           `<php.ini>`_: Version dependent. Run ``brew info php## | grep /php.ini``

Repeat the steps in `8dbf255...51f49c9`__ to preserve the unmodified copies.

	__ https://github.com/Alhadis/.files/compare/8dbf255...51f49c9

Lastly, symlink Apache's logfile somewhere more visible::

	sudo rm -f /private/var/log/apache2/error_log
	ln -s $_ ~/Documents/apache.log


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
