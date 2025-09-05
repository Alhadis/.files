<!-- vim:se noet lbr tw=72 sts=0 sw=4 ts=4 wrap:
--*- fill-column: 72; truncate-lines: nil; word-wrap: t; -*-->

Deprecation notice
========================================================================
As of September 5th 2025, the user's custom wordlist is sourced from the
[Firefox profile directory](../../etc/firefox) (see [`persdict.dat`][]).
This single file is intended for use by multiple spellchecking software,
on both macOS and OpenBSD (with [Hunspell][] being used on the latter).

[`persdict.dat`]: ../../etc/firefox/persdict.dat
[Hunspell]: https://hunspell.github.io/

The contents of this directory are now considered deprecated and will be
removed in a future commit. The `en_AU` dictionary files should be taken
from local mirror(s) of LibreOffice's [dictionary repository][1], or the
[SCOWL project's wordlist sources][2].

[1]: https://anongit.freedesktop.org/git/libreoffice/dictionaries.git]
[2]: https://github.com/en-wl/wordlist


To-do list
------------------------------------------------------------------------
* Implement Makefile task for relinking custom dictionary files to point
  to [`persdict.dat`][], preferably using symbolic links instead of hard
  links. The most relevant wordlist locations on macOS are:
  
      ~/Library/Spelling/en_AU
      ~/Library/ApplicationSupport/Google/Chrome/*/Custom Dictionary.txt

  On OpenBSD and Linux, the pertinent locations are:
  
      ~/.config/chromium/*/Custom Dictionary.txt
      ~/.config/google-chrome/*/Custom Dictionary.txt

* Remove [`en_AU.aff`][] and [`en_AU.dic`][] in favour of something more
  reliable and stable (NB: these files are really only needed by Emacs).
  (Consider this step only after these files have been used as reference
  when writing as-yet non-existent TextMate grammars for the `dic`/`aff`
  file formats described by [`hunspell(5)`][hun]).

* If a tool for converting [Wiktionary dumps] to usable dictionary files
  is ever implemented, consider revisiting [`persdict.dat`][] and nuking
  words that are defined by Wiktionary (computing-related neologisms and
  certain obscure terms aren't included in the [OED][]). [`pyglossary`],
  a Python tool for converting between dictionary/glossary formats, will
  likely be of use here.

[`en_AU.aff`]: ./en_AU.aff
[`en_AU.dic`]: ./en_AU.dic
[`pyglossary`]: https://github.com/ilius/pyglossary
[hun]: https://github.com/hunspell/hunspell/blob/91dc7853/man/hunspell.5
[OED]: https://www.oed.com/
[Wiktionary dumps]: https://dumps.wikimedia.org/
