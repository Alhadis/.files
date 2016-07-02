#!/bin/sh
cd $(dirname "$0")
brew list > brew-list.txt

# NOTES FOR LATER #
# =============== #

# Dependencies to prune if programs are uninstalled:
# - Homebrew/gui/inkscape: libsigc++, cairomm, glibmm, pangomm, atkmm, gtkmm, popt
