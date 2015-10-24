#!/bin/sh
brew list -l | perl -e 'while(<>){ print /(\S+\n)$/; }' > brew-list.txt;

