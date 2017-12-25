# OpenBSD sh/ksh .profile
alias @='emacs'
alias K='kill -KILL'
alias F='cd /root/.files'
alias P='cd /home/projects'
alias E='cd /root/.files/.emacs.d'
alias npm-start='{ npm start 2>&1; } >/dev/null &'

# Copied from `aliases.sh`
alias l='ls -alh'
alias ..='cd ..'
alias c='clear'
alias g='git status'
alias gl='git log'
alias gd='git diff --cached'
alias ga='git add --all'
alias gb='git branch -av'
alias gc='git gc && git prune -v'
alias gr='git remote --verbose'
alias gs='git show'
alias GS='/usr/local/bin/gs'
alias gp='git push'
alias yeah='git reset HEAD .;'
alias nah='git checkout -- . && git clean -fd'
alias PS='git commit --amend'
alias fuck-that-shit='git reset --soft HEAD~1'
alias s='grep -irnw . -e '
alias t='run-tests'

{
	export GPG_TTY=`tty`
	eval "$(ssh-agent -s)" >/dev/null
	ssh-add ~/.ssh/github
	gpg-agent
} 2>/dev/null;

# Configure $PATH
export PATH=~/.files/bin:$PATH

# Add paths containing additional manual-pages
MANPATH=:~/.files/share/man
MANPATH=$MANPATH:/usr/local/heirloom-doctools/man
MANPATH=$MANPATH:/usr/local/lib/node_modules/npm/man
export MANPATH

# Path shortcuts
export F=/root/.files
export P=/root/.files/.profile
export E=/root/.files/.emacs.d/init.el

# Everything else
export LESS=-R


# Print temperature diagnostics
temp(){
	sysctl hw.sensors | grep temp | \
	sed 's/hw.sensors.//; s/\.temp[0-9]=/: /;'
}
