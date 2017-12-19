# OpenBSD sh/ksh .profile
alias @='emacs'
alias K='kill -KILL'
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
alias npm-start='{ npm start 2>&1; } >/dev/null &'
alias yeah='git reset HEAD .;'
alias nah='git checkout -- . && git clean -fd'
alias PS='git commit --amend'
alias fuck-that-shit='git reset --soft HEAD~1'
alias s='grep -irnw . -e '

{
	export GPG_TTY=`tty`
	eval "$(ssh-agent -s)" >/dev/null
	ssh-add ~/.ssh/github
	gpg-agent
} 2>/dev/null;

# Add paths containing additional manual-pages
MANPATH=:/root/.files/share/man
MANPATH=$MANPATH:/usr/local/lib/node_modules/npm/man
export MANPATH

# Test function pinched from `functions.sh`
t(){
	[ -x "test.js" ] && {
		node ./test.js $@;
		return $?;
	}
}
