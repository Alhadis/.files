MAILCHECK=300
DIRSTACKSIZE=20

# Disable history logging
SAVEHIST=0
HISTFILE=

# Use Emacs-flavoured keybindings
bindkey -e

# Disable some annoying defaults
unsetopt beep
unsetopt caseglob
unsetopt sharehistory

# Configure zsh(1)-specific options
setopt alwaystoend
setopt autocd
setopt autocontinue
setopt autolist
setopt bsdecho
setopt cdablevars
setopt extendedglob
setopt globdots
setopt globstarshort
setopt histignoredups
setopt interactivecomments
setopt longlistjobs
setopt mailwarning
setopt notify
setopt numericglobsort
setopt pipefail
setopt promptsubst
setopt pushdignoredups
setopt pushdsilent
setopt pushdtohome
setopt rcquotes

# We're done initialising zsh(1). Load our main profile.
if [ -r ~/.profile ]; then
	. ~/.profile
fi
