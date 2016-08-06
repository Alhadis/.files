export PATH="$HOME/.files/bin:$PATH"
export PS1="\[\033[38;5;10m\]λ \W\[\$(print-branch)\033[38;5;22m:\033[0m\] "

# Load connected files
for i in ~/.files/{aliases,paths,functions,tmp}.sh; do
	source "$i";
done;
unset i


# Shell options
shopt -s nocaseglob;    # Case-insensitive globbing
shopt -s histappend;    # Append Bash history instead of replace
shopt -s autocd;        # DOS-style directory navigation
shopt -s globstar;      # Enable recursive globbing


# Editor paths
export HOMEBREW_EDITOR=/usr/local/bin/atom\ --dev
export EDITOR=/usr/local/bin/emacs
export VISUAL=/usr/local/bin/vim


# Various other crap
{ rm ~/.DS_Store; dsclean ~/Desktop; } > /dev/null 2>&1
cd ~/Desktop; fit;

export BOWER_DIRECTORY="components"
