export PATH="$HOME/.files/bin:$PATH"

# Load connected files
for i in ~/.files/{aliases,paths,functions,prompt,tmp}.sh; do
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

# Hunspell dictionaries
export DICPATH=/Applications/LibreOffice.app/Contents/Resources/extensions/dict-en
export WORDLIST="$HOME/Library/Application Support/Google/Chrome/Default/Custom Dictionary.txt"

# Various other crap
{ rm ~/.DS_Store; dsclean ~/Desktop; } > /dev/null 2>&1
cd ~/Desktop; fit;

export BOWER_DIRECTORY="components"
