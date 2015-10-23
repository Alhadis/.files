export PATH="$HOME/.files/bin:$PATH"

# Load connected files
for i in ~/.files/{aliases,paths,functions}.sh; do
	source "$i";
done;


# Shell options
shopt -s nocaseglob;	# Case-insensitive globbing
shopt -s histappend;	# Append Bash history instead of replace
shopt -s autocd;		# DOS-style directory navigation
shopt -s globstar;		# Enable recursive globbing


# Various other crap
{ rm ~/.DS_Store; dsclean ~/Desktop; } > /dev/null 2>&1
cd ~/Desktop; fit;
