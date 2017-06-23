# Load the default .profile
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Install CPAN modules outside Homebrew Cellar
eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"


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
shopt -s dotglob;       # Enable .file globbing


# Various other crap
{ rm ~/.DS_Store; dsclean ~/Desktop; } > /dev/null 2>&1
cd ~/Desktop; fit;

export BOWER_DIRECTORY="components"
export GREP_COLORS="sl=38;5;240:mt=1;38;5;10;48;5;22:fn=38;5;242:se=38;5;237:ln=38;5;10"
export SHELL_SESSION_HISTORY=0
unset ATOM_HOME
