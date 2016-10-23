# Commonly-accessed directories
export F=~/.files
export JOBS=~/Jobs
export LABS=~/Labs
export AF=~/Forks/Atom
export SNIP=~/Labs/Snippets
export ATOM_THEME=~/Labs/Atom-PhoenixTheme
export FILE_ICONS=~/.atom/packages/file-icons

# Stuff I can never seem to memorise
export APACHE_DIR=/private/etc/apache2
export APACHE_LOGS=/private/var/log/apache2
export PHP_INI=$(php -i | grep 'Loaded Configuration File' | sed -E 's/\s*=>\s*/:/g' | cut -d : -f 2)

# Stuff I can only memorise halfway
export SNIPS=$SNIP
export SNIPPETS=$SNIP

# Frequently-accessed projects
export ACC=~/Labs/Accordion
export AM=~/Labs/Atom-Mocha
export APL=~/Labs/language-apl
export LING=~/Forks/GitHub-Linguist
export MA=~/Jobs/MA-Mobile
export ROFF=~/Labs/language-roff
export SHITPOSTS=~/Labs/Sandbox/*/docs/../
export SIGILS=~/Labs/Atom-Sigils
export TMCG=~/Labs/TMCG-Spec

# Anything else
export ANDROID_HOME=/usr/local/opt/android-sdk
export GROFF_FONTS=/usr/local/Cellar/groff/1.22.3/share/groff/1.22.3/font/devps
export VIM_SYNTAX=/usr/local/opt/vim/share/vim/vim80/syntax
