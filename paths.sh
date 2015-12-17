# Commonly-accessed directories
export F=~/.files
export JOBS=~/Jobs
export LABS=~/Labs
export SNIP=~/Labs/Snippets

# Stuff I can never seem to memorise
export APACHE_DIR=/private/etc/apache2
export APACHE_LOGS=/private/var/log/apache2
export PHP_INI=$(php -i | grep 'Loaded Configuration File' | sed -E 's/\s*=>\s*/:/g' | cut -d : -f 2)

# Stuff I can only memorise halfway
export SNIPS=$SNIP
export SNIPPETS=$SNIP
