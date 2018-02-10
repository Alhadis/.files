# Shell options
shopt -s nocaseglob;    # Case-insensitive globbing
shopt -s autocd;        # DOS-style directory navigation
shopt -s globstar;      # Enable recursive globbing
shopt -s dotglob;       # Enable .file globbing
shopt -s checkwinsize;  # Update $LINES/$COLUMNS on each command

if [ -r ~/.profile ]; then
	. ~/.profile
fi
