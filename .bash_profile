# Shell options
{
	shopt -s autocd         # DOS-style directory navigation
	shopt -s cdable_vars    # Switch to $VAR when running `cd VAR`
	shopt -s checkwinsize   # Update $LINES/$COLUMNS on each command
	shopt -s dotglob        # Enable .file globbing
	shopt -s globstar       # Enable recursive globbing
	shopt -s nocaseglob     # Case-insensitive globbing
	shopt -u hostcomplete   # Allow tab-completion of @-prefixed files

	# Stop asking to display ~3,340 possibilities, fuck sake
	shopt -s no_empty_cmd_completion

} 2>/dev/null

# Omit duplicate entries from command history
export HISTCONTROL=ignoredups:ignorespace

if [ -r ~/.profile ]; then
	. ~/.profile
fi
