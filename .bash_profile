# Shell options
shopt -s nocaseglob;    # Case-insensitive globbing
shopt -s autocd;        # DOS-style directory navigation
shopt -s globstar;      # Enable recursive globbing
shopt -s dotglob;       # Enable .file globbing

[ -r ~/.profile ] && . ~/.profile
case "$-" in *i*) [ -r ~/.bashrc ] && . ~/.bashrc;; esac
