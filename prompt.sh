# Prompt-line customisation

colour='\001\033[38;5;10m\002'
punct='\001\033[38;5;22m\002'
reset='\001\033[0m\002'

# Dim the λ's colour if last command exited with a non-zero status
status-colour(){
	[ $? -ne 0 ] && printf '\001\033[38;5;28m\002'
}

# Print the current branch name
print-branch(){
	local c1=$(tput setaf 22)
	local c2=$(tput setaf 10)
	local name=$(printf '%s\(%s\\1%s\)' $c1 $c2 $c1)
	git branch --list 2>/dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/ ${name}/"
}

PS1="${colour}\$(status-colour)λ${colour} "
PS1+="\W\$(print-branch)${punct}:${reset} "
export PS1
unset colour punct reset
