# Prompt-line customisation
colour='\033[38;5;10m'


# Print the current branch name
print-branch(){
	local c1=$(tput setaf 22)
	local c2=$(tput setaf 10)
	local name=$(printf '%s\(%s\\1%s\)' $c1 $c2 $c1)
	git branch --list 2>/dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/ ${name}/"
}

export PS1="\[${colour}\]λ \W\[\$(print-branch)\033[38;5;22m:\033[0m\] "
