#!/bin/sh

# Spiffy-looking symbol
[ "$DISPLAY" ] && PS1='λ' || PS1='$'


# Dynamic prompt strings (Bash only)
case `basename ${0#-}` in
	bash)
		# SGR colour sequences
		colour='\001\033[38;5;10m\002'
		punct='\001\033[38;5;22m\002'
		reset='\001\033[0m\002'

		# Darken sigil's colour to indicate an error code
		statusColour(){
			[ $? -ne 0 ] && printf '\001\033[38;5;28m\002'
		}

		# Current branch of Git repository
		branchName(){
			name=`git branch --list --no-color 2>/dev/null | cut -d" " -f2 | tr -d "\n"`
			[ -n "$name" ] && printf " ${1}(${2}%s${1})" "$name"
		}

		PS1="${colour}\$(statusColour)$PS1${colour}"
		PS1="$PS1 \W\$(branchName \"${punct}\" \"${colour}\")${punct}:${reset} "
		unset colour punct reset
	;;

	*)
		PS1="$PS1 \w: "
	;;
esac

export PS1
