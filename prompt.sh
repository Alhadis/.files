#!/bin/sh

# Current branch of Git repository
branchName(){
	set -- "$1" "$2" "`git branch --list --no-color 2>/dev/null | grep '^\* ' | sed 's/^\* //; s/^(//; s/)$//;'`"
	[ -n "$3" ] && printf " ${1}(${2}%s${1})" "$3"
}

# Current working directory, with $HOME replaced by a tilde
cwd(){
	case $PWD in
		$HOME/*) printf '%s' "~${PWD#${HOME}}" ;;
		$HOME)   printf '~' ;;
		*)       printf '%s' "$PWD" ;;
	esac
}

# Same as `cwd()`, except only the basename is printed
cwdShort(){
	case $PWD in
		/) printf '/' ;;
		*) set -- "`cwd`"; printf '%s' "${1##*/}" ;;
	esac
}


# Spiffy-looking symbol
case `id -u` in
	0) PS1='#';;
	*) [ "$DISPLAY" ] && PS1='λ' || PS1='$';;
esac

# SGR colour sequences
case `id -u` in
	# Root/superuser: shades of gold/brass
	0) [ "$DISPLAY" ] \
	&& { error='38;5;94'; colour='38;5;142'; punct='38;5;58'; } \
	|| { error='31'       colour='33';       punct="$colour"; } ;;
	
	# Normal users: shades of green
	*) [ "$DISPLAY" ] \
	&& { error='38;5;28'; colour='38;5;10'; punct='38;5;22'; } \
	|| { error='31';      colour='32';      punct="$colour"; } ;;
esac

colour='\033['"$colour"'m'
punct='\033['"$punct"'m'
error='\033['"$error"'m'


# Keep wscons(4) free of glitter
[ ! "$DISPLAY" ] && [ "$TERM" = vt220 ] && PS1="$PS1 \w: " ||

# Dynamic prompt strings
case `basename ${0#-}` in
	# GNU Bourne-Again SHell
	bash)
		colour='\001'"$colour"'\002'
		punct='\001'"$punct"'\002'
		error='\001'"$error"'\002'
		
		PS1="${colour}\$([ \$? -eq 0 ] || printf '$error')$PS1${colour}"
		PS1="$PS1 \W\$(branchName \"${punct}\" \"${colour}\")${punct}:\001\033[0m\002 "
	;;
	
	# Z shell
	zsh)
		setopt PROMPT_SUBST
		PS1_TMP="\$(branchName \"${punct}\" \"${colour}\")"
		[ "$DISPLAY" ] \
		&& { error="${error:10:2}"; colour="${colour:10:2}"; punct="${punct:10:2}"; } \
		|| { error="${error:5:2}";  colour="${colour:5:2}";  punct="${punct:5:2}"; }; :
		PS1="%(?.%${colour}F.%${error}F)$PS1%${colour}F %1~%f${PS1_TMP}%${punct}F:%f "
		unset PS1_TMP
	;;
	
	# Policy-compliant Ordinary SHell
	posh)
		PS1="$PS1 "
	;;
	
	# Other POSIX shells: dash(1), ksh(1), mksh(1)
	*)
		PS1="\$([ \$? -eq 0 ] && printf '$colour' || printf '$error')$PS1"
		colour=`printf "$colour"`
		punct=`printf "$punct"`
		error=`printf "$error"`
		
		PS1="$PS1${colour} \$(cwdShort)"
		PS1="$PS1\$(branchName '$punct' '$colour')"
		PS1="$PS1${punct}:$(printf '\033[0m') "
	;;
esac

unset colour punct error
export PS1
