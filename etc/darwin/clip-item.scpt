#!/usr/bin/env osascript

(*
 * CLI for AppleScript's clipboard commands.
 *)

on run argv
	-- Print the contents of a particular clipboard entry
	if the length of argv is greater than 0
	
		-- Determine whether to suppress post-processing of binary formats
		set kRaw to false
		if argv's length ≥ 2 and first item of argv is "-p" then
			set kRaw to true
			set argv to rest of argv
		end if
		
		set i to the first item of argv
		set entry to (i + 1)th item of (clipboard info)
		set {kType, kSize} to entry
		
		-- Determine if content-type can be displayed unobfuscated
		if kRaw or kType is contained by { ¬
			«class utf8»,        ¬
			«class ut16»,        ¬
			String,              ¬
			Styled Unicode text, ¬
			Unicode text         ¬
		} then return the clipboard as kType
		
		-- Convert “furls” to ordinary POSIX paths
		if kType is «class furl» ¬
			then return POSIX path of file (the clipboard as kType)
		
		-- Convert hex-encoded data into raw binary
		do shell script "osascript -e 'the clipboard as " & kType & "' |
			LANG=C sed -E 's/^«data ....//; s/(0[Aa])?»//;' | xxd -p -r" ¬
			altering line endings false
	
	-- List what contents are available
	else
		set i to 0
		set output to ""
		repeat with entry in clipboard info
			set {kType, kSize} to entry
			if i ≥ 1 then set output to output & linefeed
			set output to output & "[" & i & "] " & kType & " (" & kSize & " byte"
			if kSize ≠ 1 then set output to output & "s"
			set i to (i + 1)
			set output to output & ")"
		end repeat
		output
	end if
end
