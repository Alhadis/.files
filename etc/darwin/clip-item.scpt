#!/usr/bin/env osascript

(*
 * CLI for AppleScript's clipboard commands.
 *)

on run argv
	-- Print the contents of a particular clipboard entry
	-- TODO: Implement hex-to-binary conversion for proper RTF and HTML output
	if the length of argv is greater than 0
		set i to the first item of argv
		set info  to clipboard info
		set entry to (i + 1)th item of info
		set type to first item of entry
		log type
		log the clipboard as type
	
	-- List what contents are available
	else
		set i to 0
		repeat with entry in clipboard info
			set {kType, kSize} to entry
			set output to "[" & i & "] " & kType & " (" & kSize & " byte"
			if kSize ≠ 1 then set output to output & "s"
			set i to (i + 1)
			log output & ")"
		end repeat
	end if
end
