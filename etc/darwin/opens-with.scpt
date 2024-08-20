#!/usr/bin/env osascript

(*
 * Print application that Finder would use to open the given file.
 *)

on run argv
	if argv's length < 1 then error "Missing required argument"

	set kPath to first item of argv
	try
		-- Silence gripes about relative POSIX paths being "scheme-less URLs"
		if kPath doesn't start with "/"
			if kPath starts with "./" then set kPath to text 3 thru (kPath's length) of kPath
			set kPath to (do shell script "pwd") & "/" & kPath
		end if

		set result to POSIX path of (get default application of (info for POSIX file kPath))
		if result ends with "/" then set result to text 1 thru (result's length - 1) of result
		result
	on error msg number code from src partial result frag to cast
		-- Pathname doesn't point to an existing file
		if code equals -43
			error ("No such file: " & kPath) number -43

		-- Filetype isn't recognised by LaunchServices(8)
		else if code equals -1728
			error ("No application knows how to open " & kPath) number -10814

		-- Something else we didn't account for
		else
			error msg number code from src partial result frag to cast
		end if
	end try
end
