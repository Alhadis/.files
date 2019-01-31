#!/usr/bin/osascript

(*
 * Resize Terminal.app to fill the screen.
 *)

on run argv
	-- Load explicit dimensions be specified by user
	if ((count argv) >= 2) then
		set termWidth  to item 1 of argv
		set termHeight to item 2 of argv
	
	-- Default to screen resolution
	else
		-- Make sure we haven't closed Finder before interrogating it
		if application "Finder" is running then
			tell application "Finder"
				set screenSize to bounds of window of desktop
				set termWidth  to item 3 of screenSize
				set termHeight to item 4 of screenSize
			end tell
		
		-- If we have, resort to an inelegant hack
		else
			set screenSize to do shell script "system_profiler SPDisplaysDataType | grep Resolution | grep -Eo '[0-9]+ x [0-9]+' | tr x ,"
			set AppleScript's text item delimiters to ","
			set arrayItems to every text item of the screenSize
			set AppleScript's text item delimiters to ""
			set termWidth  to item 1 of arrayItems
			set termHeight to item 2 of arrayItems
		end if
	end if

	tell window 1 of application "Terminal"
		set zoomed to true
		set bounds to {0, 0, termWidth as number, termHeight as number}
		set position to {0, 0}
	end tell
end
