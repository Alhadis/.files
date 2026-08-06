#!/usr/bin/env osascript

property name:    "FitWindows"
property desc:    "Resize an app's windows to fill the screen"
property cmd:     "fit-windows"
property id:      "com.alhadis." & cmd
property usage:   "Usage: " & cmd & " <app-name>"
property version: "1.0.0"

script displayInfo
	property jqPath:      null
	property pixelSize:   {0, 0}
	property logicalSize: {0, 0}
	property refreshRate: 0

	-- Extract information about the device's screen-size from system_profiler(8)
	to execProfiler()
		-- Locate jq(1) binary if it's available
		if my jqPath is null then ¬
			set my jqPath to do shell script "command -v jq 2>/dev/null || :"
		
		-- Take the easy way out if jq(1) is installed
		if my jqPath's length is 0 then
			do shell script "system_profiler -json SPDisplaysDataType | jq -r '
				.SPDisplaysDataType[].spdisplays_ndrvs[]? |
				._spdisplays_pixels, ._spdisplays_resolution'"
		else
			set tmp to path to temporary items from user domain with folder creation
			set xml to tmp's POSIX Path & "tmp.SPDisplaysReporter.plist"
			
			-- Write XML property list to $TMPDIR only if needed
			if fileExists(xml) is false then ¬
				do shell script "system_profiler -xml SPDisplaysDataType > " ¬
					& xml's quoted form without altering line endings
			
			-- Use PlistBuddy(1) to print keys at presumed array indexes
			do shell script "/usr/libexec/PlistBuddy \\
				-c 'Print :0:_items:0:spdisplays_ndrvs:0:_spdisplays_pixels' \\
				-c 'Print :0:_items:0:spdisplays_ndrvs:0:_spdisplays_resolution'" ¬
				& space & xml's quoted form
		end if
	end execProfiler

	-- Return true if the specified filepath points to an existing, non-empty file
	on fileExists(fp)
		try
			if fp doesn't contain ":" then set fp to (POSIX file fp) as Text
			get eof (alias fp) is greater than 0
		on error msg number code
			false
		end try
	end fileExists

	-- Main method for retrieving info about device's display from system_profiler(8)
	on querySysConfig()
		set {px, res} to paragraphs of execProfiler()
		set tid to AppleScript's text item delimiters
		set text item delimiters to {"x", "@"}
		ignoring case and white space but considering punctuation
			set item 1 of my pixelSize   to (text item 1 of px)  as Integer
			set item 2 of my pixelSize   to (text item 2 of px)  as Integer
			set item 1 of my logicalSize to (text item 1 of res) as Integer
			set item 2 of my logicalSize to (text item 2 of res) as Integer
			repeat with txt in {px's last text item, res's last text item}
				if txt ends with "Hz"
					set txt to text 1 thru (txt's length - 2) of txt
					set my refreshRate to txt as Number
					exit
				end if
			end repeat
		end ignoring
		set text item delimiters to tid
		
		-- Prepare return value
		set rv to {pixelSize: 0, logicalSize: 0, refreshRate: 0}
		copy my pixelSize   to rv's pixelSize
		copy my logicalSize to rv's logicalSize
		copy my refreshRate to rv's refreshRate
		return rv
	end querySysConfig
	
	to screenSize()
		set info   to querySysConfig()
		set width  to 1st  item of info's logicalSize
		set height to 2nd  item of info's logicalSize
		set scaleX to (1st item of info's pixelSize) / width
		set scaleY to (2nd item of info's pixelSize) / height
		set scale  to scaleX
		if scaleX ≠ scaleY then set scale to {scaleX, scaleY}
		{width, height, scale}
	end screenSize
end script

on run argv
	-- Handle options
	repeat
		set argc to length of argv
		if argc equals 0 then exit
		set arg to the first item of argv
		if arg isn't "" and arg does not start with "-" then exit
		set argv to the rest of argv
		if arg equals "--" then exit
		if {"-h", "--help"} contains arg then
			return cmd & ": " & desc & linefeed & usage
		else if {"-v", "--version"} contains arg then
			return "v" & version
		else
			-- Unsupported option
			log cmd & ": Unsupported option: " & quote & arg & quote
			log usage
			error number -50 from arg
		end if
	end
	
	if argv's length > 0 then
		fit(argv's first item)
	else
		log usage
		error "No application name specified" number -1715
	end
end

-- Maximise every window of a currently-running application
on fit(appName)
	-- Check that named application is actually running
	if application appName isn't running then
		error number -600 from appName
	else
		set rect to {0, 0} & displayInfo's screenSize()
		tell application appName
			tell its every window
				set zoomed to true
				set bounds to rect
				set position to {0, 0}
			end tell
		end tell
		return
	end if
end fit
