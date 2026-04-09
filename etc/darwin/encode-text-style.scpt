#!/usr/bin/env osascript -sho
use AppleScript version "2.8"
use scripting additions
use framework "Foundation"
use framework "AppKit"

on run(argv)
	if first item of argv = "font"
		set n to argv's second item
		set s to argv's third item
		encode(current application's NSFont's fontWithName_size_(n, s))
	else if first item of argv = "colour"
		set {r, g, b, a} to rgba(items 2 thru end of argv)
		encode(current application's NSColor's colorWithRed:r green:g blue:b alpha:a)
	else
		error "First argument must be font or colour"
	end if
end

on encode(obj)
	set encoded to (current application's NSArchiver's archivedDataWithRootObject:obj)
	set totalSize to encoded's |length|()
	set hex to ""
	set tid to text item delimiters
	set text item delimiters to {"= 0x", "}"}
	repeat with i from 0 to totalSize by 16
		set range to {location: i, |length|: 16}
		if i + 16 > totalSize then set range's |length| to (totalSize - i)
		set desc to description of (encoded's subdataWithRange:range) as Text
		set hex to hex & (second text item of desc)
	end repeat
	set text item delimiters to tid
	return hex
end encode

on decode(str)
	set str to run script "«data rdat" & str & "»"
	set str to current application's NSData's dataWithData:str
	current application's NSPropertyListSerialization's ¬
		dataWithPropertyList:( ¬
			"<plist version='1.0'><dict><key>value</key><data>" & ¬
				(str's base64EncodedStringWithOptions:()) & ¬
			"</data></dict></plist>") ¬
		format:(current application's NSPropertyListXMLFormat_v1_0) ¬
		options:0 ¬
		|error|:(missing value)
end decode

to stringify(object)
	if the object's class = text then return the object
	set tids to my text item delimiters
	try
		set s to {_:object} as null
	on error e
		set my text item delimiters to "Can’t make {_:"
		set s to text items 2 thru -1 of e as text
		set my text item delimiters to "} into type null."
		set s to text items 1 thru -2 of s as text
		set my text item delimiters to tids
	end try
	s
end stringify

on rgba(values)
	if length of values < 4 then set values to values & {1.0}
	repeat with i from 1 to (count values)
		set n to item i of values
		try
			if n's class = text
				if n contains "."
					set n to n as Real
				else
					set n to (n as Integer) / 255
				end if
			end if
			if n > 1.0 then set n to 1.0
			if n < 0.0 then set n to 0.0
		end try
		set item i of values to n
	end repeat
	values
end rgba
