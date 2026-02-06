`applescript://` URL protocol
================================================================================
**Function:** Open in Script Editor  
**Syntax:**
<samp>applescript://com.apple.scripteditor/?<ins><var>params</var></ins></samp>

AppleScript URLs must begin with the text[^4]

	applescript://com.apple.scripteditor/

followed by [RFC 3986]-compliant query variables of the form used by regular web
addresses. Supported [parameters] are documented below; unrecognised parameters
are presumably ignored by the handler application (Script Editor).

Using no parameters is also legal, and merely instructs Script Editor to open an
empty window analogously to [`action=new`][`action`].


Parameters
-----------------------------------------------------------------------------------

<!-------------------------------------------------------------------------------->
| Name          | Value                                                           |
|:--------------|:----------------------------------------------------------------|
| [`action`]    | [`append`] \| [`insert`] \| [`new`]                             |
| [`script`]    | URL-encoded AppleScript source                                  |
| [`appid`]     | Application identifier (`kMDItemCFBundleIdentifier`)            |
| [`directory`] | Default location (created if necessary) to save new scripts to  |
<!-------------------------------------------------------------------------------->

### `action`
One of three different modes that affect the behaviour of Script Editor:

<!------------------------------------------------------------------------------------------------>
| Action                          | Description                                                   |
|:--------------------------------|:--------------------------------------------------------------|
| <a name="new">[`new`]</a>       | Create a new window in Script Editor with URL-encoded script  |
| <a name="insert">[`insert`]</a> | Insert script into frontmost script-window at insertion point |
| <a name="append">[`append`]</a> | Append script onto frontmost script-window's contents         |
<!------------------------------------------------------------------------------------------------>

### `script`
AppleScript source code to send to the script-editor. The presence of this
parameter on newer systems may trigger a security prompt requesting the user
give permission to create a new script, ostensibly rendering the [`action`]
parameter meaningless.

### `directory`
Directory that new scripts are to be saved to. If the specified disk location
doesn't exist when the editor's "Save" dialogue is opened, it will be created
automatically.

### `appid`
If present, the new script is treated as an application script for a program
with the given identifier string. The default save location will be inside a
directory created under `~/Library/Application Scripts` named after the app-ID.
For example:
			
	applescript://com.apple.scripteditor/?action=new&appid=com.apple.TextEdit
	~/Library/Application Scripts/com.apple.TextEdit/*.scpt

If a directory has also been specified, it will be interpreted as a path
relative to the aforementioned `Library` directory:
		
	applescript://com.apple.scripteditor/?action=new&appid=com.apple.TextEdit&directory=foo/bar
	~/Library/Application Scripts/com.apple.TextEdit/foo/bar/*.scpt

As with the [`directory`] parameter, nonexistent directories (along with any
intermediate directories) are created automatically when the script-editor
prompts for the user to select a location to save to.


References
-----------------------------------------------------------------------------------
Research for this document was based on the following resources, listed
chronologically in the order I originally discovered or consulted them:

1.	<details open><summary><a name="ref-1"></a><b>A MacScripter discussion on the `[applescript]` tag:</b></summary>
	
	<small><code><https://www.macscripter.net/t/how-does-the-applescript-tag-work/50214></code> (dated&nbsp;January&nbsp;2008)</small>
	</details>

2.	<details open><summary><a name="ref-2"></a><b>Apple's page on Script Editor's URL protocol:</b></summary>
	
	<small>[`www.apple.com/applescript/scripteditor/12.html`](https://web.archive.org/web/20070914113718/http://www.apple.com/applescript/scripteditor/12.html)
	(dated ~mid-2003; archived September 2007)</small>
	</details>

3.	<details open><summary><a name="ref-3"></a><b>Strings embedded in the Script Editor.app binary:</b></summary>

	Running the [`strings(1)`] command extracts raw text embedded in a binary file (in this case, the actual executable
	of the Script Editor application), Extracted strings are printed line-by-line, prefixed by their hex-encoded
	byte offsets:
	
	```console
	λ app='/System/Applications/Utilities/Script Editor.app/Contents/MacOS/Script Editor'
	λ strings - -t x -1 "$app" | grep -E -U8 '^[a-fA-F0-9]+ applescript$'
	3bce0 applescript
	3bcec com.apple.scripteditor
	3bd03 script
	3bd0a action
	3bd11 appid
	3bd17 directory
	3bd21 new
	3bd25 insert
	3bd2c append
	```
	
	Here, we filter the results down to strings that fully-match the word `applescript` case-sensitively, in its entirety,
	along with the previous and next 8 lines (which helps identify clusters of contiguous strings with *possibly* shared usage).
	
	In our case, the aforementioned strategy worked, and revealed two adjacent strings that hinted at a URL handler:
	`applescript` (the URL sheme) and `com.apple.scripteditor` (the URL's pathname).  The strings that followed were
	mentioned in sources [1] and [2], but two of them weren't: [`appid`] and [`directory`]. After some experimentation,
	these were confirmed to be undocumented parameters that influence the behaviour of Script Editor in highly-specific
	(and decidedly unhelpful) ways. Nonetheless, I felt impelled to document my findings somewhere, and was my imperative
	for penning this entire deep-deep.
	</details>

[^4]: The trailing slash after `com.apple.scripteditor` is technically optional,
	but its omission has reportedly caused portability issues with BBCode markup
	(formerly [macscripter.net][][^5]) and possibly other websites and applications.
	To ensure consistent behaviour and handling, always include the slash when
	writing your AppleScript URLs: `com.apple.scripteditor/`

[^5]: <q cite="https://web.archive.org/web/20070914113718id_/http://www.apple.com/applescript/scripteditor/12.html#:~:text=Macscripter%20Message%20Boards%20Script"><i>Some message boards,
	such as [macscripter.net], require alternate URL formats
	for embedding links in forum messages.</i></q> [2] [Apple][2], <i>op. cit.</i>


<!-- Referenced links -->
[1]:               #ref-1
[2]:               #ref-2
[3]:               #ref-3
[`append`]:        #append
[`insert`]:        #insert
[`new`]:           #new
[`action`]:        #action
[`script`]:        #script
[`appid`]:         #appid
[`directory`]:     #directory
[parameters]:      #parameters
[`strings(1)`]:    https://www.mankier.com/1/strings
[RFC 3986]:        https://www.rfc-editor.org/rfc/rfc3986.html
[macscripter.net]: https://www.macscripter.net/
