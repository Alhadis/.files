require "irb/completion"
require "date"
require "fileutils"
require "json"
require "pathname"
require "pp"
require "uri"
require "yaml"

# Prompt string decoration
RED = ENV["DISPLAY"] ? "\e[38;5;124m" : "\e[31m"
PS1 = "#{RED}>>\e[0m  "
PS2 = "#{RED}? \e[0m  "
IRB.conf[:PROMPT][:SIMPLE] = {
	AUTO_INDENT: true,
	PROMPT_I:    PS1,
	PROMPT_C:    PS1,
	PROMPT_S:    PS2,
	RETURN:      "%s\n",
}

IRB.conf[:PROMPT_MODE]        = :SIMPLE
IRB.conf[:AUTO_INDENT]        = true
IRB.conf[:INSPECT_MODE]       = true
IRB.conf[:SAVE_HISTORY]       = false
IRB.conf[:IGNORE_SIGINT]      = false
IRB.conf[:USE_COLORIZE]       = true
IRB.conf[:USE_AUTOCOMPLETE]   = false
IRB.conf[:ECHO_ON_ASSIGNMENT] = true
