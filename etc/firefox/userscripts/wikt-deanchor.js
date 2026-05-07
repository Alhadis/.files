// ==UserScript==
// @name         Wiktionary: Trim “#English” from definition links
// @description  Removes the fragment identifier for links to English-language definitions
// @match        *://*.wiktionary.org/wiki/*
// @namespace    https://github.com/Alhadis/.files
// @downloadURL  https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/wikt-deanchor.js
// @author       Alhadis
// @license      ISC
// @version      1.0
// @grant        none
// ==/UserScript==
"use strict";

const engHash = str => /^#(?:English|en(?:-\w+)*)$/i.test(str ?? "");
if(engHash(document.location.hash)){
	const url = document.location.toString().replace(/#.*$/, "");
	document.location.replace(url);
}

else for(const el of document.links){
	const url = new URL(el.href);
	if(engHash(url.hash)){
		url.hash = "";
		el.href = url;
	}
}
