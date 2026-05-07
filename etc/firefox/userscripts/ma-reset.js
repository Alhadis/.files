// ==UserScript==
// @name         Metal Archives: Reset search type
// @description  Reset the search-form to "Band name" on pages that aren't search results
// @match        *://www.metal-archives.com/*
// @namespace    https://github.com/Alhadis/.files
// @downloadURL  https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/ma-reset.js
// @require      https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/shared.js
// @run-at       document-start
// @author       Alhadis
// @license      ISC
// @version      1.0
// @grant        none
// ==/UserScript==
"use strict";

const observer = observe('form#search_form button[type="submit"]', el => {
	const {type} = el.form;
	const url = new URL(document.location);
	if("/search" !== url.pathname || type.value !== url.searchParams.get("type"))
		type.value = type.options[0].value;
	observer.disconnect();
});
document.addEventListener("readystatechange", () => {
	if("loading" !== document.readyState)
		observer.disconnect();
});
