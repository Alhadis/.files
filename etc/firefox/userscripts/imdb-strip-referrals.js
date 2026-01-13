// ==UserScript==
// @name         IMDB: Strip referrals
// @description  Strip referrer variables from IMDb page links
// @match        *://www.imdb.com/*
// @namespace    https://github.com/Alhadis/.files
// @downloadURL  https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/imdb-strip-referrals.js
// @require      https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/shared.js
// @author       Alhadis
// @license      ISC
// @version      1.0
// @grant        none
// ==/UserScript==
"use strict";

observe("a", el => {
	const url = new URL(el.href);
	url.searchParams.delete("ref_");
	url.searchParams.delete("rf");
	el.href = url;
}, {once: true, deep: true});
