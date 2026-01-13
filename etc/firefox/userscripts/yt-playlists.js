// ==UserScript==
// @name         YouTube: Strip playlist variables
// @description  Disassociate videos from playlists so watch-histories aren't clobbered
// @match        *://www.youtube.com/*
// @namespace    https://github.com/Alhadis/.files
// @downloadURL  https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/yt-playlists.js
// @require      https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/shared.js
// @author       Alhadis
// @license      ISC
// @version      1.0
// @grant        none
// ==/UserScript==
"use strict";

observe("ytd-playlist-video-list-renderer a[href^='/watch?v=']", el => {
	const url = new URL(el.href);
	for(const attr of "list index pp".split(" "))
		url.searchParams.delete(attr);
	el.search = url.search;
}, {once: true});
