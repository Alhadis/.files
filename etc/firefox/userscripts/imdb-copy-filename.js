// ==UserScript==
// @name         IMDB: Copy movie filename
// @description  Copy movie's title and year to the clipboard
// @match        *://www.imdb.com/title/*
// @namespace    https://github.com/Alhadis/.files
// @downloadURL  https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/imdb-copy-filename.js
// @require      https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/shared.js
// @author       Alhadis
// @license      ISC
// @version      1.0
// @grant        GM_setClipboard
// ==/UserScript==
"use strict";

// Failsafe to allow execution outside GreaseMonkey-like environments
if("function" !== typeof GM_setClipboard){
	const {clipboard} = navigator;
	globalThis.GM_setClipboard = clipboard.writeText.bind(clipboard);
}

const objID = location.pathname.split("/")[2];
const title = document.querySelector("h1");
title.style.cursor = "pointer";
title.addEventListener("pointerenter", () => title.style.color = "#ccc");
title.addEventListener("pointerleave", () => title.style.removeProperty("color"));
title.addEventListener("pointerdown",  event => {
	let text = filesafe(title.textContent.trim());
	let year = document.querySelectorAll(`a[href="https://www.imdb.com/title/${objID}/releaseinfo/" i]`);
	if(year = [...year].find(el => /^\d{4}$/.test(el.textContent)))
		text += ` (${year.textContent.trim()})`;
	GM_setClipboard(text);
	event.preventDefault();
});
