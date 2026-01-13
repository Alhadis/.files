// ==UserScript==
// @name         Apple: Reset sidebar menu state
// @description  Ensure links in sidebar navigation are collapsed upon next visit
// @match        *://developer.apple.com/library/archive/documentation/*
// @namespace    https://github.com/Alhadis/.files
// @downloadURL  https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/apple-docs.js
// @author       Alhadis
// @license      ISC
// @version      1.0
// @grant        none
// @top-level-await
// ==/UserScript==
"use strict";

// Don't retain sidebar disclosure states between visits
window.addEventListener("beforeunload", () => {
	window.localStorage.clear();
});

// Wait for sidebar-menu to be populated
const toc = document.getElementById("toc");
toc.querySelector("li") || (await new Promise(fn => {
	const observer = new MutationObserver(changes => {
		for(const {addedNodes} of changes)
		for(const node of addedNodes){
			if(Node.ELEMENT_NODE !== node.nodeType) continue;
			if(node instanceof HTMLLIElement)
				return fn(observer);
			else for(const el of node.getElementsByTagName("li"))
				return fn(observer);
		}
	});
	observer.observe(toc, {childList: true});
})).disconnect();

// Expand current page in sidebar menu
const page = location.hash.slice(1);
if(page){
	const active = toc.querySelector(`li[data-aref="${page}"]`);
	let link = active;
	do{
		link.classList.add("open");
		console.log(link);
	} while(link = link.parentNode.closest("li.children:has(> .disclosure)"));
}
