"use strict";

/**
 * Replace unsafe filename characters with Unicode lookalikes.
 * @example filesafe("Q: So What?") === "Q꞉ So What⁇";
 * @param {String} input
 * @return {String}
 */
function filesafe(input){
	const map = {
		__proto__: null,
		":": "꞉",
		"<": "﹤",
		">": "﹥",
		"\\": "⧵",
		"/":  "∕",
		"?!": "⁈",
		"??": "⁇",
		"?":  "⁇",
	};
	for(const key in map)
		input = input.replaceAll(key, map[key]);
	return input
		.replace(/"([^"]*)"/g, "“$1”")
		.replace(/\.(.[^.]*)?$/, "․$1")
		.replace(/\.{3}/g, "…")
		.replace(/^\./, "․");
}

/**
 * Run a callback for all current and future DOM elements matching a CSS selector.
 *
 * @example <caption>Remove tracking variable from links</caption>
 *    observe("a[href]", el => {
 *        const url = new URL(el.href);
 *        url.searchParams.delete("ref_");
 *        el.href = url;
 *    }, {once: true});
 *
 * @param {String}       selector - A well-formed CSS selector
 * @param {Function}     callback - Function that receives element as its sole argument
 * @param {Object}      [options] - Settings that refine observation behaviour
 * @param {Boolean}     [options.deep=false] - Target matching elements in attached subdocuments
 * @param {Boolean}     [options.once=false] - Fire callback no more than once per element
 * @param {HTMLElement} [options.root=document.documentElement] - Root node to observe
 * @return {MutationObserver}
 */
function observe(selector, callback, {once = false, deep = false, root = document.documentElement} = {}){
	const refs = new WeakSet();
	const fn = el => {
		try{
			if(once){
				if(refs.has(el)) return;
				refs.add(el);
			}
			callback(el);
		}
		catch(e){
			console.error(e);
		}
	};
	const docs = [root.ownerDocument];
	if(deep) for(const iframe of root.getElementsByTagName("iframe")){
		try{ docs.push(iframe.contentWindow.document); }
		catch(e){ console.error("Unable to access contents of iframe", e); }
	}
	for(const document of docs)
	for(const node of document.querySelectorAll(selector))
		fn(node);
	const observer = new MutationObserver(changes => {
		for(const {addedNodes} of changes)
		for(const node of addedNodes){
			if(Node.ELEMENT_NODE !== node.nodeType) continue;
			if(node.matches(selector)) fn(node);
			else for(const el of node.querySelectorAll(selector)) fn(el);
		}
	});
	observer.observe(root, {childList: true, subtree: true});
	return observer;
}
