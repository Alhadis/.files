// ==UserScript==
// @name         Auto-redirect to Wikimedia Commons
// @description  Redirect localised file-pages to their canonical URL on Wikimedia Commons.
// @author       Alhadis
// @license      ISC
// @version      1.0
// @grant        GM_getValue
// @namespace    https://github.com/Alhadis/.files
// @downloadURL  https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/mw-redirect.js
//
// @comment      Wikimedia Foundation sites sourced from https://mediawiki.org/wiki/Sites_using_MediaWiki/Wikimedia
// @match        *://*.mediawiki.org/wiki/*
// @match        *://*.wikibooks.org/wiki/*
// @match        *://*.wikidata.org/wiki/*
// @match        *://*.wikifunctions.org/wiki/*
// @match        *://*.wikimedia.org/wiki/*
// @match        *://*.wikinews.org/wiki/*
// @match        *://*.wikipedia.org/wiki/*
// @match        *://*.wikiquote.org/wiki/*
// @match        *://*.wikisource.org/wiki/*
// @match        *://*.wikiversity.org/wiki/*
// @match        *://*.wikivoyage.org/wiki/*
// @match        *://*.wiktionary.org/wiki/*
//
// @comment      Third-party wikis using Commons-sourced media: https://mediawiki.org/wiki/Sites_using_InstantCommons
// @match        *://*.capoeirawiki.org/wiki/*
// @match        *://*.deurnewiki.nl/wiki/*
// @match        *://*.ennstalwiki.at/wiki/*
// @match        *://*.enviwiki.cz/wiki/*
// @match        *://*.fahrradmonteur.de/*
// @match        *://*.fandom.com/wiki/*
// @match        *://*.inklupedia.de/wiki/*
// @match        *://*.justapedia.org/wiki/*
// @match        *://*.landscapefor.eu/wiki/*
// @match        *://*.lgbthistoryuk.org/wiki/*
// @match        *://*.librepathology.org/wiki/*
// @match        *://*.librewiki.net/wiki/*
// @match        *://*.lsj.gr/wiki/*
// @match        *://*.mdwiki.org/wiki/*
// @match        *://*.miraheze.org/wiki/*
// @match        *://*.oesterreichwiki.org/wiki/*
// @match        *://*.olsbergwiki.de/*
// @match        *://*.openstreetmap.org/wiki/*
// @match        *://*.plantae.se/index.php/*
// @match        *://*.rationalwiki.org/wiki/*
// @match        *://*.rhein-neckar-wiki.de/*
// @match        *://*.secret-wiki.de/wiki/*
// @match        *://*.seminaverbi.bibleget.io/wiki/*
// @match        *://*.setesdalswiki.no/wiki/*
// @match        *://*.shoutwiki.com/wiki/*
// @match        *://*.spiele.j-crew.de/wiki/*
// @match        *://*.translatewiki.net/wiki/*
// @match        *://*.tuepedia.de/wiki/*
// @match        *://*.uncyclopedia.co/wiki/*
// @match        *://*.urbipedia.org/hoja/*
// @match        *://*.vikidia.org/wiki/*
// @match        *://*.vragwiki.dk/wiki/*
// @match        *://*.wiki.sn.at/wiki/*
// @match        *://*.wikichip.org/wiki/*
// @match        *://*.wikieducator.org/*
// @match        *://*.wikilectures.eu/w/*
// @match        *://*.wikipedalia.com/index.php/*
// @match        *://*.wikiskripta.eu/w/*
// @match        *://*.wikitrek.org/wiki/*
// ==/UserScript==
(function(){
	"use strict";

	/**
	 * Resolve the Commons-hosted file-page to redirect to.
	 *
	 * If the file only exists locally, or the localised page was navigated
	 * to from a Commons-hosted page (such as via a browesr's “Back” button),
	 * then `null` is returned.
	 *
	 * @returns {?URL}
	 * @public
	 */
	function getCommonsRedirect(){
		if(document.querySelector("#mw-content-text.mw-body-content #file")){
			var canonLink = document.querySelector("link[rel=canonical][href]");
			var referrer = document.referrer ? new URL(document.referrer) : null;
			if(canonLink && canonLink.href && !(referrer && "commons.wikimedia.org" === referrer.hostname))
				return new URL(canonLink.href);
		}
		return null;
	}

	/**
	 * Execute a Commons redirection, if one exists.
	 *
	 * @param {Boolean} [appendHistory=false]
	 *    If true, redirection creates a new entry in the browser's history.
	 *    By default, the current page's entry is replaced by the redirection target.
	 *    This reduces clutter in the user's navigation history, but it also makes it
	 *    impossible to navigate back to the localised page, if need be.
	 * @return {void}
	 * @public
	 */
	function doCommonsRedirect(appendHistory){
		var redirect = getCommonsRedirect();
		if(redirect && redirect.hostname !== location.hostname){
			window.stop();
			appendHistory
				? location.assign(redirect)
				: location.replace(redirect);
		}
	}

	var append = false;
	if("function" === typeof GM_getValue)
		append = !!GM_getValue("appendHistory", false);
	doCommonsRedirect(append);
})();
