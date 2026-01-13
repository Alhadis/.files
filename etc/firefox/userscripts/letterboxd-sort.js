// ==UserScript==
// @name         Letterboxd: Order films by rating-date
// @description  Sort account's films list by most-recently by default
// @match        *://letterboxd.com/*
// @namespace    https://github.com/Alhadis/.files
// @downloadURL  https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/letterboxd-sort.js
// @require      https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/shared.js
// @author       Alhadis
// @license      ISC
// @version      1.0
// @grant        none
// ==/UserScript==
"use strict";

observe('a[href="/Alhadis/films/" i]', el => el.href += "by/rated-date/");
