// ==UserScript==
// @name         W3 Validator: Default to file upload
// @description  Switches the validation mode to file-upload
// @match        *://validator.w3.org/*
// @namespace    https://github.com/Alhadis/.files
// @downloadURL  https://raw.github.com/Alhadis/.files/HEAD/etc/firefox/userscripts/w3-validator.js
// @author       Alhadis
// @license      ISC
// @version      1.0
// @grant        none
// ==/UserScript==
"use strict";

// Nu HTML checker only supports web-hosted uploads…
if(location.pathname.startsWith("/nu/")){
	const [form] = document.forms;
	form.showsource .checked = true;
	form.showoutline.checked = true;
	form.docselect.value = "textarea";
	form.doc.value ="";
	form.doc.focus();
}

// Old-style validator
else if("/" === location.pathname){
	document.querySelector("li:has(> a[href='#validate_by_upload'])")?.click();
	document.getElementById("uploaded_file").focus();
	const more = document.querySelector("#extra_opt_upload legend.toggletext");
	more.classList.contains("toggled") || more.firstElementChild.click();
	setTimeout(() => {
		for(const mode of ["uri", "upload", "input"]){
			const form = document.querySelector(`#validate-by-${mode} form`);
			if(form.fbc)
				form.fbc.checked = true;
			form.ss.checked      = true;
			form.fbd.checked     = true;
			form.No200.checked   = true;
			form.outline.checked = true;
			form.verbose.checked = true;
		}
	}, 10);
}
