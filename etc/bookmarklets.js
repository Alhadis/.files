// Latest SHA1
javascript:(_=>{const D=document,L=D.location,P=L.pathname.split("/");P[4]=D.querySelector(`a[href^="/${P[1]}/${P[2]}/commit/"][data-pjax]`).href.match(/\/commit\/([^/]+)/)[1];prompt("COPY+PASTA:",L.origin+P.join("/"))})();

// Edit
javascript:void(function(){var b=document.body.parentNode;var a=b.attributes;var n="contentEditable";if(a.getNamedItem(n))a.removeNamedItem(n);else b.contentEditable=true}());

// Blank
data:text/html;charset=UTF-8,<p></p><style>img{max-width:200px}</img>

// Nuke CSS
javascript:void(_=>{const D=document,S=D.styleSheets;while(S.length)S[0].ownerNode.remove();for(const E of D.querySelectorAll("[style]"))E.removeAttribute("style")})();

// Search IMDb
javascript:void(x=>x && (window.location = `https://${screen.width <= 900 ? "m." : ""}imdb.com/find?q=${encodeURIComponent(x)}`))(window.prompt("Search for a movie on IMDb:"));
