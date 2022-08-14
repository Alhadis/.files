// Latest SHA1
javascript:(_=>{const D=document,L=D.location,P=L.pathname.split("/");P[4]=D.querySelector(`a[href^="/${P[1]}/${P[2]}/commit/"][data-pjax]`).href.match(/\/commit\/([^/]+)/)[1];prompt("COPY+PASTA:",L.origin+P.join("/"))})();

// Edit
javascript:void(function(){var b=document.body.parentNode;var a=b.attributes;var n="contentEditable";if(a.getNamedItem(n))a.removeNamedItem(n);else b.contentEditable=true}());

// Blank
data:text/html;charset=UTF-8,<html contenteditable%3Dtrue><style>:focus{outline:none}</style>

// Nuke CSS
javascript:void(_=>{const D=document,S=D.styleSheets;while(S.length)S[0].ownerNode.remove();for(const E of D.querySelectorAll("[style]"))E.removeAttribute("style")})();

// Nuke JS
javascript:Function.prototype.call=undefined;

// Search IMDb
javascript:void(x=>x && (window.location = `https://${screen.width <= 900 ? "m." : ""}imdb.com/find?q=${encodeURIComponent(x)}`))(window.prompt("Search for a movie on IMDb:"));

// SVG/PNG comparison mode
javascript:void(l=>{if("file:"!==l.location.protocol||"image/"!==l.contentType.slice(0,6))return;const{documentElement:e,body:t}=l,n=t.firstElementChild;e.style.all=t.style.all=n.style.all="unset",window.onbeforeunload=()=>"Nah"})(document);
