// Latest SHA1
javascript:(_=>{const D=document,L=D.location,P=L.pathname.split("/");P[4]=D.querySelector(`a[href^="/${P[1]}/${P[2]}/commit/"][data-pjax]`).href.match(/\/commit\/([^/]+)/)[1];prompt("COPY+PASTA:",L.origin+P.join("/"))})();

// Edit
javascript:void(function(){var b=document.body.parentNode;var a=b.attributes;var n="contentEditable";if(a.getNamedItem(n))a.removeNamedItem(n);else b.contentEditable=true}());

// Blank
data:text/html;charset=UTF-8,<p></p><style>img{max-width:200px}</img>

// Nuke CSS
javascript:void(_=>{const D=document,S=D.styleSheets;while(S.length)S[0].ownerNode.remove();for(const E of D.querySelectorAll("[style]"))E.removeAttribute("style")})();

// \x09{4}
javascript:void!function(e){"use strict";function t(e,o=""){const n=[];for(const c of e.childNodes)switch(c.nodeType){case Node.TEXT_NODE:n.push(c);break;case Node.ELEMENT_NODE:o&&c.matches(o)||n.push(...t(c))}return n}const o=[".highlight > pre > .line span:first-child:not(:only-child)","table.highlight td.blob-num + .blob-code",".add-line-comment ~ .blob-code-inner","pre > code"];(function(){const{body:e}=document,t=e.querySelectorAll("*"),o=e=>!!/^pre(?:-wrap)?$/i.test(window.getComputedStyle(e).whiteSpace);return!e.childElementCount&&e.textContent&&o(e)||1===t.length&&o(t[0])})()&&o.push("body, body *");const n=(e=>{if(o=document.documentElement.style,(t=e[0].toLowerCase()+e.slice(1))in o)return t;for(var t,o,n=((n="Webkit Moz Ms O Khtml").toLowerCase()+n).split(" "),c=0;c<10;++c)if((t=n[c]+e)in o)return t;return""})("TabSize");for(const e of document.querySelectorAll("[data-tab-size]"))e.dataset.tabSize=e.style[n]=4;document.body.style[n]=4,function(e,o=2){const n=new RegExp(`(^|\\n)((?:${" ".repeat(o)})+)`,"g");for(const r of document.querySelectorAll(e))for(const e of t(r)){const{data:t}=e,r=(c=t,c.replace(/&nbsp;|&#0*160;|&#x0*A0;|\xA0/g," ")).replace(n,(e,t,n)=>t+"\t".repeat(n.length/o));r!==t&&(e.data=r)}var c}(o.join(", "))}();
