(()=>{var e={p:""};(()=>{"use strict";e.p=document.getElementById("webpack-public-path").innerText+"Js/"})(),StackExchange=window.StackExchange=window.StackExchange||{},StackOverflow=window.StackOverflow=window.StackOverflow||{},StackExchange.mathjaxEditing=function(){var e,n,t,a,r,c,o,u,i,l=!1,s=null,p="$",h=MathJax.Hub;function f(){i.disabled=!0,u.resetEquationNumbers()}function g(){i.disabled=!1}h.Queue((function(){if(u=MathJax.InputJax.TeX,i=u.config.noErrors,l=!0,h.processUpdateTime=50,h.processSectionDelay=0,MathJax.Extension["fast-preview"].Disable(),h.Config({"HTML-CSS":{EqnChunk:10,EqnChunkFactor:1},CommonHTML:{EqnChunk:10,EqnChunkFactor:1},SVG:{EqnChunk:10,EqnChunkFactor:1}}),s)return C(s,"Typeset")}));var x,T=/(\$\$?|\\(?:begin|end)\{[a-z]*\*?\}|\\[\\{}$]|[{}]|(?:\n\s*)+|@@\d+@@|`+)/i;function d(r,u){var i=e.slice(r,u+1).join("").replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;");for(c&&(i=i.replace(/\n    /g,"\n")),h.Browser.isMSIE&&(i=i.replace(/(%[^\n]*)\n/g,"$1<br/>\n"));u>r;)e[u--]="";e[r]="@@"+o.length+"@@",o.push(i),n=t=a=null}function v(u){n=t=a=c=null,o=[];for(var i=1,l=(e=x(u.replace(/\r\n?/g,"\n"),T)).length;i<l;i+=2){var s=e[i];"@"===s.charAt(0)?(e[i]="@@"+o.length+"@@",o.push(s)):n?s===t?r>0?a=i:0===r?d(n,i):n=t=a=null:s.match(/\n.*\n/)||i+2>=l?(a&&(i=a,r>=0&&d(n,i)),n=t=a=null,r=0):"{"===s&&r>=0?r++:"}"===s&&r>0&&r--:s===p||"$$"===s?(n=i,t=s,r=0):"begin"===s.substr(1,5)?(n=i,t="\\end"+s.substr(6),r=0):"`"===s.charAt(0)?(n=a=i,t=s,r=-1):"\n"===s.charAt(0)&&s.match(/    $/)&&(c=!0)}return a&&d(n,a),e.join("")}function m(e){return e=e.replace(/@@(\d+)@@/g,(function(e,n){return o[n]})),o=null,e}function C(e,n){s=!1,h.cancelTypeset=!1,h.Queue(f,[n,h,e],g)}function E(e){return p.startsWith("\\")?e:e.replace(/\\\$/g,"\\\\$")}return x=3==="aba".split(/(b)/).length?function(e,n){return e.split(n)}:function(e,n){var t,a=[];if(!n.global){var r=n.toString(),c="";r=r.replace(/^\/(.*)\/([im]*)$/,(function(e,n,t){return c=t,n})),n=new RegExp(r,c+"g")}n.lastIndex=0;for(var o=0;t=n.exec(e);)a.push(e.substring(o,t.index)),a.push.apply(a,t.slice(1)),o=t.index+t[0].length;return a.push(e.substring(o)),a},{prepareWmdForMathJax:function(e,n,t){var a=document.getElementById("wmd-preview"+n);p=t[0][0];var r=e.getConverter();r.hooks.chain("preConversion",v),r.hooks.chain("preConversion",E),r.hooks.chain("preSafe",m),e.hooks.chain("onPreviewRefresh",(function(){!function(e,n){s||(s=e,l&&(h.Cancel(),h.Queue([C,e,n])))}(a,"Typeset")})),h.Queue((function(){a&&a.querySelector(".mjx-noError")&&C(a,"Reprocess")}))}}}(),function(){var e=MathJax.Hub;if(!e.Cancel){e.cancelTypeset=!1;var n="MathJax Canceled";e.Register.StartupHook("HTML-CSS Jax Config",(function(){var t=MathJax.OutputJax["HTML-CSS"],a=t.Translate;t.Augment({Translate:function(r,c){if(e.cancelTypeset||c.cancelled)throw Error(n);return a.call(t,r,c)}})})),e.Register.StartupHook("SVG Jax Config",(function(){var t=MathJax.OutputJax.SVG,a=t.Translate;t.Augment({Translate:function(r,c){if(e.cancelTypeset||c.cancelled)throw Error(n);return a.call(t,r,c)}})})),e.Register.StartupHook("CommonHTML Jax Config",(function(){var t=MathJax.OutputJax.CommonHTML,a=t.Translate;t.Augment({Translate:function(r,c){if(e.cancelTypeset||c.cancelled)throw Error(n);return a.call(t,r,c)}})})),e.Register.StartupHook("PreviewHTML Jax Config",(function(){var t=MathJax.OutputJax.PreviewHTML,a=t.Translate;t.Augment({Translate:function(r,c){if(e.cancelTypeset||c.cancelled)throw Error(n);return a.call(t,r,c)}})})),e.Register.StartupHook("TeX Jax Config",(function(){var t=MathJax.InputJax.TeX,a=t.Translate;t.Augment({Translate:function(r,c){if(e.cancelTypeset||c.cancelled)throw Error(n);return a.call(t,r,c)}})}));var t=e.processError;e.processError=function(a,r,c){return a.message!==n?t.call(e,a,r,c):(MathJax.Message.Clear(0,0),r.jaxIDs=[],r.jax={},r.scripts=[],r.i=r.j=0,r.cancelled=!0,null)},e.Cancel=function(){this.cancelTypeset=!0}}}()})();