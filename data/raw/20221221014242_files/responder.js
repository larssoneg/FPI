var _____WB$wombat$assign$function_____ = function(name) {return (self._wb_wombat && self._wb_wombat.local_init && self._wb_wombat.local_init(name)) || self[name]; };
if (!self.__WB_pmw) { self.__WB_pmw = function(obj) { this.__WB_source = obj; return this; } }
{
  let window = _____WB$wombat$assign$function_____("window");
  let self = _____WB$wombat$assign$function_____("self");
  let document = _____WB$wombat$assign$function_____("document");
  let location = _____WB$wombat$assign$function_____("location");
  let top = _____WB$wombat$assign$function_____("top");
  let parent = _____WB$wombat$assign$function_____("parent");
  let frames = _____WB$wombat$assign$function_____("frames");
  let opener = _____WB$wombat$assign$function_____("opener");

(()=>{"use strict";var e={742:(e,t)=>{for(var n=[],o=[],r="undefined"!==typeof Uint8Array?Uint8Array:Array,a="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",c=0,u=a.length;c<u;++c)n[c]=a[c],o[a.charCodeAt(c)]=c;function i(e){var t=e.length;if(t%4>0)throw new Error("Invalid string. Length must be a multiple of 4");var n=e.indexOf("=");return-1===n&&(n=t),[n,n===t?0:4-n%4]}function s(e,t,o){for(var r,a,c=[],u=t;u<o;u+=3)r=(e[u]<<16&16711680)+(e[u+1]<<8&65280)+(255&e[u+2]),c.push(n[(a=r)>>18&63]+n[a>>12&63]+n[a>>6&63]+n[63&a]);return c.join("")}o["-".charCodeAt(0)]=62,o["_".charCodeAt(0)]=63}},t={};function n(o){var r=t[o];if(void 0!==r)return r.exports;var a=t[o]={exports:{}};return e[o](a,a.exports,n),a.exports}(()=>{const e={ERROR:5,WARN:4,INFO:3,DEBUG:2,LOG:1,OFF:100};let t=e.INFO,o=!1;const r={ERROR:"color:#FC0C1B;background-color:#FFEFEF",WARN:"color:#5B3A06;background-color:#FFFBE6",INFO:"color:#363F47;background-color:#e1e9ff",DEBUG:"color:#444;background-color:#eee",LOG:"color:#000;background-color:transparent",DEFAULT:"color:#000;background-color:transparent;font-weight:",ONEID:"color:#2887b4;background-color:transparent;font-weight:bold"};let a=u();function c(n){return t>e[n]?(e="",t={})=>{}:o?function(e){const t=["%c[OneID]%c %c[%s]%c: ",r.ONEID,r.DEFAULT,r[e],e,r.DEFAULT];return window.console[e.toLowerCase()].bind(window.console,...t)}(n):function(e){return window.console[e.toLowerCase()].bind(window.console,"[OneID] [%s]: ",e)}(n)}function u(){return{error:c("ERROR"),warn:c("WARN"),info:c("INFO"),debug:c("DEBUG"),log:c("LOG")}}const i=["CALL","CALL_DONE","EVENT"];function s(){const e="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_=+";let t="";for(let n=0;n<24;n+=1){t+=e[Math.floor(Math.random()*e.length)]}return t}function d(e,t="DEFAULT"){const n=`PatchBay[${t}]`,o={},r={},c={};return Object.keys(e).forEach((t=>{e[t].onmessage=function(e){return t=>{if(!t||!t.data||"object"!==typeof t.data)return;const{data:n}=t;-1!==i.indexOf(n.type)&&(u(n.name,e,n),setTimeout(d.bind(null,n.name)))}}(t)})),{addHandler:function(e,t){if("function"!==typeof t)return;o[e]=t,d(e)},message:f,broadcast:function(t,n){const o={id:s(),type:"EVENT",data:n,name:t};u(t,null,o),setTimeout(d.bind(null,t)),Object.keys(e).forEach((e=>{f(e,t,n)}))},call:function(e,t,o){return new Promise(((r,u)=>{const i=s();a.debug(`${n}: call() invoked, target=${e}, name=${t}, id=${i}`),c[i]={resolve:r,reject:u},g(e,t,"CALL",o,i)}))}};function u(e,t,o){a.debug(`${n}: queueMessage(name=${e}, source=${t})`),r[e]||(r[e]=[]),r[e].push({source:t,data:o})}function d(e){if(a.debug(`${n}: flushQueue(name=${e})`),!Array.isArray(r[e])||r[e].length<1)return void a.debug(`${n}: Nothing in queue for '${e}', exit early`);const t=[];for(;r[e].length;){const{source:u,data:i}=r[e].pop(),s=i&&i.type||void 0,d=i&&i.id||void 0,f=`${n} - ${d||"NO_ID"}`;if(a.debug(`${f}: flushQueue loop, source=${u}, type=${s}`),"CALL"===s||"EVENT"===s){let n;if(!o[e]){a.debug(`${f}: flushQueue loop, NO HANDLER FOR name=${e}, deferring`),t.push({source:u,data:i});continue}a.debug(`${f}: flushQueue loop, about to call handler for name=${e}`),n=o[e](i&&i.data),a.debug(`${f}: flushQueue loop, obtained return value from name=${e}`),"CALL"===s&&(n&&n.then||(a.debug(`${f}: flushQueue loop, handler for name=${e} returned plain value`),n=Promise.resolve(n)),n.then((t=>{a.debug(`${f}: flushQueue, promise resolved for name=${e}, messaging back`),l(u,e,t,d,"resolved")})).catch((t=>{a.debug(`${f}: flushQueue, promise caught error for name=${e}, messaging back`),l(u,e,t,d,"rejected")})))}if("CALL_DONE"===s){const{status:e}=i;if(!c[d]){a.debug(`${f}: flushQueue loop, pendingCalls EMPTY FOR id=${d}!!!`);continue}"resolved"===e?(a.debug(`${f}: flushQueue loop, resolving promise for id=${d}`),c[d].resolve(i.data)):(a.debug(`${f}: flushQueue loop, rejecting promise for id=${d}`),c[d].reject(i.data)),delete c[d]}}for(const o of t)a.debug(`${n}: flushQueue, pushing deferred item back into queue, name=${e}`),r[e].push(o)}function l(e,t,n,o,r){g(e,t,"CALL_DONE",n,o,r)}function f(e,t,n){g(e,t,"EVENT",n)}function g(o,r,c,i,l,f){if(l=l||s(),o===t)return a.debug(`${n}: Target is self...`),u(r,o,{id:l,type:c,name:r,data:i,status:f}),void setTimeout(d.bind(null,r));e[o]?e[o].postMessage({id:l,type:c,name:r,data:i,status:f}):a.debug(`${n}: Target '${o}' has not been registered!!!`)}}const l=(f="responder",new Promise((e=>{const t={};window.addEventListener("message",(function n(o){o.data&&o.data.targetName===f&&("comm-bootstrap-start"===o.data.event&&o.source.postMessage({event:"comm-bootstrap-ack",targetName:f},"*"),"comm-bootstrap-data"===o.data.event&&(t[o.data.port]=o.ports[0],Object.keys(t).length>=o.data.ports.length&&(window.removeEventListener("message",n),e(t))))}))}))).then((e=>new d(e,"responder")));var f;n(742);function g(e=""){if("string"!==typeof e)return 0;let t=-1;const n=function(){const e=new Array(256);for(let t=0;t<e.length;t++){let n=t;for(let e=0;e<8;e++)n=1&n?3988292384^n>>>1:n>>>1;e[t]=n>>>0}return e}();for(let o=0;o<e.length;o++)t=t>>>8^n[255&(t^e.charCodeAt(o))];return(-1^t)>>>0}const $="ac",p="ab2",h="idn",m="rt",w="token",b="ts",v=/^\.?www/,y=/^\./;function E(e){return e&&document.cookie.replace(new RegExp(`(?:(?:^|.*;\\s*)${e}\\s*\\=\\s*([^;]*).*$)|^.*$`),"$1")||null}function k(e,t,n){if(document.cookie=`${encodeURIComponent(e)}=; expires=Thu, 01 Jan 1970 00:00:00 GMT${t?`; domain=${t}`:""}${n?`; path=${n}`:""}`,t&&!v.test(t)){const o=`www${/^\./.test(t)?"":"."}${t}`;a.debug(`also deleting cookie from ${o}`),k(e,o,n)}return!0}function L(e){const t=function(){let e=window.location.hostname;return"localhost"===e?e:(e=e.replace(v,"").replace(y,""),`.${e}`)}();a.debug("responder: guestLogout");[p,h,m,w,b].forEach((n=>{const o=`${e}.${n}`;a.debug(`deleting cookie: ${o}`),k(o,t,"/")})),a.debug(`deleting cookie: ${e}-${$}`),k(`${e}-${$}`,t,"/")}function O(e){if(!e)return null;const t=localStorage.getItem(e);try{const n=JSON.parse(t);if(!n._expiry)return n;const o=new Date(n._expiry);if(delete n._expiry,o>new Date)return n;localStorage.removeItem(e)}catch(e){return t}}function A({key:e,value:t,expires:n}){const o=JSON.stringify({...t,_expiry:n},function(){const e=new Map;return(t,n)=>n?"object"!==typeof n?n:e.has(n)?void 0:(e.set(n,!0),n):n}());localStorage.setItem(e,o)}const D=18e5,N=9e5;function R(e){if(!e)return null;return E(`device_${g(e).toString(16)}`)}function I({deviceId:e,loginValue:t}){if(!e||!t)return null;const n=g(t),o=new Date;o.setMonth(o.getMonth()+6),T({key:`device_${n.toString(16)}`,value:e,expires:o,secure:!0})}function x(){const e=new URLSearchParams(window.location.search);return E(`${e.get("clientId")}-${e.get("environment")}.${p}`)}function F(){const e=new URLSearchParams(window.location.search),t=e.get("clientId"),n=e.get("environment"),o=new Date(Date.now()+D);T({key:`${t}-${n}.${p}`,value:o.toISOString(),expires:o,secure:!0})}function T({key:e,value:t,expires:n,secure:o=!1}){const r=window.location.hostname;!function(e,t,n,o,r,c){let u,i=!1;if(v.test(n)&&(n=n.replace(v,""),i=!0),!e||/^(?:expires|max-age|path|domain|secure)$/i.test(e))return!1;if(u=`${e}=${t}`,r&&r instanceof Date&&(u+=`; expires=${r.toUTCString()}`),n&&(u+=`; domain=${n}`),o&&(u+=`; path=${o}`),c&&(u+="; secure"),document.cookie=u,i){const t=`www${/^\./.test(n)?"":"."}${n}`;a.debug(`calling delete on domain ${t}`),k(e,t,o||"")}}(e,t,"localhost"===r?r:`.${r}`,"/",n,o)}function C({recoveryToken:e}){const t=new URLSearchParams(window.location.search),n=t.get("clientId"),o=t.get("environment"),r=new Date(Date.now()+N);T({key:`${n}-${o}.${m}`,value:e,expires:r,secure:!0})}function S(){const e=new URLSearchParams(window.location.search);return E(`${e.get("clientId")}-${e.get("environment")}.${m}`)}l.then((e=>{e.addHandler("deviceID.read",R),e.addHandler("deviceID.write",I),e.addHandler("ageBandGating.read",x),e.addHandler("ageBandGating.write",F),e.addHandler("otpRecoveryToken.write",C),e.addHandler("otpRecoveryToken.read",S),e.addHandler("get.apiKey",E),e.addHandler("get.localStorage",O),e.addHandler("set.localStorage",A),e.addHandler("get.cookie",E),e.addHandler("set.cookie",T),e.addHandler("delete.cookie",k),e.addHandler("guest.logout",L)}))})()})();

}
/*
     FILE ARCHIVED ON 01:31:09 Dec 21, 2022 AND RETRIEVED FROM THE
     INTERNET ARCHIVE ON 01:24:32 Jul 21, 2023.
     JAVASCRIPT APPENDED BY WAYBACK MACHINE, COPYRIGHT INTERNET ARCHIVE.

     ALL OTHER CONTENT MAY ALSO BE PROTECTED BY COPYRIGHT (17 U.S.C.
     SECTION 108(a)(3)).
*/
/*
playback timings (ms):
  captures_list: 195.413
  exclusion.robots: 0.08
  exclusion.robots.policy: 0.072
  cdx.remote: 0.05
  esindex: 0.008
  LoadShardBlock: 37.12 (3)
  PetaboxLoader3.datanode: 48.086 (4)
  load_resource: 187.781
  PetaboxLoader3.resolve: 146.5
*/