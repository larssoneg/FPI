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

!function(){var e="",n="createElement",s="concat",t="exports",i="translate",o="./node_modules/@fitt/core/fitt/index.js",r="label",a="emptyObj",c="name",l="fpi",u="info",p="length",d="div",f="conference",m="./node_modules/react/index.js",g="stats",w="./node_modules/prop-types/index.js",h="RNK",v="EFF",y="string",b="season",x="view",j="description",S="-",_="desc",N="propTypes",T="push",k="FPI",A="category",I="siteName",L="state",C="mix",E="displayName",D="title",F="league",P="sort",W=" ",O="dictionary",R="parseValue",V="isTrend",H="isLink",M=".",G="./node_modules/classnames/index.js",B="object",K="selected",z="defaults",U="emptyAry",J="W-L-T",$="efficiencies",q="unshift",X="base100",Y="removeInteger",Q="sortingStat",Z="inherits",ee="apply",ne="prototype",se="params",te="team",ie="handleRedirect",oe="props",re="fpirank",ae="numties",ce="numlosses",le="numwins",ue="./src/pagetypes/fpi/config/utils.js",pe="getData",de="hideSort",fe="sortDirection",me="completeConfig",ge="routing",we="decimals",he="secondIndex",ve="index",ye="sport",be="noop",xe="isConferenceSelected",je="super",Se="helpers",_e="struc",Ne="isRequired",Te="DIR_DESC",ke="DIR_ASC",Ae="paramValues",Ie="./src/node_modules/espn-api/constants.js",Le="type",Ce="isNum",Ee="down",De="isMobile",Fe="value",Pe="link",We="split",Oe="stat",Re="fpi.fpi",Ve="span",He="./src/node_modules/espn-common-constants/index.js",Me="Fragment",Ge="group",Be="isFunc",Ke="skipFormat",ze="fpi.projections.note",Ue="projections",Je="efficiencies.totefficiency",$e="SPECIAL TEAMS",qe="stefficiencyrank",Xe="stefficiency",Ye="DEFENSE",Qe="defefficiencyrank",Ze="defefficiency",en="OFFENSE",nn="offefficiencyrank",sn="offefficiency",tn="OVERALL",on="totefficiencyrank",rn="totefficiency",an="avgingamewprank",cn="REM SOS",ln="sosremainingrank",un="avgsosrank",pn="probwintitle",dn="probmaketitlegame",fn="PLAYOFF%",mn="probmakeplayoffs",gn="WIN DIV%",wn="probwindiv",hn="projectedl",vn="PROJ W-L",yn="projectedw",bn="POWER INDEX",xn="rankchange7days",jn="Efficiencies",Sn="fpi.projections.note.extend",_n="./src/pagetypes/fpi/config sync recursive ^\\.\\/.*\\.js$",Nn="./src/pagetypes/fpi/config/index.js",Tn="parseStatsList",kn="showGroup",An="sortingCategory",In="parseDynamicSubheaders",Ln="parseSingleSubheader",Cn="thirdIndex",En="thirdName",Dn="secondName",Fn="getConfigByLeague",Pn="categories",Wn="./node_modules/lodash/get.js",On="hasSign",Rn="./src/node_modules/espn-powerIndex-parser/index.js",Vn="./src/node_modules/espn-parser/index.js",Hn="./node_modules/lodash/capitalize.js",Mn="getSitename",Gn="paramValue",Bn="parsedResponse",Kn="./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/index.js",zn="./src/node_modules/espn-metadata/espnMetadata.js",Un="className",Jn="./src/components/ui/AnchorLink/AnchorLink.jsx",$n="./node_modules/@fitt/ui-espn/common/Icon/Icon.jsx",qn="subheaders",Xn="headers",Yn="direction",Qn="./node_modules/lodash/map.js",Zn="./node_modules/lodash/findIndex.js",es="./node_modules/lodash/concat.js",ns="./src/node_modules/espn-sport-types/index.js",ss="strReplace",ts="contextTypes",is="noteWithLink",os="conferences",rs="dir/:dir",as="sort/:sort",cs="group/:group",ls="season/:season",us="view/:view",ps=":pageType",ds=":league",fs="./src/pagetypes/helpers/redirect.js",ms="webpackJsonp";(window[ms]=window[ms]||[])[T]([[l],{"./src/components/fpi/Filters.jsx":function(e,s,i){"use strict";var r,c=i(m),l=i("./node_modules/create-react-class/index.js"),u=i(w),g=i("./node_modules/@fitt/ui-espn/common/Dropdown/DropdownList.jsx"),h=i(o),v=i("./node_modules/@fitt/ui-espn/common/Tabs/index.jsx").Tabs,y=i(fs).Options,x=h.cstr,j=h[U],S=h[a],_=h[Be],N=[ds,ps,"_",us,ls,cs,as,rs];r=l({displayName:"Filters",defaultProps:{lists:{},defaults:{}},propTypes:{lists:u[B]},redirectConference:function(e){var n,s=this[oe]||S,t=s[ie],i=s[z]||S,o=x(i[f]);n=new y(Ge,N,{group:o}),_(t)&&t(n,e)},redirectSeason:function(e){var n,s=this[oe]||S,t=s[ie],i=s[z]||S,o=x(i[b]);n=new y(b,N,{season:o}),_(t)&&t(n,e)},render:function(){var e=this[oe]||S,s=e.lists||S,t=s[os]||j,i=s.views||j,o=s.seasons||j,r=e[K]||S,a=r.viewIdx,l=r[f],u=r[b];return c[n](c[Me],null,c[n](v,{data:i,activeTab:a,dynamic:!0,className:"mt5 mb3"}),c[n](d,{className:"flex flex-wrap"},o[p]?c[n](g,{list:o,defaultValue:u,onChange:this.redirectSeason,className:"mt3 mr2"}):null,t[p]?c[n](g,{list:t,defaultValue:l,onChange:this.redirectConference,className:"mt3 mr2"}):null))}}),e[t]=r},"./src/components/fpi/Footer.jsx":function(i,c,l){"use strict";var u=l(m),f=l(o),g=l("./node_modules/@fitt/ui-espn/common/AnchorLink/index.jsx"),h=u[Me],v=l(w),b=l(G),x=l(He),j=f[U],S=f[a],_=l("./node_modules/@fitt/ui-espn/common/Glossary/index.jsx"),T=l("./src/components/ui/Layout/GridColumn.jsx"),k=l("./src/components/ui/Layout/GridLayout.jsx"),A=x.THEMES.DARK;function I(t,i){var o=t.notes||S,a=o.noteByView||e,c=o[is]||S,l=c[r]||e,f=c.href||e,m=t[O]||j,w=m[p],v=(i&&i.theme)===A,y=b({"brdr-clr-gray-08":!v,"brdr-clr-heavy-metal":v}),x=b({"clr-gray-05":!v,"clr-boulder":v});return u[n](h,null,u[n](d,{className:e[s](y," n8 mt5 ")[s](x," pt5 bt")},u[n](d,{className:"footer__selectionNotes mb3"},a),f?u[n](g,{to:f,className:"h8"},l):null),w?u[n](k,{is_9_3:!0,className:"mt4"},u[n](T,{col1:!0},u[n](d,{className:e[s](y," bb")},u[n](Ve,{className:"n8 db pr3 "[s](x)})),u[n](_,{data:m,withWindowResize:!0,fullWidth:!0}))):null)}I[ts]={theme:v[y]},I[N]={},i[t]=I},"./src/components/fpi/Table.jsx":function(i,l,u){"use strict";(function(l){var h=u(m),v=u(o),y=u(w),b=v[U],_=v[a],N=v.first,T=v.cstr,k=v[ss],A=u("./node_modules/mobx/lib/mobx.js").toJS,I=u("./src/components/ui/common/withConfig.js"),L=u(G),C=u(ns).getSportIDBySportAbbrev,E=u(es),O=u("./node_modules/lodash/compact.js"),R=u(Zn),z=u(Qn),J=u("./node_modules/lodash/filter.js"),$=u("./node_modules/@fitt/ui-espn/common/Table/index.jsx"),q=$.FixedTable,X=$.Table__Heading,Y=$.Table__Row,Q=u("./src/components/fpi/common/HeaderLink.jsx"),Z=u("./src/components/fpi/common/StatCell.jsx"),ee=u("./src/components/fpi/common/TeamCell.jsx"),ne=u("./src/components/fpi/Trend/index.jsx"),se=u("./node_modules/@fitt/ui-espn/common/NoDataAvailable/index.jsx");function ie(t,i,o,r,a){var l=a||_;return z(t,(function(t){var a,u,p=t||_,d=p[te]||_,m=d.id||"-1",w=p[f]||e,v=d.uid||e,y=function(s){return z(s,(function(s){var t=s||_,i=t[c]||e,o=t[V],r=t[Fe],a=o&&"0"===r?"--":r;return o?h[n](ne,{key:i,trend:a,align:"trend"}):h[n](Z,{key:i,value:a})}))}(p[g]||b),x=!1,j=v?"team-"[s](v):e,S=v?"conf-"[s](v):e,N=l[te]&&h[n](ee,{key:j,team:d,isMobile:i}),T=l[f]&&!i&&h[n](Z,{key:S,value:w}),k=O([N,T]);try{x=o.isFollowingTeam(m,r)}catch(e){}return u=L({"is-favorite":x}),a=E(k,y),h[n](Y,{className:u},a)}))}function oe(t){var i=t||_,o=i[De],a=i.table||_,l=i[K]||_,u=l[F],f="college-football"===u,m=a[D]||e,w=a[Xn]||b,v=a.statics||b,y=A(w.colgroups,!1)||b,I=y,L=function(e,s){return z(e,(function(e,t){var i=e||W,o=s[t]||1;return h[n](X,{key:t,className:"tc",colSpan:o},i)}))}(w[Xn]||b,I)||b,E=w[qn]||b,O=J(w[qn],(function(e){return e&&"CONF"!==e[r]}))||b,V=o&&f?O:E,G=ie(a[g]||b,o,i.fanData,T(C(u)),v)||b,B=function(t,i){var o=(i||_)[P]||e,a=o[Yn]||"desc",l=(o[Oe]||e)[We](M)[1]||"fpi";return z(t,(function(e){var t=e||_,i=t[Ge],o=i?k(i,W,S):"none",u="group-"[s](o);return h[n](Q,{className:u,curSorting:l,curDirection:a,type:t[c],text:t[r],isLink:t[H],desc:t[j],link:t[Pe]})}))}(V,l),U=((l[P]||_)[Oe]||"fpi.fpi")[We](M)[1]||"fpi",$=R(V,[c,U])+1,Y=N(y),Z=i[x]||"fpi",ee="mt4 Table2__title--remove-capitalization FPI__Table--"[s](Z," league-")[s](u);return h[n](d,null,G[p]?h[n](q,{className:ee,title:m,colgroups:I,headings:L,subheaders:B,rows:G,fixedLeft:Y,sortedCol:$,shouldScroll:!0}):h[n](d,{className:"mt5"},h[n](se,{content:"No Data Available"})))}oe[ts]={translate:y.func,fanData:y[B]},i[t]=l.env.TEST_CONTEXT?oe:I(oe)}).call(this,u("./node_modules/process/browser.js"))},"./src/components/fpi/Trend/index.jsx":function(s,i,r){"use strict";var a=r(m),c=r(o),l=r(G),u=r(w),p=r($n);function f(s){var t=s.trend,i=s.up||e,o=s[Ee]||e,r=s.pos||e,u=s.align||"trend tc",f=c.cnum(t),m=c[Ce](f),g=Math.abs(f),w=m&&f>0||!1,h=e,v=m?g:t,y=w?"arrow__up":"arrow__down",b=r?y+"__"+r:e,x=l(u,{positive:m&&w,negative:m&&!w});return i&&o&&(h=w?i:o),a[n](d,{className:x,"aria-labelledby":b},m?a[n](p,{icon:y,title:h}):null,v)}r("./src/components/fpi/Trend/styles.css"),f[N]={trend:u[y],customStyles:u[B]},s[t]=f},"./src/components/fpi/Trend/styles.css":function(e,n,s){},"./src/components/fpi/common/ArrowIcon.jsx":function(e,i,o){"use strict";var r=o(m),a=o(w),c=o($n);function l(e){return r[n](c,{className:"ArrowIcon__TableSort",icon:"caret__"[s](e[Le])})}l[N]={type:a.oneOf(["up",Ee])},e[t]=l},"./src/components/fpi/common/HeaderLink.jsx":function(e,s,i){"use strict";var r=i(m),c=i(w),l=i(G),u=i(Ie)[Ae],p=u[ke],f=u[Te],g=i(o)[a],h=i(Jn),v=i("./src/components/fpi/common/ArrowIcon.jsx");function b(e){var s=e||g,t=s.curSorting,i=s.curDirection,o=s[Le],a=s.text,c=s[H],u=s[_],m=s.styles,w=s[Pe],y=l(s[Un],{underline:c}),b=function(e,s,t){var i;s===e&&(t===p&&(i=r[n](v,{type:"up"})),t===f&&(i=r[n](v,{type:Ee})));return i}(t,o,i);return r[n](Ve,{className:y,title:u,style:m},c&&w?r[n](h,{classes:"clr-gray-01",to:w},a,b):r[n](d,null,a))}b[N]={curSorting:c[y],curDirection:c[y],islink:c.bool,type:c[y],text:c[y][Ne]},b[E]="HeaderLink",e[t]=b},"./src/components/fpi/common/StatCell.jsx":function(e,s,i){"use strict";var o=i(m),r=i(w),a=i(G);function c(e){var s=e[Un],t=e[Fe],i=e.hasColor,r=a(s,{"clr-negative":i&&t<0,"clr-positive":i&&t>=0});return o[n](d,{className:r},t)}c[N]={className:r[y],value:r[y][Ne]},e[t]=c},"./src/components/fpi/common/TeamCell.jsx":function(e,s,i){"use strict";var r=i(m),c=i(w),l=i(Jn),u=i("./src/components/ui/Logo/CombinerLogo.jsx"),p=i(o)[a];function f(e,s,t){var i=e;return s&&(i=r[n](l,{to:s,"data-clubhouse-uid":t},e)),i}function g(e){var s=e||p,t=s[te]||p,i=s[De],o=t.links,a=t.logo,c=t[E],l=t.abbrev,m=t.uid,g=i?l:c,w=a?function(e,s){return r[n](u,{src:e,name:s,height:20,width:20,size:"sm"})}(a,c):null,h=w?f(w,o):null,v=f(g,o,m);return r[n](d,{className:"flex items-start mr7"},r[n](Ve,{className:"pr3 TeamLink__Logo"},h),v)}g[N]={team:c[B][Ne],isMobile:c.bool},g.defaultProps={team:{}},e[t]=g},"./src/components/fpi/index.jsx":function(s,i,r){"use strict";var c=r(m),u=r(w),p=r("./src/components/common/PageHeading.jsx"),d=r(o),f=r("./node_modules/lodash/values.js"),g=r("./src/components/fpi/Filters.jsx"),h=r("./src/components/fpi/Table.jsx"),v=r("./src/components/fpi/Footer.jsx"),y=c[Me],b=d[a];function j(s){var t=s||b,i=t[De],o=t[l]||b,r=t[se]||b,a=o.filters,u=o[z]||b,d=o[K]||b,m=o.pageHeading||"Football Power Index",w=t[ie],j=o.table||b,S=t.guestProfile,_=t.isWebview,N=o.notes,T=f(o[O])||[],k=r[x]||e;return c[n](y,null,c[n](p,{title:m,className:"mb5",isWebview:_}),c[n](g,{lists:a,defaults:u,selected:d,handleRedirect:w}),c[n](h,{table:j,selected:d,isMobile:i,guestProfile:S,view:k}),c[n](v,{params:r,isWebview:_,dictionary:T,notes:N}))}r("./src/components/fpi/styles.css"),j[N]={content:u[B]},s[t]=j},"./src/components/fpi/styles.css":function(e,n,s){},"./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/college-football.js":function(n,s,c){"use strict";var l=c(zn),p=c(Kn),d=c(o),m=d[a],g=d[_e],w=d[C];function h(){p[ee](this,arguments)}h.prototype={getTitle:function(){var e=this[u]||m,n=e[b],s=g(e[I]),t=e[f]||0,o={key:"meta.tite.powerIndex.ncaaf",season:n,siteName:s},r=e[xe]?w({},[o,{conference:t}]):o;return this[i](r)},getDescription:function(){var e=this[u]||m,n=g(e[I]),s=e[f],t={key:"meta.description.powerIndex.ncaaf",siteName:n},o=e[xe]?w({},[t,{conference:s}]):t;return this[i](o)},getKeywords:function(){var e=this[u]||m,n=g(e[I]),s=e[f],t={key:"meta.keywords.powerIndex.ncaaf",siteName:n},o=e[xe]?w({},[t,{conference:s}]):t;return this[i](o)},get:function n(){var s=this,t=s[Se]||m,i=s[L]||m,o=s[Bn]||m,a=(o[K]||m)[b]||e,c=(o.metadata||m)[f]||m,l="80"!==(c[Gn]||e),p=c[r]||e,d=t[Mn](i)||e;return s[u]=w(s[u],{season:a,siteName:d,conference:p,isConferenceSelected:l}),s[je](n)()}},l[Z](h,p),n[t]=h},"./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/index.js":function(n,s,r){"use strict";var c=r(zn),l=r(Hn),p=r(o),d=p[a],f=p[_e],m=p[C],g=p[be];function w(){c[ee](this,arguments)}w[ne]={getTitle:function(){var n=this[u]||d,s={key:"meta.title.powerIndex",league:n[F]||e,sport:n[ye]||e,season:n[b]||e,siteName:n[I]||e};return this[i](s)},getDescription:function(){var n=this[u]||d,s={key:"meta.description.powerIndex",league:this[i](n[F])||e,siteName:n[I]||"espn"};return this[i](s)},getKeywords:function(){var n=this[u]||d,s={key:"meta.keywords.powerIndex",league:n[F]||e,sport:n[ye]||e,siteName:n[I]||"espn"};return this[i](s)},getAnalytics:function(){var e=this[L]||d;return((this[Se]||d).getSportAnalytics||g)(e)},get:function n(){var s=this,t=s[Se]||d,o=s[L]||d,r=s[i](f(t.getLeagueName(o))),a=l(t.getSportName(o)),c=((s[Bn]||d)[K]||d)[b]||e,p=f(t[Mn](o))||e;return s[u]=m(s[u],{league:r,sport:a,season:c,siteName:p}),s[je](n)()}},c[Z](w,c),n[t]=w},"./src/node_modules/espn-powerIndex-parser/college-football.js":function(n,c,l){"use strict";var u=l(Vn),p=l(Rn),d=l(o),f=d[C],m=d[a],g=d[U],w=d[Ce];function h(){p[ee](this,arguments)}var v={getPageHeading:function(n,s,t,o){var a={key:"pageTitle.powerIndex.ncaaf",season:t},c=o||m,l=c[r]||e,u="80"!==(c[Gn]||e)?f({},[a,{conference:l}]):a;return this[i](u)},parseTableTitle:function(e,n){return this[i]({key:"tableTitle.fpi",view:e,conference:n})},parseSingleStat:function(n,t){var i=n||m,o=t||m,r=o[ve],a=o[he],c=o[Q],l=o[we]||0,u=o[Y],p=o[On],d=o[V],f=o[X],h=(i.totals||g)[r],v=h&&"-"!==h?h:"--",y=i.values||g,b=y[r],x=w(a)?y[a]:null,j=w(b)?this[R](b,l,u,p,f):v,_=w(x)&&this[R](x,l,u,p),N=_?e[s](j,S)[s](_):j;return{name:c||e,value:N,isTrend:d}}};h[ne]=v,u[Z](h,p),n[t]=h},"./src/node_modules/espn-powerIndex-parser/index.js":function(n,l,u){"use strict";var d,m=u(Vn),w=u(Ie),h=u(o),v=u(He),y=u("./src/node_modules/espn-common-modules-parser/modules/urlCreation.js").handleUrlCreation,_=u("./node_modules/lodash/bind.js"),N=u(es),k=u("./node_modules/lodash/find.js"),I=u(Zn),G=u(Wn),B=u(Qn),K=u("./node_modules/lodash/reduce.js"),J=u("./node_modules/lodash/remove.js"),$=u(Hn),ie=h[a],oe=h[U],re=u("./node_modules/lodash/isEqual.js"),ae=h[Be],ce=h[Ce],le=h.isObj,ue=h.isAry,pe=h[C],be=h[_e],xe=h[ss],Se=h.idxOf,Ne=h.forEach,Le=v.LANGUAGE,Ee=w[Ae],De=Ee[Te],Re=Ee[ke],Ve=Le.EN,Me=[ds,ps,"_",us,ls,cs,as,rs];function Ke(){m[ee](this,arguments),this[O]={}}d={parseView:function(e,n){var s=((this[L]||ie)[ge]||ie)[se]||ie,t=pe({},[s,{sort:null,dir:null}]),o=n||ie,r=o.param;return t[x]=r,{title:this[i](o.text),url:y(Me,t,e)}},parseViews:function(e,n){var s=this;return B(e,(function(e){return e&&s.parseView(n,e)}))},parseConferenceItem:function(e){var n=e||ie,s=n.shortName||n[c],t=n.groupId;return{value:t,label:s,paramValue:t}},parseConferencesList:function(e,n,t){var o=this[i]("Conferences"),r=n===Ve?"All "[s](o):o,a=B(e,this.parseConferenceItem);return a[p]&&a[q]({value:t,label:r,paramValue:t}),a},getPageHeading:function(e,n,s){var t={key:"pageTitle.powerIndex",league:be(e),sport:$(n),season:s};return this[i](t)},parse:function n(s){var t,o=this,a=o[je](n)(s)||ie,c=o.getRoutingParam(x),l=G(a,"FPI[0]",ie),u=G(a,"seasonsDropdownV3[0].seasons",oe),p=G(a,"conferences[0].conferences",oe),d=l.teams||oe,m=l.currentValues||ie,g=l[Pn]||oe,w=l.currentSeason||ie,h=m[F],v=m[ye],y=m[f],j=m.lang,S=m[P]||ie,_=m[b]||e,N=(o.config||ie)[Fn],T=ae(N)?N(h):ie,A=T[z]||ie,L=T.views||ie,C=T.fpiHeaders||ie,W=T.staticColumns||ie,R=A[f],V=A[x],H=c||V,M=(A[P]||ie)[H]||ie,K=w.year,U=C[H],$=I(L,(function(e){return e&&e.param===H})),q=(J(t=p,{groupId:"80"}),t),X=o.parseViews(L,{view:V}),Y=G(X,[$,D],e),Q=o.parseConferencesList(q,j,R),Z=k(Q,(function(e){return e&&e[Fe]===y}))||ie,ee=Z[r]||e,ne=o.getPageHeading(h,v,_,Z),se=o.parseTableTitle(Y,ee),te=o[me](g,U),re=o.parseHeaders(te,M,S,W),ce=o.parseTeamsStatsList(d,te,g,W)||ie,le=B(u||oe,(function(n){var s=n||ie,t=s.year;return{value:t,label:s[E]||e,paramValue:t}})),ue=$>=0?L[$]:ie,pe=o[i](ue.note),de=j===Ve?ue[is]:ie;return{metadata:{conference:Z},filters:{views:X,conferences:Q,seasons:le},defaults:{conference:R,season:K},selected:{viewIdx:$,conference:y,sort:S,season:_,league:h},pageHeading:ne,responses:s,table:{headers:re,stats:ce,title:se,statics:W},dictionary:o[O],notes:{noteByView:pe,noteWithLink:de}}},completeConfig:function(e,n){var s=n||{},t=s[A],i=t&&k(e,(function(e){return e&&e[c]===t})),o=s[g];return s[g]=B(o,i?_(this.completeConfigStat,this,i):_(this[me],this,e)),s},completeConfigStat:function(n,s){var t,o=le(s)?s:{name:s},a=o[c],l=o[Dn],u=o[En],p=o[E],d="differential"===n[c],f=n.names||oe,m=Se(f,a),g=n.displayNames||oe,w=n.descriptions||oe,h=g[m],v=w[m],y=p&&this[i](p)||h;return o[ve]=m,o[Q]=d?xe(a,/Differential$/,e):a,o[E]=y,o[j]=o[j]?o[j]:v,o[r]||(t=(n.labels||oe)[m]||e,o[r]=d?xe(t,/Diff$/,e):t),l&&(o[he]=Se(f,l)),u&&(o[Cn]=Se(f,u)),o},parseHeaders:function(e,n,s,t){var o=this,a=[],l=[],u=B(t,(function(e){var t=e||ie,a={label:t[r]&&o[i](t[r]),description:t[j]&&o[i](t[j]),isLink:t[H]||!1,name:t[c],sortDirection:t[fe]},l=t[A];return t.parse?_(o[Ln],o,l,n,s,null,a)():a})),d=o[In](e,n,a,l,s),f=N(u,d);return!a[p]&&a[T](d[p]),a[q](u[p]),l[p]&&l[q](W),{colgroups:a,headers:l,subheaders:f}},parseDynamicSubheaders:function(e,n,s,t,o){var r,a=this,c=e[An]||e[A],l=e[g]||oe,u=e[D];return r=c?B(l,_(a[Ln],a,c,n,o,e)):K(l,(function(e,i){return N(e,a[In](i,n,s,t,o))}),[]),u&&(s[T](l[p]),t[T](a[i](u))),r},parseTeamsStatsList:function(e,n,s,t){var i=this,o=n&&n[g];return Ne(t,(function(e){var n=e||ie;if(n.parse){var t=i[me](s,{category:n[A],stats:[n]});ue(o,!0)&&o[q](t)}})),B(e,_(i.parseSingleTeamStats,i,n))},parseSingleTeamStats:function(n,s){var t=s||ie,i=t[te]||ie,o=G(i,"group.shortName")||e,r=t[Pn]||oe;return{team:this.parseTeam(i),stats:this[Tn](n,r),conference:o}},parseStatsList:function(e,n){var s=this,t=e[A],i=e[g]||oe,o=t&&k(n,(function(e){return(e||ie)[c]===t}));return t?B(i,_(s.parseSingleStat,s,o)):K(i,(function(e,t){return N(e,s[Tn](t,n))}),[])},parseSingleStat:function(n,t){var i=n||ie,o=t||ie,r=o[ve],a=o[he],c=o[Cn],l=o[Q],u=o[we]||0,p=o[Y],d=o[On],f=o[V],m=o[X],g=(i.totals||oe)[r],w=g&&"-"!==g?g:"--",h=i.values||oe,v=h[r],y=ce(a)?h[a]:null,b=ce(c)?h[c]:null,x=ce(v)?this[R](v,u,p,d,m):w,j=ce(y)&&this[R](y,u,p,d),_=ce(b)&&this[R](b,u,p,d),N=j&&_?e[s](x,S)[s](j,S)[s](_):j?e[s](x,S)[s](j):x;return{name:l||e,value:N,isTrend:f}},parseValue:function(e,n,t,i){var o=e;return o=new Intl.NumberFormat(Ve,{minimumFractionDigits:n,maximumFractionDigits:n}).format(o),t&&(o=M[s](o[We](M)[1])),i&&o>0&&(o="+"[s](o)),o},parseSingleSubheader:function(n,t,i,o,r){var a=((this[L]||ie)[ge]||ie)[se]||ie,l=r||{},u=e[s](n,M)[s](l[c]),p=l[fe],d=i||ie,f=d[Yn],m=u===d[Oe]?f===Re?De:Re:p,g=o||ie,w={sort:u,dir:m};return re(t||ie,w)&&(w={sort:null,dir:null}),l[H]=!l[de],l[Ge]=g[D],l[H]&&(l[Pe]=y(Me,pe({},[a,w]))),this.addStatToDictionary(l,o),l},addStatToDictionary:function(n,t){var i=n||ie,o=t||ie,a=i[Q],c=this[O][a],l=i[r],u=o[kn]?e[s](o[D],W)[s](l):l;c||(this[O][a]={abbr:u,desc:i[j]})},parseTableTitle:function(e){return this[i]({key:"tableTitle.fpi",view:e})}},Ke[ne]=d,m[Z](Ke,m),n[t]=Ke},"./src/pagetypes/fpi/PowerIndexPageType.js":function(e,n,s){"use strict";var i,r=s("./src/node_modules/espn-pagetype/ESPNpageType.js"),c=s(Rn),l=s("./src/node_modules/espn-powerIndex-parser/college-football.js"),u=s("./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/college-football.js"),p=s(Kn),d=s("./src/api/index.js"),f=s(ns),m=s(fs).handler,g=f.LEAGUE_NAMES,w=s(o),h=s("./src/pagetypes/fpi/renderer.jsx"),v=s(Nn),y=d.productAPI,b=w[a],j=w[C],S=w[be],_=f.isCollegeByLabel,N={nfl:"2015","college-football":"2005"};(i=new r({componentName:"FPI",config:v,fetchData:function(e,n){var s=n||b,t=s[F],i=((v[Fn]||S)(t)||b)[z]||b,o=i[x],r=s[x]||o,a=(i[P]||b)[r],c=s[P]?{}:a,l=j({},[s,c]),u=j({},[s,{filterBy:"powerindex",startingseason:N[t]}]),p=[];return p[T](y[pe]("FPI",l,e,{allowError:!0})),p[T](y[pe]("seasonsDropdownV3",u,e)),_(t)&&p[T](y[pe]("conferences",s,e)),Promise.all(p)},parser:c,metaData:p,updateState:function(e){return e},render:h,instanceAug:{handleRedirect:m}})).register({type:g.COLLEGE_FOOTBALL,parser:l,metaData:u}),e[t]=i},"./src/pagetypes/fpi/config sync recursive ^\\.\\/.*\\.js$":function(e,n,s){var i={"./college-football.js":"./src/pagetypes/fpi/config/college-football.js","./index.js":Nn,"./nfl.js":"./src/pagetypes/fpi/config/nfl.js","./utils.js":ue};function o(e){var n=r(e);return s(n)}function r(e){if(!s.o(i,e)){var n=new Error("Cannot find module '"+e+"'");throw n.code="MODULE_NOT_FOUND",n}return i[e]}o.keys=function(){return Object.keys(i)},o.resolve=r,e[t]=o,o.id=_n},"./src/pagetypes/fpi/config/college-football.js":function(e,n,s){"use strict";var i=s(ue),o=i.Stat,r=i.Group,a={label:"What is ESPN's College Football Power Index?",href:"https://web.archive.org/web/20201202152451/https://www.espn.com/blog/statsinfo/post/_/id/122612/an-inside-look-at-college-fpi"},c=[{param:l,text:k,note:Sn,noteWithLink:a},{param:"resume",text:"Resume",note:Sn,noteWithLink:a},{param:$,text:jn,note:"fpi.efficiencies.note.extend"}],u={team:{label:"Team"}},p=new r("fpi",[new o(le,"W-L",0,null,ce,"W-L",null,ae)],W),d={fpi:new r(null,[p,new r("fpi",[new o(l,k,1),new o(re,"RK",0,{sortAscending:!0}),new o(xn,"TREND",null,{hideSort:!0,isTrend:!0})],bn),new r("fpi",[new o(yn,vn,1,null,hn),new o("probwinout","WIN OUT%",1,{base100:!0}),new o("prob6wins","6WINS%",1,{base100:!0}),new o(wn,gn,1,{base100:!0}),new o("probwinconf","WIN CONF%",1,{base100:!0}),new o(mn,fn,1,{base100:!0}),new o(dn,"MAKE NC%",1,{base100:!0}),new o(pn,"WIN NC%",1,{base100:!0})],"PROJECTIONS")]),resume:new r(null,[new r("resume",[new o(re,k,0,{sortAscending:!0}),new o("tbd","AP/CFP",0,{sortAscending:!0}),new o("accomplishmentrank","SOR",0,{sortAscending:!0}),new o(un,"SOS",0,{sortAscending:!0}),new o(ln,cn,0,{sortAscending:!0}),new o("gamecontrolrank","GC",0,{sortAscending:!0}),new o(an,"AVGWP",0,{sortAscending:!0})],"RANKS")]),efficiencies:new r(null,[new r("efficiencies",[new o(rn,v,1),new o(on,h,0,{sortAscending:!0})],tn,!0),new r("efficiencies",[new o(sn,v,1),new o(nn,h,0,{sortAscending:!0})],en,!0),new r("efficiencies",[new o(Ze,v,1),new o(Qe,h,0,{sortAscending:!0})],Ye,!0),new r("efficiencies",[new o(Xe,v,1),new o(qe,h,0,{sortAscending:!0})],$e,!0)])},f={conference:"80",view:l,sort:{fpi:{sort:Re,dir:_},resume:{sort:"resume.fpirank",dir:"asc"},efficiencies:{sort:Je,dir:_}}};e[t]={views:c,defaults:f,fpiHeaders:d,staticColumns:u}},"./src/pagetypes/fpi/config/index.js":function(e,n,i){"use strict";var r=i(o);e[t]={getConfigByLeague:function(e,n){var t,o;try{t=i(_n)("./"[s](e,".js"))}catch(e){0}return o=r[Be](t)?t(n):t,r[C]({},o)}}},"./src/pagetypes/fpi/config/nfl.js":function(e,n,s){"use strict";var i=s(ue),o=i.Stat,r=i.Group,a={label:"What is ESPN's NFL Football Power Index?",href:"https://web.archive.org/web/20201202152451/https://www.espn.com/blog/statsinfo/post/_/id/123048/a-guide-to-nfl-fpi"},c={team:{label:"Team"}},u=new r("fpi",[new o(le,J,0,null,ce,J,null,ae)],W),p={fpi:new r(null,[u,new r("fpi",[new o(l,k,1),new o(re,"RK",0,{sortAscending:!0}),new o(xn,"TREND",null,{hideSort:!0,isTrend:!0}),new o("epaoffense","OFF",1),new o("epadefense","DEF",1),new o("epaspecialteams","ST",1)],bn),new r("fpi",[new o(un,"SOS",0,{sortAscending:!0}),new o(ln,cn,0,{sortAscending:!0}),new o(an,"AVGWP",0,{sortAscending:!0})],"RANKS")]),projections:new r(null,[new r("fpi",[new o(le,J,0,null,ce,J,null,ae)]),new r("projections",[new o(yn,vn,1,null,hn),new o(mn,fn,1,{base100:!0}),new o(wn,gn,1,{base100:!0}),new o("probmakedivplayoffs","MAKE DIV%",1,{base100:!0}),new o("probmakeconfchamp","MAKE CONF%",1,{base100:!0}),new o(dn,"MAKE SB%",1,{base100:!0}),new o(pn,"WIN SB%",1,{base100:!0})])]),efficiencies:new r(null,[u,new r("efficiencies",[new o(rn,v,1),new o(on,h,0,{sortAscending:!0})],tn,!0),new r("efficiencies",[new o(sn,v,1),new o(nn,h,0,{sortAscending:!0})],en,!0),new r("efficiencies",[new o(Ze,v,1),new o(Qe,h,0,{sortAscending:!0})],Ye,!0),new r("efficiencies",[new o(Xe,v,1),new o(qe,h,0,{sortAscending:!0})],$e,!0)])},d=[{param:l,text:k,note:ze,noteWithLink:a},{param:Ue,text:"Projections",note:ze,noteWithLink:a},{param:$,text:jn,note:"fpi.efficiencies.note"}],f={view:l,sort:{fpi:{sort:Re,dir:_},projections:{sort:"projections.projectedw",dir:_},efficiencies:{sort:Je,dir:_}}};e[t]={views:d,defaults:f,fpiHeaders:p,staticColumns:c}},"./src/pagetypes/fpi/config/utils.js":function(e,n,s){"use strict";var i=s(Ie)[Ae],o=i[Te],a=i[ke];e[t]={Group:function(e,n,s,t){e&&(this[A]=e),n&&(this[g]=n),s&&(this[D]=s),t&&(this[kn]=t)},Stat:function(e,n,s,t,i,l,u,p){var d=this,f=t||{},m=f[Ke],g=f.sortAscending,w=f[Y],h=f[X],v=f[V],y=f[de];d[c]=e,d[r]=n,d[we]=s||0,d[fe]=g?a:o,d[X]=h,d[de]=y,d[V]=v,m&&(d[Ke]=!0),w&&(d[Y]=!0),i&&(d[Dn]=i),p&&(d[En]=p),l&&(d[E]=l),u&&(d[An]=u)}}},"./src/pagetypes/fpi/index.js":function(e,n,s){"use strict";var i=s("./src/pagetypes/fpi/PowerIndexPageType.js"),o=s("./src/pagetypes/pageWrapper.jsx"),r={components:{SponsoredLinks:s("./src/components/ui/Ad/SponsoredLinks.jsx")}};e[t]=o(i,r)},"./src/pagetypes/fpi/renderer.jsx":function(e,s,i){"use strict";var r=i(o),c=r[a],l=i(Wn),u=i(m),p=i("./src/components/fpi/index.jsx"),d=i("./src/components/common/Layout/CoreLayout.jsx"),f=i("./src/components/ui/Cards/Card.jsx"),g=f.Card,w=f.Card__Content,h=i("./src/node_modules/espn-breakpoints/index.js"),v=h.TABLET[0],y=h.DESKTOP_LABEL,b=i(He).WEBVIEW;e[t]=function(){var e=this[oe]||c,s=e[L]||c,t=s.viewport,i=(s[ge]||c)[se]||c,o=i.modifier===b,a=t&&r.cnum(t.width,0,0),f=s.page||c,m=l(f,"meta.canonical"),h=l(f,"outbrain.id"),x=f.taboola,j=f[Le],S=f.content||c,_=(e.components||c).SponsoredLinks,N=this[ie]||r[be],T=(s.app||c).device,k=a?a<=v:!r.strMatch(T,y),A=l(e,"guest.profile");return u[n](d,{windowSize:a},u[n](g,null,u[n](w,null,u[n](p,{fpi:S,params:i,isMobile:k,handleRedirect:N,windowWidth:a,isWebview:o,guestProfile:A}))),_?u[n](_,{taboola:x,pageType:j,id:h,src:m,isHorizontal:!0}):null)}},6:function(e,n,s){s("./src/client/index.js"),e[t]=s("./src/pagetypes/fpi/index.js")}},[[6,"espnfitt"]]])}();

}
/*
     FILE ARCHIVED ON 15:24:51 Dec 02, 2020 AND RETRIEVED FROM THE
     INTERNET ARCHIVE ON 01:26:55 Jul 18, 2023.
     JAVASCRIPT APPENDED BY WAYBACK MACHINE, COPYRIGHT INTERNET ARCHIVE.

     ALL OTHER CONTENT MAY ALSO BE PROTECTED BY COPYRIGHT (17 U.S.C.
     SECTION 108(a)(3)).
*/
/*
playback timings (ms):
  captures_list: 197.813
  exclusion.robots: 0.064
  exclusion.robots.policy: 0.056
  RedisCDXSource: 0.555
  esindex: 0.008
  LoadShardBlock: 178.897 (3)
  PetaboxLoader3.datanode: 543.949 (4)
  load_resource: 409.256
  PetaboxLoader3.resolve: 25.179
*/