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

!function(){var e="createElement",n="",s="concat",t="exports",o="translate",i="./node_modules/@fitt/core/fitt/index.js",r="label",a="emptyObj",c="name",l="fpi",u="info",p="length",d="div",f="conference",m="./node_modules/react/index.js",g="stats",w="./node_modules/prop-types/index.js",h="RNK",v="EFF",y="string",b="season",x="view",j="description",S="desc",_="propTypes",N="push",T="FPI",k="category",A="siteName",I="state",L="mix",C="displayName",E="title",D="league",F="sort",P=" ",W="dictionary",O="isLink",R="-",V=".",H="./node_modules/classnames/index.js",M="object",G="selected",B="defaults",K="W-L-T",z="efficiencies",U="unshift",J="inherits",$="80",q="apply",X="prototype",Y="params",Q="isTrend",Z="team",ee="handleRedirect",ne="props",se="emptyAry",te="fpirank",oe="numties",ie="numlosses",re="numwins",ae="./src/pagetypes/fpi/config/utils.js",ce="getData",le="parseValue",ue="base100",pe="removeInteger",de="hideSort",fe="sortDirection",me="sortingStat",ge="completeConfig",we="routing",he="sport",ve="noop",ye="isConferenceSelected",be="super",xe="helpers",je="struc",Se="isRequired",_e="DIR_DESC",Ne="DIR_ASC",Te="paramValues",ke="./src/node_modules/espn-api/constants.js",Ae="type",Ie="down",Le="isMobile",Ce="value",Ee="link",De="split",Fe="stat",Pe="fpi.fpi",We="span",Oe="./src/node_modules/espn-common-constants/index.js",Re="Fragment",Ve="group",He="isFunc",Me="skipFormat",Ge="fpi.projections.note",Be="projections",Ke="efficiencies.totefficiency",ze="SPECIAL TEAMS",Ue="stefficiencyrank",Je="stefficiency",$e="DEFENSE",qe="defefficiencyrank",Xe="defefficiency",Ye="OFFENSE",Qe="offefficiencyrank",Ze="offefficiency",en="OVERALL",nn="totefficiencyrank",sn="totefficiency",tn="avgingamewprank",on="REM SOS",rn="sosremainingrank",an="avgsosrank",cn="probwintitle",ln="probmaketitlegame",un="PLAYOFF%",pn="probmakeplayoffs",dn="WIN DIV%",fn="probwindiv",mn="projectedl",gn="PROJ W-L",wn="projectedw",hn="POWER INDEX",vn="rankchange7days",yn="Efficiencies",bn="fpi.projections.note.extend",xn="./src/pagetypes/fpi/config sync recursive ^\\.\\/.*\\.js$",jn="./src/pagetypes/fpi/config/index.js",Sn="decimals",_n="parseStatsList",Nn="showGroup",Tn="sortingCategory",kn="parseDynamicSubheaders",An="parseSingleSubheader",In="thirdIndex",Ln="secondIndex",Cn="thirdName",En="secondName",Dn="getConfigByLeague",Fn="categories",Pn="./node_modules/lodash/get.js",Wn="./src/node_modules/espn-powerIndex-parser/index.js",On="./src/node_modules/espn-parser/index.js",Rn="./node_modules/lodash/capitalize.js",Vn="getSitename",Hn="paramValue",Mn="parsedResponse",Gn="./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/index.js",Bn="./src/node_modules/espn-metadata/espnMetadata.js",Kn="className",zn="./src/components/ui/AnchorLink/AnchorLink.jsx",Un="./node_modules/@fitt/ui-espn/common/Icon/Icon.jsx",Jn="subheaders",$n="headers",qn="direction",Xn="./node_modules/lodash/map.js",Yn="./node_modules/lodash/findIndex.js",Qn="./node_modules/lodash/concat.js",Zn="./src/node_modules/espn-sport-types/index.js",es="strReplace",ns="contextTypes",ss="noteWithLink",ts="conferences",os="dir/:dir",is="sort/:sort",rs="group/:group",as="season/:season",cs="view/:view",ls=":pageType",us=":league",ps="./src/pagetypes/helpers/redirect.js",ds="webpackJsonp";(window[ds]=window[ds]||[])[N]([[l],{"./src/components/fpi/Filters.jsx":function(n,s,o){"use strict";var r,c=o(m),l=o("./node_modules/create-react-class/index.js"),u=o(w),g=o("./node_modules/@fitt/ui-espn/common/Dropdown/DropdownList.jsx"),h=o(i),v=o("./node_modules/@fitt/ui-espn/common/Tabs/index.jsx").Tabs,y=o(ps).Options,x=h.cstr,j=h[se],S=h[a],_=h[He],N=[us,ls,"_",cs,as,rs,is,os];r=l({displayName:"Filters",defaultProps:{lists:{},defaults:{}},propTypes:{lists:u[M]},redirectConference:function(e){var n,s=this[ne]||S,t=s[ee],o=s[B]||S,i=x(o[f]);n=new y(Ve,N,{group:i}),_(t)&&t(n,e)},redirectSeason:function(e){var n,s=this[ne]||S,t=s[ee],o=s[B]||S,i=x(o[b]);n=new y(b,N,{season:i}),_(t)&&t(n,e)},render:function(){var n=this[ne]||S,s=n.lists||S,t=s[ts]||j,o=s.views||j,i=s.seasons||j,r=n[G]||S,a=r.viewIdx,l=r[f],u=r[b];return c[e](c[Re],null,c[e](v,{data:o,activeTab:a,dynamic:!0,className:"mt5 mb3"}),c[e](d,{className:"flex flex-wrap"},i[p]?c[e](g,{list:i,defaultValue:u,onChange:this.redirectSeason,className:"mt3 mr2"}):null,t[p]?c[e](g,{list:t,defaultValue:l,onChange:this.redirectConference,className:"mt3 mr2"}):null))}}),n[t]=r},"./src/components/fpi/Footer.jsx":function(o,c,l){"use strict";var u=l(m),f=l(i),g=l("./node_modules/@fitt/ui-espn/common/AnchorLink/index.jsx"),h=u[Re],v=l(w),b=l(H),x=l(Oe),j=f[se],S=f[a],N=l("./node_modules/@fitt/ui-espn/common/Glossary/index.jsx"),T=l("./src/components/ui/Layout/GridColumn.jsx"),k=l("./src/components/ui/Layout/GridLayout.jsx"),A=x.THEMES.DARK;function I(t,o){var i=t.notes||S,a=i.noteByView||n,c=i[ss]||S,l=c[r]||n,f=c.href||n,m=t[W]||j,w=m[p],v=(o&&o.theme)===A,y=b({"brdr-clr-gray-08":!v,"brdr-clr-heavy-metal":v}),x=b({"clr-gray-05":!v,"clr-boulder":v});return u[e](h,null,u[e](d,{className:n[s](y," n8 mt5 ")[s](x," pt5 bt")},u[e](d,{className:"footer__selectionNotes mb3"},a),f?u[e](g,{to:f,className:"h8"},l):null),w?u[e](k,{is_9_3:!0,className:"mt4"},u[e](T,{col1:!0},u[e](d,{className:n[s](y," bb")},u[e](We,{className:"n8 db pr3 "[s](x)})),u[e](N,{data:m,withWindowResize:!0,fullWidth:!0}))):null)}I[ns]={theme:v[y]},I[_]={},o[t]=I},"./src/components/fpi/Table.jsx":function(o,l,u){"use strict";(function(l){var h=u(m),v=u(i),y=u(w),b=v[se],S=v[a],_=v.first,N=v.cstr,T=v[es],k=u("./node_modules/mobx/lib/mobx.js").toJS,A=u("./src/components/ui/common/withConfig.js"),I=u(H),L=u(Zn).getSportIDBySportAbbrev,C=u(Qn),W=u("./node_modules/lodash/compact.js"),B=u(Yn),K=u(Xn),z=u("./node_modules/lodash/filter.js"),U=u("./node_modules/@fitt/ui-espn/common/Table/index.jsx"),J=U.FixedTable,$=U.Table__Heading,q=U.Table__Row,X=u("./src/components/fpi/common/HeaderLink.jsx"),Y=u("./src/components/fpi/common/StatCell.jsx"),ee=u("./src/components/fpi/common/TeamCell.jsx"),ne=u("./src/components/fpi/Trend/index.jsx"),te=u("./node_modules/@fitt/ui-espn/common/NoDataAvailable/index.jsx");function oe(t,o,i,r,a){var l=a||S;return K(t,(function(t){var a,u,p=t||S,d=p[Z]||S,m=d.id||"-1",w=p[f]||n,v=d.uid||n,y=function(s){return K(s,(function(s){var t=s||S,o=t[c]||n,i=t[Q],r=t[Ce],a=i&&"0"===r?"--":r;return i?h[e](ne,{key:o,trend:a,align:"trend"}):h[e](Y,{key:o,value:a})}))}(p[g]||b),x=!1,j=v?"team-"[s](v):n,_=v?"conf-"[s](v):n,N=l[Z]&&h[e](ee,{key:j,team:d,isMobile:o}),T=l[f]&&!o&&h[e](Y,{key:_,value:w}),k=W([N,T]);try{x=i.isFollowingTeam(m,r)}catch(e){}return u=I({"is-favorite":x}),a=C(k,y),h[e](q,{className:u},a)}))}function ie(t){var o=t||S,i=o[Le],a=o.table||S,l=o[G]||S,u=l[D],f="college-football"===u,m=a[E]||n,w=a[$n]||b,v=a.statics||b,y=k(w.colgroups,!1)||b,A=y,I=function(n,s){return K(n,(function(n,t){var o=n||P,i=s[t]||1;return h[e]($,{key:t,className:"tc",colSpan:i},o)}))}(w[$n]||b,A)||b,C=w[Jn]||b,W=z(w[Jn],(function(e){return e&&"CONF"!==e[r]}))||b,H=i&&f?W:C,M=oe(a[g]||b,i,o.fanData,N(L(u)),v)||b,U=function(t,o){var i=(o||S)[F]||n,a=i[qn]||"desc",l=(i[Fe]||n)[De](V)[1]||"fpi";return K(t,(function(n){var t=n||S,o=t[Ve],i=o?T(o,P,R):"none",u="group-"[s](i);return h[e](X,{className:u,curSorting:l,curDirection:a,type:t[c],text:t[r],isLink:t[O],desc:t[j],link:t[Ee]})}))}(H,l),q=((l[F]||S)[Fe]||"fpi.fpi")[De](V)[1]||"fpi",Y=B(H,[c,q])+1,Q=_(y),Z=o[x]||"fpi",ee="mt4 Table2__title--remove-capitalization FPI__Table--"[s](Z," league-")[s](u);return h[e](d,null,M[p]?h[e](J,{className:ee,title:m,colgroups:A,headings:I,subheaders:U,rows:M,fixedLeft:Q,sortedCol:Y,shouldScroll:!0}):h[e](d,{className:"mt5"},h[e](te,{content:"No Data Available"})))}ie[ns]={translate:y.func,fanData:y[M]},o[t]=l.env.TEST_CONTEXT?ie:A(ie)}).call(this,u("./node_modules/process/browser.js"))},"./src/components/fpi/Trend/index.jsx":function(s,o,r){"use strict";var a=r(m),c=r(i),l=r(H),u=r(w),p=r(Un);function f(s){var t=s.trend,o=s.up||n,i=s[Ie]||n,r=s.pos||n,u=s.align||"trend tc",f=c.cnum(t),m=c.isNum(f),g=Math.abs(f),w=m&&f>0||!1,h=n,v=m?g:t,y=w?"arrow__up":"arrow__down",b=r?y+"__"+r:n,x=l(u,{positive:m&&w,negative:m&&!w});return o&&i&&(h=w?o:i),a[e](d,{className:x,"aria-labelledby":b},m?a[e](p,{icon:y,title:h}):null,v)}r("./src/components/fpi/Trend/styles.css"),f[_]={trend:u[y],customStyles:u[M]},s[t]=f},"./src/components/fpi/Trend/styles.css":function(e,n,s){},"./src/components/fpi/common/ArrowIcon.jsx":function(n,o,i){"use strict";var r=i(m),a=i(w),c=i(Un);function l(n){return r[e](c,{className:"ArrowIcon__TableSort",icon:"caret__"[s](n[Ae])})}l[_]={type:a.oneOf(["up",Ie])},n[t]=l},"./src/components/fpi/common/HeaderLink.jsx":function(n,s,o){"use strict";var r=o(m),c=o(w),l=o(H),u=o(ke)[Te],p=u[Ne],f=u[_e],g=o(i)[a],h=o(zn),v=o("./src/components/fpi/common/ArrowIcon.jsx");function b(n){var s=n||g,t=s.curSorting,o=s.curDirection,i=s[Ae],a=s.text,c=s[O],u=s[S],m=s.styles,w=s[Ee],y=l(s[Kn],{underline:c}),b=function(n,s,t){var o;s===n&&(t===p&&(o=r[e](v,{type:"up"})),t===f&&(o=r[e](v,{type:Ie})));return o}(t,i,o);return r[e](We,{className:y,title:u,style:m},c&&w?r[e](h,{classes:"clr-gray-01",to:w},a,b):r[e](d,null,a))}b[_]={curSorting:c[y],curDirection:c[y],islink:c.bool,type:c[y],text:c[y][Se]},b[C]="HeaderLink",n[t]=b},"./src/components/fpi/common/StatCell.jsx":function(n,s,o){"use strict";var i=o(m),r=o(w),a=o(H);function c(n){var s=n[Kn],t=n[Ce],o=n.hasColor,r=a(s,{"clr-negative":o&&t<0,"clr-positive":o&&t>=0});return i[e](d,{className:r},t)}c[_]={className:r[y],value:r[y][Se]},n[t]=c},"./src/components/fpi/common/TeamCell.jsx":function(n,s,o){"use strict";var r=o(m),c=o(w),l=o(zn),u=o("./src/components/ui/Logo/CombinerLogo.jsx"),p=o(i)[a];function f(n,s,t){var o=n;return s&&(o=r[e](l,{to:s,"data-clubhouse-uid":t},n)),o}function g(n){var s=n||p,t=s[Z]||p,o=s[Le],i=t.links,a=t.logo,c=t[C],l=t.abbrev,m=t.uid,g=o?l:c,w=a?function(n,s){return r[e](u,{src:n,name:s,height:20,width:20,size:"sm"})}(a,c):null,h=w?f(w,i):null,v=f(g,i,m);return r[e](d,{className:"flex items-start mr7"},r[e](We,{className:"pr3 TeamLink__Logo"},h),v)}g[_]={team:c[M][Se],isMobile:c.bool},g.defaultProps={team:{}},n[t]=g},"./src/components/fpi/index.jsx":function(s,o,r){"use strict";var c=r(m),u=r(w),p=r("./src/components/common/PageHeading.jsx"),d=r(i),f=r("./node_modules/lodash/values.js"),g=r("./src/components/fpi/Filters.jsx"),h=r("./src/components/fpi/Table.jsx"),v=r("./src/components/fpi/Footer.jsx"),y=c[Re],b=d[a];function j(s){var t=s||b,o=t[Le],i=t[l]||b,r=t[Y]||b,a=i.filters,u=i[B]||b,d=i[G]||b,m=i.pageHeading||"Football Power Index",w=t[ee],j=i.table||b,S=t.guestProfile,_=t.isWebview,N=i.notes,T=f(i[W])||[],k=r[x]||n;return c[e](y,null,c[e](p,{title:m,className:"mb5",isWebview:_}),c[e](g,{lists:a,defaults:u,selected:d,handleRedirect:w}),c[e](h,{table:j,selected:d,isMobile:o,guestProfile:S,view:k}),c[e](v,{params:r,isWebview:_,dictionary:T,notes:N}))}r("./src/components/fpi/styles.css"),j[_]={content:u[M]},s[t]=j},"./src/components/fpi/styles.css":function(e,n,s){},"./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/college-football.js":function(e,s,c){"use strict";var l=c(Bn),p=c(Gn),d=c(i),m=d[a],g=d[je],w=d[L];function h(){p[q](this,arguments)}h.prototype={getTitle:function(){var e=this[u]||m,n=e[b],s=g(e[A]),t=e[f]||0,i={key:"meta.tite.powerIndex.ncaaf",season:n,siteName:s},r=e[ye]?w({},[i,{conference:t}]):i;return this[o](r)},getDescription:function(){var e=this[u]||m,n=g(e[A]),s=e[f],t={key:"meta.description.powerIndex.ncaaf",siteName:n},i=e[ye]?w({},[t,{conference:s}]):t;return this[o](i)},getKeywords:function(){var e=this[u]||m,n=g(e[A]),s=e[f],t={key:"meta.keywords.powerIndex.ncaaf",siteName:n},i=e[ye]?w({},[t,{conference:s}]):t;return this[o](i)},get:function e(){var s=this,t=s[xe]||m,o=s[I]||m,i=s[Mn]||m,a=(i[G]||m)[b]||n,c=(i.metadata||m)[f]||m,l=(c[Hn]||n)!==$,p=c[r]||n,d=t[Vn](o)||n;return s[u]=w(s[u],{season:a,siteName:d,conference:p,isConferenceSelected:l}),s[be](e)()}},l[J](h,p),e[t]=h},"./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/index.js":function(e,s,r){"use strict";var c=r(Bn),l=r(Rn),p=r(i),d=p[a],f=p[je],m=p[L],g=p[ve];function w(){c[q](this,arguments)}w[X]={getTitle:function(){var e=this[u]||d,s={key:"meta.title.powerIndex",league:e[D]||n,sport:e[he]||n,season:e[b]||n,siteName:e[A]||n};return this[o](s)},getDescription:function(){var e=this[u]||d,s={key:"meta.description.powerIndex",league:this[o](e[D])||n,siteName:e[A]||"espn"};return this[o](s)},getKeywords:function(){var e=this[u]||d,s={key:"meta.keywords.powerIndex",league:e[D]||n,sport:e[he]||n,siteName:e[A]||"espn"};return this[o](s)},getAnalytics:function(){var e=this[I]||d;return((this[xe]||d).getSportAnalytics||g)(e)},get:function e(){var s=this,t=s[xe]||d,i=s[I]||d,r=s[o](f(t.getLeagueName(i))),a=l(t.getSportName(i)),c=((s[Mn]||d)[G]||d)[b]||n,p=f(t[Vn](i))||n;return s[u]=m(s[u],{league:r,sport:a,season:c,siteName:p}),s[be](e)()}},c[J](w,c),e[t]=w},"./src/node_modules/espn-powerIndex-parser/college-football.js":function(e,s,c){"use strict";var l=c(On),u=c(Wn),p=c(i),d=p[L],f=p[a];function m(){u[q](this,arguments)}var g={getPageHeading:function(e,s,t,i){var a={key:"pageTitle.powerIndex.ncaaf",season:t},c=i||f,l=c[r]||n,u=(c[Hn]||n)!==$?d({},[a,{conference:l}]):a;return this[o](u)},parseTableTitle:function(e,n){return this[o]({key:"tableTitle.fpi",view:e,conference:n})}};m[X]=g,l[J](m,u),e[t]=m},"./src/node_modules/espn-powerIndex-parser/index.js":function(e,l,u){"use strict";var d,m=u(On),w=u(ke),h=u(i),v=u(Oe),y=u("./src/node_modules/espn-common-modules-parser/modules/urlCreation.js").handleUrlCreation,S=u("./node_modules/lodash/bind.js"),_=u(Qn),T=u("./node_modules/lodash/find.js"),A=u(Yn),H=u(Pn),M=u(Xn),G=u("./node_modules/lodash/reduce.js"),K=u("./node_modules/lodash/remove.js"),z=u(Rn),ee=h[a],ne=h[se],te=u("./node_modules/lodash/isEqual.js"),oe=h[He],ie=h.isNum,re=h.isObj,ae=h.isAry,ce=h[L],ve=h[je],ye=h[es],xe=h.idxOf,Se=h.forEach,Ae=v.LANGUAGE,Ie=w[Te],Le=Ie[_e],Pe=Ie[Ne],We=Ae.EN,Re=[us,ls,"_",cs,as,rs,is,os];function Me(){m[q](this,arguments),this[W]={}}d={parseView:function(e,n){var s=((this[I]||ee)[we]||ee)[Y]||ee,t=ce({},[s,{sort:null,dir:null}]),i=n||ee,r=i.param;return t[x]=r,{title:this[o](i.text),url:y(Re,t,e)}},parseViews:function(e,n){var s=this;return M(e,(function(e){return e&&s.parseView(n,e)}))},parseConferenceItem:function(e){var n=e||ee,s=n.shortName||n[c],t=n.groupId;return{value:t,label:s,paramValue:t}},parseConferencesList:function(e,n,t){var i=this[o]("Conferences"),r=n===We?"All "[s](i):i,a=M(e,this.parseConferenceItem);return a[p]&&a[U]({value:t,label:r,paramValue:t}),a},getPageHeading:function(e,n,s){var t={key:"pageTitle.powerIndex",league:ve(e),sport:z(n),season:s};return this[o](t)},parse:function e(s){var t,i=this,a=i[be](e)(s)||ee,c=i.getRoutingParam(x),l=H(a,"FPI[0]",ee),u=H(a,"seasonsDropdownV3[0].seasons",ne),p=H(a,"conferences[0].conferences",ne),d=l.teams||ne,m=l.currentValues||ee,g=l[Fn]||ne,w=l.currentSeason||ee,h=m[D],v=m[he],y=m[f],j=m.lang,S=m[F]||ee,_=m[b]||n,N=(i.config||ee)[Dn],k=oe(N)?N(h):ee,I=k[B]||ee,L=k.views||ee,P=k.fpiHeaders||ee,O=k.staticColumns||ee,R=I[f],V=I[x],G=c||V,z=(I[F]||ee)[G]||ee,U=w.year,J=P[G],q=A(L,(function(e){return e&&e.param===G})),X=(K(t=p,{groupId:$}),t),Y=i.parseViews(L,{view:V}),Q=H(Y,[q,E],n),Z=i.parseConferencesList(X,j,R),se=T(Z,(function(e){return e&&e[Ce]===y}))||ee,te=se[r]||n,ie=i.getPageHeading(h,v,_,se),re=i.parseTableTitle(Q,te),ae=i[ge](g,J),ce=i.parseHeaders(ae,z,S,O),le=i.parseTeamsStatsList(d,ae,g,O)||ee,ue=M(u||ne,(function(e){var s=e||ee,t=s.year;return{value:t,label:s[C]||n,paramValue:t}})),pe=q>=0?L[q]:ee,de=i[o](pe.note),fe=j===We?pe[ss]:ee;return{metadata:{conference:se},filters:{views:Y,conferences:Z,seasons:ue},defaults:{conference:R,season:U},selected:{viewIdx:q,conference:y,sort:S,season:_,league:h},pageHeading:ie,responses:s,table:{headers:ce,stats:le,title:re,statics:O},dictionary:i[W],notes:{noteByView:de,noteWithLink:fe}}},completeConfig:function(e,n){var s=n||{},t=s[k],o=t&&T(e,(function(e){return e&&e[c]===t})),i=s[g];return s[g]=M(i,o?S(this.completeConfigStat,this,o):S(this[ge],this,e)),s},completeConfigStat:function(e,s){var t,i=re(s)?s:{name:s},a=i[c],l=i[En],u=i[Cn],p=i[C],d="differential"===e[c],f=e.names||ne,m=xe(f,a),g=e.displayNames||ne,w=e.descriptions||ne,h=g[m],v=w[m],y=p&&this[o](p)||h;return i.index=m,i[me]=d?ye(a,/Differential$/,n):a,i[C]=y,i[j]=i[j]?i[j]:v,i[r]||(t=(e.labels||ne)[m]||n,i[r]=d?ye(t,/Diff$/,n):t),l&&(i[Ln]=xe(f,l)),u&&(i[In]=xe(f,u)),i},parseHeaders:function(e,n,s,t){var i=this,a=[],l=[],u=M(t,(function(e){var t=e||ee,a={label:t[r]&&i[o](t[r]),description:t[j]&&i[o](t[j]),isLink:t[O]||!1,name:t[c],sortDirection:t[fe]},l=t[k];return t.parse?S(i[An],i,l,n,s,null,a)():a})),d=i[kn](e,n,a,l,s),f=_(u,d);return!a[p]&&a[N](d[p]),a[U](u[p]),l[p]&&l[U](P),{colgroups:a,headers:l,subheaders:f}},parseDynamicSubheaders:function(e,n,s,t,i){var r,a=this,c=e[Tn]||e[k],l=e[g]||ne,u=e[E];return r=c?M(l,S(a[An],a,c,n,i,e)):G(l,(function(e,o){return _(e,a[kn](o,n,s,t,i))}),[]),u&&(s[N](l[p]),t[N](a[o](u))),r},parseTeamsStatsList:function(e,n,s,t){var o=this,i=n&&n[g];return Se(t,(function(e){var n=e||ee;if(n.parse){var t=o[ge](s,{category:n[k],stats:[n]});ae(i,!0)&&i[U](t)}})),M(e,S(o.parseSingleTeamStats,o,n))},parseSingleTeamStats:function(e,s){var t=s||ee,o=t[Z]||ee,i=H(o,"group.shortName")||n,r=t[Fn]||ne;return{team:this.parseTeam(o),stats:this[_n](e,r),conference:i}},parseStatsList:function(e,n){var s=this,t=e[k],o=e[g]||ne,i=t&&T(n,(function(e){return(e||ee)[c]===t}));return t?M(o,S(s.parseSingleStat,s,i)):G(o,(function(e,t){return _(e,s[_n](t,n))}),[])},parseSingleStat:function(e,t){var o=e||ee,i=t||ee,r=i.index,a=i[Ln],c=i[In],l=i[me],u=i[Sn]||0,p=i[pe],d=i.hasSign,f=i[Q],m=i[ue],g=(o.totals||ne)[r],w=g&&"-"!==g?g:"--",h=o.values||ne,v=h[r],y=ie(a)?h[a]:null,b=ie(c)?h[c]:null,x=ie(v)?this[le](v,u,p,d,m):w,j=ie(y)&&this[le](y,u,p,d),S=ie(b)&&this[le](b,u,p,d),_=j&&S?n[s](x,R)[s](j,R)[s](S):j?n[s](x,R)[s](j):x;return{name:l||n,value:_,isTrend:f}},parseValue:function(e,n,t,o){var i=e;return i=new Intl.NumberFormat(We,{minimumFractionDigits:n,maximumFractionDigits:n}).format(i),t&&(i=V[s](i[De](V)[1])),o&&i>0&&(i="+"[s](i)),i},parseSingleSubheader:function(e,t,o,i,r){var a=((this[I]||ee)[we]||ee)[Y]||ee,l=r||{},u=n[s](e,V)[s](l[c]),p=l[fe],d=o||ee,f=d[qn],m=u===d[Fe]?f===Pe?Le:Pe:p,g=i||ee,w={sort:u,dir:m};return te(t||ee,w)&&(w={sort:null,dir:null}),l[O]=!l[de],l[Ve]=g[E],l[O]&&(l[Ee]=y(Re,ce({},[a,w]))),this.addStatToDictionary(l,i),l},addStatToDictionary:function(e,t){var o=e||ee,i=t||ee,a=o[me],c=this[W][a],l=o[r],u=i[Nn]?n[s](i[E],P)[s](l):l;c||(this[W][a]={abbr:u,desc:o[j]})},parseTableTitle:function(e){return this[o]({key:"tableTitle.fpi",view:e})}},Me[X]=d,m[J](Me,m),e[t]=Me},"./src/pagetypes/fpi/PowerIndexPageType.js":function(e,n,s){"use strict";var o,r=s("./src/node_modules/espn-pagetype/ESPNpageType.js"),c=s(Wn),l=s("./src/node_modules/espn-powerIndex-parser/college-football.js"),u=s("./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/college-football.js"),p=s(Gn),d=s("./src/api/index.js"),f=s(Zn),m=s(ps).handler,g=f.LEAGUE_NAMES,w=s(i),h=s("./src/pagetypes/fpi/renderer.jsx"),v=s(jn),y=d.productAPI,b=w[a],j=w[L],S=w[ve],_=f.isCollegeByLabel,T={nfl:"2015","college-football":"2005"};(o=new r({componentName:"FPI",config:v,fetchData:function(e,n){var s=n||b,t=s[D],o=((v[Dn]||S)(t)||b)[B]||b,i=o[x],r=s[x]||i,a=(o[F]||b)[r],c=s[F]?{}:a,l=j({},[s,c]),u=j({},[s,{filterBy:"powerindex",startingseason:T[t]}]),p=[];return p[N](y[ce]("FPI",l,e,{allowError:!0})),p[N](y[ce]("seasonsDropdownV3",u,e)),_(t)&&p[N](y[ce]("conferences",s,e)),Promise.all(p)},parser:c,metaData:p,updateState:function(e){return e},render:h,instanceAug:{handleRedirect:m}})).register({type:g.COLLEGE_FOOTBALL,parser:l,metaData:u}),e[t]=o},"./src/pagetypes/fpi/config sync recursive ^\\.\\/.*\\.js$":function(e,n,s){var o={"./college-football.js":"./src/pagetypes/fpi/config/college-football.js","./index.js":jn,"./nfl.js":"./src/pagetypes/fpi/config/nfl.js","./utils.js":ae};function i(e){var n=r(e);return s(n)}function r(e){if(!s.o(o,e)){var n=new Error("Cannot find module '"+e+"'");throw n.code="MODULE_NOT_FOUND",n}return o[e]}i.keys=function(){return Object.keys(o)},i.resolve=r,e[t]=i,i.id=xn},"./src/pagetypes/fpi/config/college-football.js":function(e,n,s){"use strict";var o=s(ae),i=o.Stat,r=o.Group,a={label:"What is ESPN's College Football Power Index?",href:"https://web.archive.org/web/20201111103647/https://www.espn.com/blog/statsinfo/post/_/id/122612/an-inside-look-at-college-fpi"},c=[{param:l,text:T,note:bn,noteWithLink:a},{param:"resume",text:"Resume",note:bn,noteWithLink:a},{param:z,text:yn,note:"fpi.efficiencies.note.extend"}],u={team:{label:"Team"}},p=new r("fpi",[new i(re,"W-L",0,null,ie,"W-L",null,oe)],P),d={fpi:new r(null,[p,new r("fpi",[new i(l,T,1),new i(te,"RK",0,{sortAscending:!0}),new i(vn,"TREND",null,{hideSort:!0,isTrend:!0})],hn),new r("fpi",[new i(wn,gn,1,null,mn),new i("probwinout","WIN OUT%",1,{base100:!0}),new i("prob6wins","6WINS%",1,{base100:!0}),new i(fn,dn,1,{base100:!0}),new i("probwinconf","WIN CONF%",1,{base100:!0}),new i(pn,un,1,{base100:!0}),new i(ln,"MAKE NC%",1,{base100:!0}),new i(cn,"WIN NC%",1,{base100:!0})],"PROJECTIONS")]),resume:new r(null,[new r("resume",[new i(te,T,0,{sortAscending:!0}),new i("tbd","AP/CFP",0,{sortAscending:!0}),new i("accomplishmentrank","SOR",0,{sortAscending:!0}),new i(an,"SOS",0,{sortAscending:!0}),new i(rn,on,0,{sortAscending:!0}),new i("gamecontrolrank","GC",0,{sortAscending:!0}),new i(tn,"AVGWP",0,{sortAscending:!0})],"RANKS")]),efficiencies:new r(null,[new r("efficiencies",[new i(sn,v,1),new i(nn,h,0,{sortAscending:!0})],en,!0),new r("efficiencies",[new i(Ze,v,1),new i(Qe,h,0,{sortAscending:!0})],Ye,!0),new r("efficiencies",[new i(Xe,v,1),new i(qe,h,0,{sortAscending:!0})],$e,!0),new r("efficiencies",[new i(Je,v,1),new i(Ue,h,0,{sortAscending:!0})],ze,!0)])},f={conference:$,view:l,sort:{fpi:{sort:Pe,dir:S},resume:{sort:"resume.fpirank",dir:"asc"},efficiencies:{sort:Ke,dir:S}}};e[t]={views:c,defaults:f,fpiHeaders:d,staticColumns:u}},"./src/pagetypes/fpi/config/index.js":function(e,n,o){"use strict";var r=o(i);e[t]={getConfigByLeague:function(e,n){var t,i;try{t=o(xn)("./"[s](e,".js"))}catch(e){0}return i=r[He](t)?t(n):t,r[L]({},i)}}},"./src/pagetypes/fpi/config/nfl.js":function(e,n,s){"use strict";var o=s(ae),i=o.Stat,r=o.Group,a={label:"What is ESPN's NFL Football Power Index?",href:"https://web.archive.org/web/20201111103647/https://www.espn.com/blog/statsinfo/post/_/id/123048/a-guide-to-nfl-fpi"},c={team:{label:"Team"}},u=new r("fpi",[new i(re,K,0,null,ie,K,null,oe)],P),p={fpi:new r(null,[u,new r("fpi",[new i(l,T,1),new i(te,"RK",0,{sortAscending:!0}),new i(vn,"TREND",null,{hideSort:!0,isTrend:!0}),new i("epaoffense","OFF",1),new i("epadefense","DEF",1),new i("epaspecialteams","ST",1)],hn),new r("fpi",[new i(an,"SOS",0,{sortAscending:!0}),new i(rn,on,0,{sortAscending:!0}),new i(tn,"AVGWP",0,{sortAscending:!0})],"RANKS")]),projections:new r(null,[new r("fpi",[new i(re,K,0,null,ie,K,null,oe)]),new r("projections",[new i(wn,gn,1,null,mn),new i(pn,un,1,{base100:!0}),new i(fn,dn,1,{base100:!0}),new i("probmakedivplayoffs","MAKE DIV%",1,{base100:!0}),new i("probmakeconfchamp","MAKE CONF%",1,{base100:!0}),new i(ln,"MAKE SB%",1,{base100:!0}),new i(cn,"WIN SB%",1,{base100:!0})])]),efficiencies:new r(null,[u,new r("efficiencies",[new i(sn,v,1),new i(nn,h,0,{sortAscending:!0})],en,!0),new r("efficiencies",[new i(Ze,v,1),new i(Qe,h,0,{sortAscending:!0})],Ye,!0),new r("efficiencies",[new i(Xe,v,1),new i(qe,h,0,{sortAscending:!0})],$e,!0),new r("efficiencies",[new i(Je,v,1),new i(Ue,h,0,{sortAscending:!0})],ze,!0)])},d=[{param:l,text:T,note:Ge,noteWithLink:a},{param:Be,text:"Projections",note:Ge,noteWithLink:a},{param:z,text:yn,note:"fpi.efficiencies.note"}],f={view:l,sort:{fpi:{sort:Pe,dir:S},projections:{sort:"projections.projectedw",dir:S},efficiencies:{sort:Ke,dir:S}}};e[t]={views:d,defaults:f,fpiHeaders:p,staticColumns:c}},"./src/pagetypes/fpi/config/utils.js":function(e,n,s){"use strict";var o=s(ke)[Te],i=o[_e],a=o[Ne];e[t]={Group:function(e,n,s,t){e&&(this[k]=e),n&&(this[g]=n),s&&(this[E]=s),t&&(this[Nn]=t)},Stat:function(e,n,s,t,o,l,u,p){var d=this,f=t||{},m=f[Me],g=f.sortAscending,w=f[pe],h=f[ue],v=f[Q],y=f[de];d[c]=e,d[r]=n,d[Sn]=s||0,d[fe]=g?a:i,d[ue]=h,d[de]=y,d[Q]=v,m&&(d[Me]=!0),w&&(d[pe]=!0),o&&(d[En]=o),p&&(d[Cn]=p),l&&(d[C]=l),u&&(d[Tn]=u)}}},"./src/pagetypes/fpi/index.js":function(e,n,s){"use strict";var o=s("./src/pagetypes/fpi/PowerIndexPageType.js"),i=s("./src/pagetypes/pageWrapper.jsx"),r={components:{SponsoredLinks:s("./src/components/ui/Ad/SponsoredLinks.jsx")}};e[t]=i(o,r)},"./src/pagetypes/fpi/renderer.jsx":function(n,s,o){"use strict";var r=o(i),c=r[a],l=o(Pn),u=o(m),p=o("./src/components/fpi/index.jsx"),d=o("./src/components/common/Layout/CoreLayout.jsx"),f=o("./src/components/ui/Cards/Card.jsx"),g=f.Card,w=f.Card__Content,h=o("./src/node_modules/espn-breakpoints/index.js"),v=h.TABLET[0],y=h.DESKTOP_LABEL,b=o(Oe).WEBVIEW;n[t]=function(){var n=this[ne]||c,s=n[I]||c,t=s.viewport,o=(s[we]||c)[Y]||c,i=o.modifier===b,a=t&&r.cnum(t.width,0,0),f=s.page||c,m=l(f,"meta.canonical"),h=l(f,"outbrain.id"),x=f.taboola,j=f[Ae],S=f.content||c,_=(n.components||c).SponsoredLinks,N=this[ee]||r[ve],T=(s.app||c).device,k=a?a<=v:!r.strMatch(T,y),A=l(n,"guest.profile");return u[e](d,{windowSize:a},u[e](g,null,u[e](w,null,u[e](p,{fpi:S,params:o,isMobile:k,handleRedirect:N,windowWidth:a,isWebview:i,guestProfile:A}))),_?u[e](_,{taboola:x,pageType:j,id:h,src:m,isHorizontal:!0}):null)}},6:function(e,n,s){s("./src/client/index.js"),e[t]=s("./src/pagetypes/fpi/index.js")}},[[6,"espnfitt"]]])}();

}
/*
     FILE ARCHIVED ON 10:36:47 Nov 11, 2020 AND RETRIEVED FROM THE
     INTERNET ARCHIVE ON 01:26:31 Jul 18, 2023.
     JAVASCRIPT APPENDED BY WAYBACK MACHINE, COPYRIGHT INTERNET ARCHIVE.

     ALL OTHER CONTENT MAY ALSO BE PROTECTED BY COPYRIGHT (17 U.S.C.
     SECTION 108(a)(3)).
*/
/*
playback timings (ms):
  captures_list: 158.078
  exclusion.robots: 0.066
  exclusion.robots.policy: 0.058
  cdx.remote: 0.053
  esindex: 0.007
  LoadShardBlock: 120.734 (3)
  PetaboxLoader3.datanode: 246.585 (4)
  load_resource: 187.148
  PetaboxLoader3.resolve: 26.841
*/