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

!function(){var e="createElement",n="",s="exports",t="concat",o="translate",i="./node_modules/@fitt/core/fitt/index.js",r="label",a="emptyObj",c="name",l="fpi",p="conference",u="div",d="./node_modules/react/index.js",f="info",m="length",g="stats",w="./node_modules/prop-types/index.js",h="EFF",v="season",y="string",b="RNK",x="desc",j="description",S="propTypes",_="view",N="push",T="sort",k="FPI",A="dictionary",I="displayName",L="title",C="mix",E="category",D="siteName",F="league",P="state",O="./node_modules/classnames/index.js",W="object",R="selected",V="defaults",H=" ",M=".",G="isLink",B="handleRedirect",K="prototype",z="props",U="efficiencies",J="team",$="inherits",q="isTrend",X="unshift",Y="W-L",Q="params",Z="apply",ee="emptyAry",ne="fpirank",se="isConferenceSelected",te="split",oe="value",ie="isMobile",re="./src/pagetypes/fpi/config/utils.js",ae="getData",ce="stat",le="fpi.fpi",pe="span",ue="base100",de="removeInteger",fe="./src/node_modules/espn-common-constants/index.js",me="down",ge="hideSort",we="super",he="./src/node_modules/espn-api/constants.js",ve="paramValues",ye="DIR_ASC",be="sortDirection",xe="DIR_DESC",je="sortingStat",Se="isRequired",_e="Fragment",Ne="group",Te="completeConfig",ke="struc",Ae="routing",Ie="helpers",Le="isFunc",Ce="sport",Ee="noop",De="link",Fe="type",Pe="./node_modules/lodash/capitalize.js",Oe="./src/node_modules/espn-parser/index.js",We="getSitename",Re="./src/node_modules/espn-powerIndex-parser/index.js",Ve="paramValue",He="parsedResponse",Me="./node_modules/lodash/get.js",Ge="categories",Be="getConfigByLeague",Ke="./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/index.js",ze="./src/node_modules/espn-metadata/espnMetadata.js",Ue="secondName",Je="thirdName",$e="className",qe="./src/components/ui/AnchorLink/AnchorLink.jsx",Xe="secondIndex",Ye="parseSingleSubheader",Qe="parseDynamicSubheaders",Ze="sortingCategory",en="showGroup",nn="parseStatsList",sn="./node_modules/@fitt/ui-espn/common/Icon/Icon.jsx",tn="decimals",on="subheaders",rn="headers",an="parseValue",cn="./src/pagetypes/fpi/config/index.js",ln="./src/pagetypes/fpi/config sync recursive ^\\.\\/.*\\.js$",pn="fpi.projections.note.extend",un="Efficiencies",dn="rankchange7days",fn="POWER INDEX",mn="projectedw",gn="direction",wn="PROJ W-L",hn="projectedl",vn="./node_modules/lodash/map.js",yn="./node_modules/lodash/findIndex.js",bn="./node_modules/lodash/concat.js",xn="probwindiv",jn="WIN DIV%",Sn="./src/node_modules/espn-sport-types/index.js",_n="strReplace",Nn="probmakeplayoffs",Tn="PLAYOFF%",kn="contextTypes",An="probmaketitlegame",In="probwintitle",Ln="avgsosrank",Cn="noteWithLink",En="sosremainingrank",Dn="REM SOS",Fn="avgingamewprank",Pn="totefficiency",On="totefficiencyrank",Wn="OVERALL",Rn="offefficiency",Vn="offefficiencyrank",Hn="conferences",Mn="OFFENSE",Gn="defefficiency",Bn="defefficiencyrank",Kn="DEFENSE",zn="stefficiency",Un="stefficiencyrank",Jn="dir/:dir",$n="sort/:sort",qn="group/:group",Xn="season/:season",Yn="view/:view",Qn=":pageType",Zn=":league",es="SPECIAL TEAMS",ns="efficiencies.totefficiency",ss="projections",ts="./src/pagetypes/helpers/redirect.js",os="numwins",is="numlosses",rs="numties",as="fpi.projections.note",cs="skipFormat",ls="webpackJsonp";(window[ls]=window[ls]||[])[N]([[l],{"./src/components/fpi/Filters.jsx":function(n,t,o){"use strict";var r,c=o(d),l=o("./node_modules/create-react-class/index.js"),f=o(w),g=o("./node_modules/@fitt/ui-espn/common/Dropdown/DropdownList.jsx"),h=o(i),y=o("./node_modules/@fitt/ui-espn/common/Tabs/index.jsx").Tabs,b=o(ts).Options,x=h.cstr,j=h[ee],S=h[a],_=h[Le],N=[Zn,Qn,"_",Yn,Xn,qn,$n,Jn];r=l({displayName:"Filters",defaultProps:{lists:{},defaults:{}},propTypes:{lists:f[W]},redirectConference:function(e){var n,s=this[z]||S,t=s[B],o=s[V]||S,i=x(o[p]);n=new b(Ne,N,{group:i}),_(t)&&t(n,e)},redirectSeason:function(e){var n,s=this[z]||S,t=s[B],o=s[V]||S,i=x(o[v]);n=new b(v,N,{season:i}),_(t)&&t(n,e)},render:function(){var n=this[z]||S,s=n.lists||S,t=s[Hn]||j,o=s.views||j,i=s.seasons||j,r=n[R]||S,a=r.viewIdx,l=r[p],d=r[v];return c[e](c[_e],null,c[e](y,{data:o,activeTab:a,dynamic:!0,className:"mt5 mb3"}),c[e](u,{className:"flex flex-wrap"},i[m]?c[e](g,{list:i,defaultValue:d,onChange:this.redirectSeason,className:"mt3 mr2"}):null,t[m]?c[e](g,{list:t,defaultValue:l,onChange:this.redirectConference,className:"mt3 mr2"}):null))}}),n[s]=r},"./src/components/fpi/Footer.jsx":function(o,c,l){"use strict";var p=l(d),f=l(i),g=l("./node_modules/@fitt/ui-espn/common/AnchorLink/index.jsx"),h=p[_e],v=l(w),b=l(O),x=l(fe),j=f[ee],_=f[a],N=l("./node_modules/@fitt/ui-espn/common/Glossary/index.jsx"),T=l("./src/components/ui/Layout/GridColumn.jsx"),k=l("./src/components/ui/Layout/GridLayout.jsx"),I=x.THEMES.DARK;function L(s,o){var i=s.notes||_,a=i.noteByView||n,c=i[Cn]||_,l=c[r]||n,d=c.href||n,f=s[A]||j,w=f[m],v=(o&&o.theme)===I,y=b({"brdr-clr-gray-08":!v,"brdr-clr-heavy-metal":v}),x=b({"clr-gray-05":!v,"clr-boulder":v});return p[e](h,null,p[e](u,{className:n[t](y," n8 mt5 ")[t](x," pt5 bt")},p[e](u,{className:"footer__selectionNotes mb3"},a),d?p[e](g,{to:d,className:"h8"},l):null),w?p[e](k,{is_9_3:!0,className:"mt4"},p[e](T,{col1:!0},p[e](u,{className:n[t](y," bb")},p[e](pe,{className:"n8 db pr3 "[t](x)})),p[e](N,{data:f,withWindowResize:!0,fullWidth:!0}))):null)}L[kn]={theme:v[y]},L[S]={},o[s]=L},"./src/components/fpi/Table.jsx":function(o,l,f){"use strict";(function(l){var h=f(d),v=f(i),y=f(w),b=v[ee],x=v[a],S=v.first,N=v.cstr,k=v[_n],A=f("./node_modules/mobx/lib/mobx.js").toJS,I=f("./src/components/ui/common/withConfig.js"),C=f(O),E=f(Sn).getSportIDBySportAbbrev,D=f(bn),P=f("./node_modules/lodash/compact.js"),V=f(yn),B=f(vn),K=f("./node_modules/lodash/filter.js"),z=f("./node_modules/@fitt/ui-espn/common/Table/index.jsx"),U=z.FixedTable,$=z.Table__Heading,X=z.Table__Row,Y=f("./src/components/fpi/common/HeaderLink.jsx"),Q=f("./src/components/fpi/common/StatCell.jsx"),Z=f("./src/components/fpi/common/TeamCell.jsx"),ne=f("./src/components/fpi/Trend/index.jsx"),se=f("./node_modules/@fitt/ui-espn/common/NoDataAvailable/index.jsx");function re(s,o,i,r,a){var l=a||x;return B(s,(function(s){var a,u,d=s||x,f=d[J]||x,m=f.id||"-1",w=d[p]||n,v=f.uid||n,y=function(s){return B(s,(function(s){var t=s||x,o=t[c]||n,i=t[q],r=t[oe],a=i&&"0"===r?"--":r;return i?h[e](ne,{key:o,trend:a,align:"trend"}):h[e](Q,{key:o,value:a})}))}(d[g]||b),j=!1,S=v?"team-"[t](v):n,_=v?"conf-"[t](v):n,N=l[J]&&h[e](Z,{key:S,team:f,isMobile:o}),T=l[p]&&!o&&h[e](Q,{key:_,value:w}),k=P([N,T]);try{j=i.isFollowingTeam(m,r)}catch(e){}return u=C({"is-favorite":j}),a=D(k,y),h[e](X,{className:u},a)}))}function ae(s){var o=s||x,i=o[ie],a=o.table||x,l=o[R]||x,p=l[F],d="college-football"===p,f=a[L]||n,w=a[rn]||b,v=a.statics||b,y=A(w.colgroups,!1)||b,I=y,C=function(n,s){return B(n,(function(n,t){var o=n||H,i=s[t]||1;return h[e]($,{key:t,className:"tc",colSpan:i},o)}))}(w[rn]||b,I)||b,D=w[on]||b,P=K(w[on],(function(e){return e&&"CONF"!==e[r]}))||b,O=i&&d?P:D,W=re(a[g]||b,i,o.fanData,N(E(p)),v)||b,z=function(s,o){var i=(o||x)[T]||n,a=i[gn]||"desc",l=(i[ce]||n)[te](M)[1]||"fpi";return B(s,(function(n){var s=n||x,o=s[Ne],i=o?k(o,H,"-"):"none",p="group-"[t](i);return h[e](Y,{className:p,curSorting:l,curDirection:a,type:s[c],text:s[r],isLink:s[G],desc:s[j],link:s[De]})}))}(O,l),J=((l[T]||x)[ce]||"fpi.fpi")[te](M)[1]||"fpi",q=V(O,[c,J])+1,X=S(y),Q=o[_]||"fpi",Z="mt4 Table2__title--remove-capitalization FPI__Table--"[t](Q," league-")[t](p);return h[e](u,null,W[m]?h[e](U,{className:Z,title:f,colgroups:I,headings:C,subheaders:z,rows:W,fixedLeft:X,sortedCol:q,shouldScroll:!0}):h[e](u,{className:"mt5"},h[e](se,{content:"No Data Available"})))}ae[kn]={translate:y.func,fanData:y[W]},o[s]=l.env.TEST_CONTEXT?ae:I(ae)}).call(this,f("./node_modules/process/browser.js"))},"./src/components/fpi/Trend/index.jsx":function(t,o,r){"use strict";var a=r(d),c=r(i),l=r(O),p=r(w),f=r(sn);function m(s){var t=s.trend,o=s.up||n,i=s[me]||n,r=s.pos||n,p=s.align||"trend tc",d=c.cnum(t),m=c.isNum(d),g=Math.abs(d),w=m&&d>0||!1,h=n,v=m?g:t,y=w?"arrow__up":"arrow__down",b=r?y+"__"+r:n,x=l(p,{positive:m&&w,negative:m&&!w});return o&&i&&(h=w?o:i),a[e](u,{className:x,"aria-labelledby":b},m?a[e](f,{icon:y,title:h}):null,v)}r("./src/components/fpi/Trend/styles.css"),m[S]={trend:p[y],customStyles:p[W]},t[s]=m},"./src/components/fpi/Trend/styles.css":function(e,n,s){},"./src/components/fpi/common/ArrowIcon.jsx":function(n,o,i){"use strict";var r=i(d),a=i(w),c=i(sn);function l(n){return r[e](c,{className:"ArrowIcon__TableSort",icon:"caret__"[t](n[Fe])})}l[S]={type:a.oneOf(["up",me])},n[s]=l},"./src/components/fpi/common/HeaderLink.jsx":function(n,t,o){"use strict";var r=o(d),c=o(w),l=o(O),p=o(he)[ve],f=p[ye],m=p[xe],g=o(i)[a],h=o(qe),v=o("./src/components/fpi/common/ArrowIcon.jsx");function b(n){var s=n||g,t=s.curSorting,o=s.curDirection,i=s[Fe],a=s.text,c=s[G],p=s[x],d=s.styles,w=s[De],y=l(s[$e],{underline:c}),b=function(n,s,t){var o;s===n&&(t===f&&(o=r[e](v,{type:"up"})),t===m&&(o=r[e](v,{type:me})));return o}(t,i,o);return r[e](pe,{className:y,title:p,style:d},c&&w?r[e](h,{classes:"clr-gray-01",to:w},a,b):r[e](u,null,a))}b[S]={curSorting:c[y],curDirection:c[y],islink:c.bool,type:c[y],text:c[y][Se]},b[I]="HeaderLink",n[s]=b},"./src/components/fpi/common/StatCell.jsx":function(n,t,o){"use strict";var i=o(d),r=o(w),a=o(O);function c(n){var s=n[$e],t=n[oe],o=n.hasColor,r=a(s,{"clr-negative":o&&t<0,"clr-positive":o&&t>=0});return i[e](u,{className:r},t)}c[S]={className:r[y],value:r[y][Se]},n[s]=c},"./src/components/fpi/common/TeamCell.jsx":function(n,t,o){"use strict";var r=o(d),c=o(w),l=o(qe),p=o("./src/components/ui/Logo/CombinerLogo.jsx"),f=o(i)[a];function m(n,s,t){var o=n;return s&&(o=r[e](l,{to:s,"data-clubhouse-uid":t},n)),o}function g(n){var s=n||f,t=s[J]||f,o=s[ie],i=t.links,a=t.logo,c=t[I],l=t.abbrev,d=t.uid,g=o?l:c,w=a?function(n,s){return r[e](p,{src:n,name:s,height:20,width:20,size:"sm"})}(a,c):null,h=w?m(w,i):null,v=m(g,i,d);return r[e](u,{className:"flex items-start mr7"},r[e](pe,{className:"pr3 TeamLink__Logo"},h),v)}g[S]={team:c[W][Se],isMobile:c.bool},g.defaultProps={team:{}},n[s]=g},"./src/components/fpi/index.jsx":function(t,o,r){"use strict";var c=r(d),p=r(w),u=r("./src/components/common/PageHeading.jsx"),f=r(i),m=r("./node_modules/lodash/values.js"),g=r("./src/components/fpi/Filters.jsx"),h=r("./src/components/fpi/Table.jsx"),v=r("./src/components/fpi/Footer.jsx"),y=c[_e],b=f[a];function x(s){var t=s||b,o=t[ie],i=t[l]||b,r=t[Q]||b,a=i.filters,p=i[V]||b,d=i[R]||b,f=i.pageHeading||"Football Power Index",w=t[B],x=i.table||b,j=t.guestProfile,S=t.isWebview,N=i.notes,T=m(i[A])||[],k=r[_]||n;return c[e](y,null,c[e](u,{title:f,className:"mb5",isWebview:S}),c[e](g,{lists:a,defaults:p,selected:d,handleRedirect:w}),c[e](h,{table:x,selected:d,isMobile:o,guestProfile:j,view:k}),c[e](v,{params:r,isWebview:S,dictionary:T,notes:N}))}r("./src/components/fpi/styles.css"),x[S]={content:p[W]},t[s]=x},"./src/components/fpi/styles.css":function(e,n,s){},"./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/college-football.js":function(e,t,c){"use strict";var l=c(ze),u=c(Ke),d=c(i),m=d[a],g=d[ke],w=d[C];function h(){u[Z](this,arguments)}h.prototype={getTitle:function(){var e=this[f]||m,n=e[v],s=g(e[D]),t=e[p]||0,i={key:"meta.tite.powerIndex.ncaaf",season:n,siteName:s},r=e[se]?w({},[i,{conference:t}]):i;return this[o](r)},getDescription:function(){var e=this[f]||m,n=g(e[D]),s=e[p],t={key:"meta.description.powerIndex.ncaaf",siteName:n},i=e[se]?w({},[t,{conference:s}]):t;return this[o](i)},getKeywords:function(){var e=this[f]||m,n=g(e[D]),s=e[p],t={key:"meta.keywords.powerIndex.ncaaf",siteName:n},i=e[se]?w({},[t,{conference:s}]):t;return this[o](i)},get:function e(){var s=this,t=s[Ie]||m,o=s[P]||m,i=s[He]||m,a=(i[R]||m)[v]||n,c=(i.metadata||m)[p]||m,l="80"!==(c[Ve]||n),u=c[r]||n,d=t[We](o)||n;return s[f]=w(s[f],{season:a,siteName:d,conference:u,isConferenceSelected:l}),s[we](e)()}},l[$](h,u),e[s]=h},"./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/index.js":function(e,t,r){"use strict";var c=r(ze),l=r(Pe),p=r(i),u=p[a],d=p[ke],m=p[C],g=p[Ee];function w(){c[Z](this,arguments)}w[K]={getTitle:function(){var e=this[f]||u,s={key:"meta.title.powerIndex",league:e[F]||n,sport:e[Ce]||n,season:e[v]||n,siteName:e[D]||n};return this[o](s)},getDescription:function(){var e=this[f]||u,s={key:"meta.description.powerIndex",league:this[o](e[F])||n,siteName:e[D]||"espn"};return this[o](s)},getKeywords:function(){var e=this[f]||u,s={key:"meta.keywords.powerIndex",league:e[F]||n,sport:e[Ce]||n,siteName:e[D]||"espn"};return this[o](s)},getAnalytics:function(){var e=this[P]||u;return((this[Ie]||u).getSportAnalytics||g)(e)},get:function e(){var s=this,t=s[Ie]||u,i=s[P]||u,r=s[o](d(t.getLeagueName(i))),a=l(t.getSportName(i)),c=((s[He]||u)[R]||u)[v]||n,p=d(t[We](i))||n;return s[f]=m(s[f],{league:r,sport:a,season:c,siteName:p}),s[we](e)()}},c[$](w,c),e[s]=w},"./src/node_modules/espn-powerIndex-parser/college-football.js":function(e,t,c){"use strict";var l=c(Oe),p=c(Re),u=c(i),d=u[C],f=u[a];function m(){p[Z](this,arguments)}var g={getPageHeading:function(e,s,t,i){var a={key:"pageTitle.powerIndex.ncaaf",season:t},c=i||f,l=c[r]||n,p="80"!==(c[Ve]||n)?d({},[a,{conference:l}]):a;return this[o](p)},parseTableTitle:function(e,n){return this[o]({key:"tableTitle.fpi",view:e,conference:n})}};m[K]=g,l[$](m,p),e[s]=m},"./src/node_modules/espn-powerIndex-parser/index.js":function(e,l,u){"use strict";var d,f=u(Oe),w=u(he),h=u(i),y=u(fe),b=u("./src/node_modules/espn-common-modules-parser/modules/urlCreation.js").handleUrlCreation,x=u("./node_modules/lodash/bind.js"),S=u(bn),k=u("./node_modules/lodash/find.js"),D=u(yn),O=u(Me),W=u(vn),R=u("./node_modules/lodash/reduce.js"),B=u("./node_modules/lodash/remove.js"),z=u(Pe),U=h[a],Y=h[ee],ne=u("./node_modules/lodash/isEqual.js"),se=h[Le],ie=h.isNum,re=h.isObj,ae=h.isAry,le=h[C],pe=h[ke],me=h[_n],Se=h.idxOf,_e=h.forEach,Ie=y.LANGUAGE,Ee=w[ve],Fe=Ee[xe],We=Ee[ye],Re=Ie.EN,Ve=[Zn,Qn,"_",Yn,Xn,qn,$n,Jn];function He(){f[Z](this,arguments),this[A]={}}d={parseView:function(e,n){var s=((this[P]||U)[Ae]||U)[Q]||U,t=le({},[s,{sort:null,dir:null}]),i=n||U,r=i.param;return t[_]=r,{title:this[o](i.text),url:b(Ve,t,e)}},parseViews:function(e,n){var s=this;return W(e,(function(e){return e&&s.parseView(n,e)}))},parseConferenceItem:function(e){var n=e||U,s=n.shortName||n[c],t=n.groupId;return{value:t,label:s,paramValue:t}},parseConferencesList:function(e,n,s){var i=this[o]("Conferences"),r=n===Re?"All "[t](i):i,a=W(e,this.parseConferenceItem);return a[m]&&a[X]({value:s,label:r,paramValue:s}),a},getPageHeading:function(e,n,s){var t={key:"pageTitle.powerIndex",league:pe(e),sport:z(n),season:s};return this[o](t)},parse:function e(s){var t,i=this,a=i[we](e)(s)||U,c=i.getRoutingParam(_),l=O(a,"FPI[0]",U),u=O(a,"seasonsDropdownV3[0].seasons",Y),d=O(a,"conferences[0].conferences",Y),f=l.teams||Y,m=l.currentValues||U,g=l[Ge]||Y,w=l.currentSeason||U,h=m[F],y=m[Ce],b=m[p],x=m.lang,j=m[T]||U,S=m[v]||n,N=(i.config||U)[Be],C=se(N)?N(h):U,E=C[V]||U,P=C.views||U,R=C.fpiHeaders||U,H=C.staticColumns||U,M=E[p],G=E[_],K=c||G,z=(E[T]||U)[K]||U,J=w.year,$=R[K],q=D(P,(function(e){return e&&e.param===K})),X=(B(t=d,{groupId:"80"}),t),Q=i.parseViews(P,{view:G}),Z=O(Q,[q,L],n),ee=i.parseConferencesList(X,x,M),ne=k(ee,(function(e){return e&&e[oe]===b}))||U,te=ne[r]||n,ie=i.getPageHeading(h,y,S,ne),re=i.parseTableTitle(Z,te),ae=i[Te](g,$),ce=i.parseHeaders(ae,z,j,H),le=i.parseTeamsStatsList(f,ae,g,H)||U,pe=W(u||Y,(function(e){var s=e||U,t=s.year;return{value:t,label:s[I]||n,paramValue:t}})),ue=q>=0?P[q]:U,de=i[o](ue.note),fe=x===Re?ue[Cn]:U;return{metadata:{conference:ne},filters:{views:Q,conferences:ee,seasons:pe},defaults:{conference:M,season:J},selected:{viewIdx:q,conference:b,sort:j,season:S,league:h},pageHeading:ie,responses:s,table:{headers:ce,stats:le,title:re,statics:H},dictionary:i[A],notes:{noteByView:de,noteWithLink:fe}}},completeConfig:function(e,n){var s=n||{},t=s[E],o=t&&k(e,(function(e){return e&&e[c]===t})),i=s[g];return s[g]=W(i,o?x(this.completeConfigStat,this,o):x(this[Te],this,e)),s},completeConfigStat:function(e,s){var t,i=re(s)?s:{name:s},a=i[c],l=i[Ue],p=i[Je],u=i[I],d="differential"===e[c],f=e.names||Y,m=Se(f,a),g=e.displayNames||Y,w=e.descriptions||Y,h=g[m],v=w[m],y=u&&this[o](u)||h;return i.index=m,i[je]=d?me(a,/Differential$/,n):a,i[I]=y,i[j]=i[j]?i[j]:v,i[r]||(t=(e.labels||Y)[m]||n,i[r]=d?me(t,/Diff$/,n):t),l&&(i[Xe]=Se(f,l)),p&&(i.thirdIndex=Se(f,p)),i},parseHeaders:function(e,n,s,t){var i=this,a=[],l=[],p=W(t,(function(e){var t=e||U,a={label:t[r]&&i[o](t[r]),description:t[j]&&i[o](t[j]),isLink:t[G]||!1,name:t[c],sortDirection:t[be]},l=t[E];return t.parse?x(i[Ye],i,l,n,s,null,a)():a})),u=i[Qe](e,n,a,l,s),d=S(p,u);return!a[m]&&a[N](u[m]),a[X](p[m]),l[m]&&l[X](H),{colgroups:a,headers:l,subheaders:d}},parseDynamicSubheaders:function(e,n,s,t,i){var r,a=this,c=e[Ze]||e[E],l=e[g]||Y,p=e[L];return r=c?W(l,x(a[Ye],a,c,n,i,e)):R(l,(function(e,o){return S(e,a[Qe](o,n,s,t,i))}),[]),p&&(s[N](l[m]),t[N](a[o](p))),r},parseTeamsStatsList:function(e,n,s,t){var o=this,i=n&&n[g];return _e(t,(function(e){var n=e||U;if(n.parse){var t=o[Te](s,{category:n[E],stats:[n]});ae(i,!0)&&i[X](t)}})),W(e,x(o.parseSingleTeamStats,o,n))},parseSingleTeamStats:function(e,s){var t=s||U,o=t[J]||U,i=O(o,"group.shortName")||n,r=t[Ge]||Y;return{team:this.parseTeam(o),stats:this[nn](e,r),conference:i}},parseStatsList:function(e,n){var s=this,t=e[E],o=e[g]||Y,i=t&&k(n,(function(e){return(e||U)[c]===t}));return t?W(o,x(s.parseSingleStat,s,i)):R(o,(function(e,t){return S(e,s[nn](t,n))}),[])},parseSingleStat:function(e,s){var o=e||U,i=s||U,r=i.index,a=i[Xe],c=i[je],l=i[tn]||0,p=i[de],u=i.hasSign,d=i[q],f=i[ue],m=(o.totals||Y)[r],g=m&&"-"!==m?m:"--",w=o.values||Y,h=w[r],v=ie(a)?w[a]:null,y=ie(h)?this[an](h,l,p,u,f):g,b=ie(v)&&this[an](v,l,p,u),x=b?n[t](y,"-")[t](b):y;return{name:c||n,value:x,isTrend:d}},parseValue:function(e,n,s,o){var i=e;return i=new Intl.NumberFormat(Re,{minimumFractionDigits:n,maximumFractionDigits:n}).format(i),s&&(i=M[t](i[te](M)[1])),o&&i>0&&(i="+"[t](i)),i},parseSingleSubheader:function(e,s,o,i,r){var a=((this[P]||U)[Ae]||U)[Q]||U,l=r||{},p=n[t](e,M)[t](l[c]),u=l[be],d=o||U,f=d[gn],m=p===d[ce]?f===We?Fe:We:u,g=i||U,w={sort:p,dir:m};return ne(s||U,w)&&(w={sort:null,dir:null}),l[G]=!l[ge],l[Ne]=g[L],l[G]&&(l[De]=b(Ve,le({},[a,w]))),this.addStatToDictionary(l,i),l},addStatToDictionary:function(e,s){var o=e||U,i=s||U,a=o[je],c=this[A][a],l=o[r],p=i[en]?n[t](i[L],H)[t](l):l;c||(this[A][a]={abbr:p,desc:o[j]})},parseTableTitle:function(e){return this[o]({key:"tableTitle.fpi",view:e})}},He[K]=d,f[$](He,f),e[s]=He},"./src/pagetypes/fpi/PowerIndexPageType.js":function(e,n,t){"use strict";var o,r=t("./src/node_modules/espn-pagetype/ESPNpageType.js"),c=t(Re),l=t("./src/node_modules/espn-powerIndex-parser/college-football.js"),p=t("./src/node_modules/espn-metadata/pagetypes/espn-powerIndex-metadata/college-football.js"),u=t(Ke),d=t("./src/api/index.js"),f=t(Sn),m=t(ts).handler,g=f.LEAGUE_NAMES,w=t(i),h=t("./src/pagetypes/fpi/renderer.jsx"),v=t(cn),y=d.productAPI,b=w[a],x=w[C],j=w[Ee],S=f.isCollegeByLabel,k={nfl:"2015","college-football":"2005"};(o=new r({componentName:"FPI",config:v,fetchData:function(e,n){var s=n||b,t=s[F],o=((v[Be]||j)(t)||b)[V]||b,i=o[_],r=s[_]||i,a=(o[T]||b)[r],c=s[T]?{}:a,l=x({},[s,c]),p=x({},[s,{filterBy:"powerindex",startingseason:k[t]}]),u=[];return u[N](y[ae]("FPI",l,e,{allowError:!0})),u[N](y[ae]("seasonsDropdownV3",p,e)),S(t)&&u[N](y[ae]("conferences",s,e)),Promise.all(u)},parser:c,metaData:u,updateState:function(e){return e},render:h,instanceAug:{handleRedirect:m}})).register({type:g.COLLEGE_FOOTBALL,parser:l,metaData:p}),e[s]=o},"./src/pagetypes/fpi/config sync recursive ^\\.\\/.*\\.js$":function(e,n,t){var o={"./college-football.js":"./src/pagetypes/fpi/config/college-football.js","./index.js":cn,"./nfl.js":"./src/pagetypes/fpi/config/nfl.js","./utils.js":re};function i(e){var n=r(e);return t(n)}function r(e){if(!t.o(o,e)){var n=new Error("Cannot find module '"+e+"'");throw n.code="MODULE_NOT_FOUND",n}return o[e]}i.keys=function(){return Object.keys(o)},i.resolve=r,e[s]=i,i.id=ln},"./src/pagetypes/fpi/config/college-football.js":function(e,n,t){"use strict";var o=t(re),i=o.Stat,r=o.Group,a={label:"What is ESPN's College Football Power Index?",href:"https://web.archive.org/web/20200922093936/https://www.espn.com/blog/statsinfo/post/_/id/122612/an-inside-look-at-college-fpi"},c=[{param:l,text:k,note:pn,noteWithLink:a},{param:"resume",text:"Resume",note:pn,noteWithLink:a},{param:U,text:un,note:"fpi.efficiencies.note.extend"}],p={team:{label:"Team"}},u={fpi:new r(null,[new r("fpi",[new i(l,k,1),new i(ne,"RK",0,{sortAscending:!0}),new i(dn,"TREND",null,{hideSort:!0,isTrend:!0})],fn),new r("fpi",[new i(mn,wn,1,null,hn),new i("probwinout","WIN OUT%",1,{base100:!0}),new i("prob6wins","6WINS%",1,{base100:!0}),new i(xn,jn,1,{base100:!0}),new i("probwinconf","WIN CONF%",1,{base100:!0}),new i(Nn,Tn,1,{base100:!0}),new i(An,"MAKE NC%",1,{base100:!0}),new i(In,"WIN NC%",1,{base100:!0})],"PROJECTIONS")]),resume:new r(null,[new r("resume",[new i(ne,k,0,{sortAscending:!0}),new i("tbd","AP/CFP",0,{sortAscending:!0}),new i("accomplishmentrank","SOR",0,{sortAscending:!0}),new i(Ln,"SOS",0,{sortAscending:!0}),new i(En,Dn,0,{sortAscending:!0}),new i("gamecontrolrank","GC",0,{sortAscending:!0}),new i(Fn,"AVGWP",0,{sortAscending:!0})],"RANKS")]),efficiencies:new r(null,[new r("efficiencies",[new i(Pn,h,1),new i(On,b,0,{sortAscending:!0})],Wn,!0),new r("efficiencies",[new i(Rn,h,1),new i(Vn,b,0,{sortAscending:!0})],Mn,!0),new r("efficiencies",[new i(Gn,h,1),new i(Bn,b,0,{sortAscending:!0})],Kn,!0),new r("efficiencies",[new i(zn,h,1),new i(Un,b,0,{sortAscending:!0})],es,!0)])},d={conference:"80",view:l,sort:{fpi:{sort:le,dir:x},resume:{sort:"resume.fpirank",dir:"asc"},efficiencies:{sort:ns,dir:x}}};e[s]={views:c,defaults:d,fpiHeaders:u,staticColumns:p}},"./src/pagetypes/fpi/config/index.js":function(e,n,o){"use strict";var r=o(i);e[s]={getConfigByLeague:function(e,n){var s,i;try{s=o(ln)("./"[t](e,".js"))}catch(e){0}return i=r[Le](s)?s(n):s,r[C]({},i)}}},"./src/pagetypes/fpi/config/nfl.js":function(e,n,t){"use strict";var o=t(re),i=o.Stat,r=o.Group,a={label:"What is ESPN's NFL Football Power Index?",href:"https://web.archive.org/web/20200922093936/https://www.espn.com/blog/statsinfo/post/_/id/123048/a-guide-to-nfl-fpi"},c={team:{label:"Team"}},p=new r("fpi",[new i(os,Y,0,null,is,Y,null,rs)],H),u={fpi:new r(null,[p,new r("fpi",[new i(l,k,1),new i(ne,"RK",0,{sortAscending:!0}),new i(dn,"TREND",null,{hideSort:!0,isTrend:!0}),new i("epaoffense","OFF",1),new i("epadefense","DEF",1),new i("epaspecialteams","ST",1)],fn),new r("fpi",[new i(Ln,"SOS",0,{sortAscending:!0}),new i(En,Dn,0,{sortAscending:!0}),new i(Fn,"AVGWP",0,{sortAscending:!0})],"RANKS")]),projections:new r(null,[new r("fpi",[new i(os,Y,0,null,is,Y,null,rs)]),new r("projections",[new i(mn,wn,1,null,hn),new i(Nn,Tn,1,{base100:!0}),new i(xn,jn,1,{base100:!0}),new i("probmakedivplayoffs","MAKE DIV%",1,{base100:!0}),new i("probmakeconfchamp","MAKE CONF%",1,{base100:!0}),new i(An,"MAKE SB%",1,{base100:!0}),new i(In,"WIN SB%",1,{base100:!0})])]),efficiencies:new r(null,[p,new r("efficiencies",[new i(Pn,h,1),new i(On,b,0,{sortAscending:!0})],Wn,!0),new r("efficiencies",[new i(Rn,h,1),new i(Vn,b,0,{sortAscending:!0})],Mn,!0),new r("efficiencies",[new i(Gn,h,1),new i(Bn,b,0,{sortAscending:!0})],Kn,!0),new r("efficiencies",[new i(zn,h,1),new i(Un,b,0,{sortAscending:!0})],es,!0)])},d=[{param:l,text:k,note:as,noteWithLink:a},{param:ss,text:"Projections",note:as,noteWithLink:a},{param:U,text:un,note:"fpi.efficiencies.note"}],f={view:l,sort:{fpi:{sort:le,dir:x},projections:{sort:"projections.projectedw",dir:x},efficiencies:{sort:ns,dir:x}}};e[s]={views:d,defaults:f,fpiHeaders:u,staticColumns:c}},"./src/pagetypes/fpi/config/utils.js":function(e,n,t){"use strict";var o=t(he)[ve],i=o[xe],a=o[ye];e[s]={Group:function(e,n,s,t){e&&(this[E]=e),n&&(this[g]=n),s&&(this[L]=s),t&&(this[en]=t)},Stat:function(e,n,s,t,o,l,p,u){var d=this,f=t||{},m=f[cs],g=f.sortAscending,w=f[de],h=f[ue],v=f[q],y=f[ge];d[c]=e,d[r]=n,d[tn]=s||0,d[be]=g?a:i,d[ue]=h,d[ge]=y,d[q]=v,m&&(d[cs]=!0),w&&(d[de]=!0),o&&(d[Ue]=o),u&&(d[Je]=u),l&&(d[I]=l),p&&(d[Ze]=p)}}},"./src/pagetypes/fpi/index.js":function(e,n,t){"use strict";var o=t("./src/pagetypes/fpi/PowerIndexPageType.js"),i=t("./src/pagetypes/pageWrapper.jsx"),r={components:{SponsoredLinks:t("./src/components/ui/Ad/SponsoredLinks.jsx")}};e[s]=i(o,r)},"./src/pagetypes/fpi/renderer.jsx":function(n,t,o){"use strict";var r=o(i),c=r[a],l=o(Me),p=o(d),u=o("./src/components/fpi/index.jsx"),f=o("./src/components/common/Layout/CoreLayout.jsx"),m=o("./src/components/ui/Cards/Card.jsx"),g=m.Card,w=m.Card__Content,h=o("./src/node_modules/espn-breakpoints/index.js"),v=h.TABLET[0],y=h.DESKTOP_LABEL,b=o(fe).WEBVIEW;n[s]=function(){var n=this[z]||c,s=n[P]||c,t=s.viewport,o=(s[Ae]||c)[Q]||c,i=o.modifier===b,a=t&&r.cnum(t.width,0,0),d=s.page||c,m=l(d,"meta.canonical"),h=l(d,"outbrain.id"),x=d.taboola,j=d[Fe],S=d.content||c,_=(n.components||c).SponsoredLinks,N=this[B]||r[Ee],T=(s.app||c).device,k=a?a<=v:!r.strMatch(T,y),A=l(n,"guest.profile");return p[e](f,{windowSize:a},p[e](g,null,p[e](w,null,p[e](u,{fpi:S,params:o,isMobile:k,handleRedirect:N,windowWidth:a,isWebview:i,guestProfile:A}))),_?p[e](_,{taboola:x,pageType:j,id:h,src:m,isHorizontal:!0}):null)}},6:function(e,n,t){t("./src/client/index.js"),e[s]=t("./src/pagetypes/fpi/index.js")}},[[6,"espnfitt"]]])}();

}
/*
     FILE ARCHIVED ON 09:39:36 Sep 22, 2020 AND RETRIEVED FROM THE
     INTERNET ARCHIVE ON 01:28:45 Jul 18, 2023.
     JAVASCRIPT APPENDED BY WAYBACK MACHINE, COPYRIGHT INTERNET ARCHIVE.

     ALL OTHER CONTENT MAY ALSO BE PROTECTED BY COPYRIGHT (17 U.S.C.
     SECTION 108(a)(3)).
*/
/*
playback timings (ms):
  captures_list: 163.489
  exclusion.robots: 0.067
  exclusion.robots.policy: 0.057
  RedisCDXSource: 1.204
  esindex: 0.009
  LoadShardBlock: 140.233 (3)
  PetaboxLoader3.datanode: 170.802 (5)
  load_resource: 122.253 (2)
  PetaboxLoader3.resolve: 85.225 (2)
*/