(function(){var nav=window.navigator||{};function AY2()
{function euU(v)
{v=parseInt(v);if (isNaN(v))
return "";else if (v<10)
return "&FL=FL_OLD";else
return "&FL=FL" +Math.min(v,11);}
var nm=nav.mimeTypes||{};var np=nav.plugins||[];if (nm["application/x-shockwave-flash"]&&nm["application/x-shockwave-flash"].enabledPlugin&&(np["Shockwave Flash 2.0"]||np["Shockwave Flash"])) {var qz4=np["Shockwave Flash 2.0"]?" 2.0":"";var E6q=np["Shockwave Flash" +qz4]["description"];return euU(E6q.split(" ")[2].replace(/\..*/,""));}
var ax=window.ActiveXObject;if (ax) {try {var axo=new ax("ShockwaveFlash.ShockwaveFlash.7");return euU(axo.GetVariable("$version").split(" ")[1].replace(/,.*/,""));} catch (e) {}
try {new ax("ShockwaveFlash.ShockwaveFlash.6");return euU(6);} catch (e) {}
try {var axo=new ax("ShockwaveFlash.ShockwaveFlash.3");return euU(axo.GetVariable("$version").split(" ")[1].replace(/,.*/,""));} catch (e) {}
try {new ax("ShockwaveFlash.ShockwaveFlash.3");return euU(3);} catch (e) {}}
return "";}
function os()
{var u=nav.userAgent.toLowerCase();var p=nav.platform.toLowerCase();if (p.indexOf("mac") !=-1)
return "MAC";if (u.indexOf("android") !=-1)
return "ANDROID";if (p.indexOf("linux") !=-1)
return "LINUX";if (u.indexOf("windows phone") !=-1)
return "WINPHONE";if (nav.platform.indexOf("iPad") !=-1)
return "IPAD";if (nav.platform.indexOf("iPod") !=-1)
return "IPOD";if (nav.platform.indexOf("iPhone") !=-1)
return "IPHONE";var win=parseFloat((u.match(new RegExp("windows nt ([0-9]+\\.[0-9]+)"))||{})[1])||0;if (win>=6.2)
return "WIN8";if (win>=6.1)
return "WIN7";if (win>=6.0)
return "WINVISTA";if (win>=5.1)
return "WINXP";if ((p.indexOf("win") !=-1)&&((u.indexOf("win 9x 4.90") !=-1)||(u.indexOf("windows me") !=-1)||(u.indexOf("98") !=-1)||(u.indexOf("nt 5.0") !=-1)||(u.indexOf("nt") !=-1)))
return "WIN_OLD";return "OTHER";}
function zpU()
{var a=[0,0,800,600,1024,768,1280,1024,1600,1200,0,0,640,480,720,480,0,0,0,0,1152,864,1280,720,1280,768,1280,960,1280,800,1280,854,0,0,1400,1050,1440,900,1680,1050,1920,1080,1920,1200,2048,1536,2560,1600,0,0,0,0,768,1024,1360,768,1600,900,320,480,480,800,1366,768,800,480,960,540,960,640,1024,600,1024,640,1093,614,1136,640,1152,720,2560,1440,2880,1800];var s=window.screen||{};for (var i=0;(i<a.length)&&((a[i] !=s.width)||(a[i+1] !=s.height));i +=2);return "&RES=" +((i<a.length)?"RS" +(i/2):"OTHER") +"&RESW=" +s.width +"&RESH=" +s.height;}
var lang=(nav.language||nav.userLanguage||"").toLowerCase().substr(0,2);var url="&OS=" +os() +AY2() +"&JE=" +(((typeof(nav.javaEnabled) !="undefined")&&nav.javaEnabled())?1:0) +(lang?("&UL=" +lang):"") +zpU();if (window.CM8DispatcherApps&&(window.CM8DispatcherApps.length>0))
url=window.CM8DispatcherApps.shift() +url;if (window.CM8DispatcherAjax)
window.CM8DispatcherAjax(url);else
document.write("<script type='text/javascript' src='" +url +"'></scr"+"ipt>");})();