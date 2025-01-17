// All rights reserved CheckM8 Inc. (c) 2009


if (typeof(window.CM8Page) == "undefined") {
	if (document.location && (document.location.search.indexOf('CM8Page=') != -1))
		window.CM8Page=document.location.search.replace(/.*CM8Page=/, "").replace(/&.*/, "");
	else
			window.CM8Page=String(Math.random()).slice(2);
}

window.CM8Page = String(window.CM8Page).replace(/[^a-zA-Z0-9\._]/g, "_");




var CM8XCat = decodeURIComponent(((CM8Cat.toLowerCase().indexOf("encoded:") == 0) ? CM8Cat.substring(8) : encodeURIComponent(CM8Cat))); 

window.CM8E = window.CM8E || {};
CM8E[CM8XCat] = CM8E[CM8XCat] || {};
var CM8ET = {
	cat: CM8XCat,
	page: CM8Page,
	server: CM8Server,
	lastRequestSerial: 999,
	pageViewStartSerial: 1,
	serialsData: {},
	spotlesses: {},
	placeHolders: [],
	lastPVCriterias: "",
	lastPVPHs: {},
	counts: {},


	clearRemovedPlaceHolders: function(elementToBeRemoved)
	{
		for (var cat in CM8E) {
			var otherCM8CE = CM8E[cat];
	
			for (var i = (otherCM8CE.placeHolders || []).length - 1; i >= 0; i--) {
				var ph = otherCM8CE.placeHolders[i];
				if (typeof(ph.element) != "string") {
					var elementInDom = function(scan)
						{
							if (! scan)
								return true;
							for (; scan && (scan != document.body); scan = scan.parentNode)
								if (scan == elementToBeRemoved) {
									scan = null;
									break;
								}
							return scan;
						};
					var placeHolderExists = elementInDom(ph.element) && elementInDom(ph.showingElement);
					
				}
				else {
					try {
						var placeHolderExists = eval(ph.element);
						
					}
					catch(e) {
						var placeHolderExists = false;
						
					}
				}
				if (! placeHolderExists) {
					if (ph.cleanup)
						ph.cleanup();
					otherCM8CE.placeHolders.splice(i, 1);
				}
			}
		}
	},
	
	
	initRequest: function(newPV)
	{
		this.lastRequestSerial++;
		
		if (newPV)
			this.pageViewStartSerial = this.lastRequestSerial;
		var now = new Date().getTime();
		this.serialsData[this.lastRequestSerial] = {cat: this.cat, serial: this.lastRequestSerial, sendTime: now, newPV: newPV, ads: {} };

		for (var i = 0; i < this.placeHolders.length; i++) {
			var ph = this.placeHolders[i];
			if (ph.lock_time && ((now - ph.lock_time) >20000)) {
				
				ph.lock_time = null;
			}
		}

		for (var otherCat in CM8E)
			if (CM8E[otherCat].clearRemovedPlaceHolders)
				CM8E[otherCat].clearRemovedPlaceHolders();
		
		return this.lastRequestSerial;
	},
	
	
	preparePH: function(serial, element, own, companion, format)
	{
		// TODO: REMOVE THIS!!!  Need to be done on the plugin !!!
		if (typeof(element) == "string")
			element = element.replace(/(\(function\(\){ var pluginData = \(window.CM8MainWindow \|\| window\).CM8E\[)([^\]]*)(\].pluginsData\[[0-9]*\]; return pluginData.pluginWindow.CM8PluginProxy\(plugin)s(Data.pluginObject,.*)/, "$1'$2'$3$4");
	
	
	
		if (typeof(element) != "string") {
			for (var parent = element; parent && (parent != document.body); parent = parent.parentNode) {
				for (var i = 0; i < this.placeHolders.length; i++) {
					var ph = this.placeHolders[i];
					if (ph.lock_time && (ph.element == parent) && ph.own) {
						
						return false; 
					}
				}
			}
			if (! parent) { 
				
				return false;
			}
			var kickedOut = [];
			if (own) {
				for (var i = 0; i < this.placeHolders.length; i++) {
					var ph = this.placeHolders[i];
					if (typeof(ph.element) != "string") {
						for (var parent = ph.element; parent && (parent != document.body); parent = parent.parentNode) {
							if (element == parent) {
								if (ph.lock_time) {
									
									return false;
								}
								if (! companion) {
									kickedOut.push(i);
									
									if (ph.cleanup)
										ph.cleanup();
									break;
								}
							}
						}
					}
				}
			}
			
			for (var i = kickedOut.length - 1; i >= 0; i--)
				this.placeHolders.splice(kickedOut[i], 1);
		}
		else {
			for (var i = 0; i < this.placeHolders.length; i++) {
				var ph = this.placeHolders[i];
				if (ph.lock_time && (ph.element == element)) {
					
					return false; 
				}
			}
		}
		
		this.placeHolders.push({
				element: element,
				showingElement: null,
				own: own,
				companion: companion,
				format: format,
				adId: null,
				serial: serial });
		
		return true;
	},
	
	
	buildRequest: function(serial, path, extraParams)
	{
		var noHistory = (String(extraParams).indexOf("no_history=true") != -1);
		
		var ignoredFormats = {};
		if (window.CM8IgnoredFormats && CM8IgnoredFormats.length)
			for (var i = 0; i < CM8IgnoredFormats.length; i++)
				ignoredFormats[CM8IgnoredFormats[i]] = true;
	
		var serialData = this.serialsData[serial];
		var recentSerials = [];
		if (! noHistory) {
			for (var serialScan in this.serialsData) {
				var serialScanData = this.serialsData[serialScan];
				var time = serialScanData.sendTime;
				if (serialScanData.receiveTime && time && (serialData.sendTime - time) <= 25000)
					recentSerials.push(serialScan);
			}
		}
		
		var history = [];
		if (! noHistory) {
			for (var adId in this.spotlesses)
				history.push(adId);
			for (var i = 0; i < this.placeHolders.length; i++) {
				var ph = this.placeHolders[i];
				if (ph.adId && (! ignoredFormats[ph.format]))
					history.push(ph.adId);
			}
		}
		
		if (serialData.newPV) {
			this.lastPVCriterias = "";
			this.lastPVPHs = {};
			this.counts = {};
		}
		
		var xPH = "";
		if (! noHistory)
			for (var format in this.lastPVPHs)
				if (! ignoredFormats[format])
					xPH += format + "," + this.lastPVPHs[format] + ",";
		
		var dynamicDetections = (function(){function getTime()
{var vuU=new Date();var Hz4=new String(vuU.getYear() +1900);for(i=4-Hz4.length;i>0;i--)
Hz4="0" +Hz4;var MM=new String(vuU.getMonth()+1);if (MM<1||MM>12)
MM=1;if (MM.length<2)
MM="0" +MM;var DD=new String(vuU.getDate());if (DD<1||DD>31)
DD=1;if (DD.length<2)
DD="0" +DD;var HH=new String(vuU.getHours());if (HH<0||HH>24)
HH=1;if (HH.length<2)
HH="0" +HH;return "&DATE=" +Hz4 +MM +DD +"&HOUR=" +HH;}
function DE4()
{var w,h;if (self.innerWidth) {w=self.innerWidth;h=self.innerHeight;}
else if (document.documentElement&&document.documentElement.clientWidth) {w=document.documentElement.clientWidth;h=document.documentElement.clientHeight;}
else if (document.body.clientWidth) {w=document.body.clientWidth;h=document.body.clientHeight}
else {w=1024;h=768;}
var ypU;if (w>1200)
ypU="WR_E";else if (w>1000)
ypU="WR_D";else if (w>800)
ypU="WR_C";else if (w>600)
ypU="WR_B";else
ypU="WR_A";return "&WIDTH=" +w +"&HEIGHT=" +h +"&WIDTH_RANGE=" +ypU;}
var nav=window.navigator||{};function AY2()
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
var lang=(nav.language||nav.userLanguage||"").toLowerCase().substr(0,2);window.CM8GetLocation=function()
{var uuU=[];var loc=document.location.href;for (var i=0;i<loc.length;i++) {var code=loc.charCodeAt(i);if ((code>=256)||(code==32))
uuU.push(encodeURIComponent(loc.substr(i,1)));else if (code==92)
uuU.push("\\\\");else if (code==63)
uuU.push("\\q");else if (code==38)
uuU.push("\\a");else if (code==37)
uuU.push("\\p");else if (code==35)
uuU.push("\\s");else if (code==34)
uuU.push("\\w");else if (code==39)
uuU.push("\\u");else if (code==60)
uuU.push("\\l");else if (code==62)
uuU.push("\\g");else
uuU.push(loc.substr(i,1));}
return uuU.join("");};return ("&LOC=" +CM8GetLocation().substr(0,1000) +DE4() +getTime() +"&OS=" +os() +AY2() +"&JE=" +(((typeof(nav.javaEnabled) !="undefined")&&nav.javaEnabled())?1:0) +(lang?("&UL=" +lang):"") +zpU() +"&ORD=" +String(Math.random()).slice(2));})();
		
		serialData.url =
				((document.location.protocol=="https:")?"https:":"http:") +
				"//" + this.server + path + "?" +
				"cat=" + encodeURIComponent(serialData.cat).replace(/%2C/gi, ",") +
				(this.page ? "&page=" + this.page : "") +
				"&serial=" + serial + ":" + this.pageViewStartSerial + ":" +
					String.fromCharCode(65 + (this.counts.total ? 1 : 0) + (this.counts.rm ? 2 : 0) + (this.counts.ph ? 4 : 0)) +
				(recentSerials.length ? "&recent_serials=" + recentSerials.join(",") : "") +
				(history.length ? "&history=" + history.join(",") : "") +
				(xPH ? "&x_place_holders=" + xPH : "") +
				(this.lastPVCriterias ? "&criterias=" + this.lastPVCriterias : "") +
				(serialData.newPV ? "&same_pv=false" : "") +
				"&" + dynamicDetections + "&" +
				extraParams;
		
		return serialData.url;
	},
	

	requestReceived: function(serial, spotlesses, criterias, placeHolders, counts)
	{
		if (! this.serialsData[serial]) {
			
			return;
		}
			
		this.serialsData[serial].receiveTime = new Date().getTime();
		
	
		for (var spotlessIndex = 0; spotlessIndex < spotlesses.length; spotlessIndex++) {
			var spotless = spotlesses[spotlessIndex];
			var spotlessData = this.spotlesses[spotless];
			
			if (spotlessData && spotlessData.cleanup)
				spotlessData.cleanup();
			this.spotlesses[spotless] = {};
		}
	
		if (serial >= this.pageViewStartSerial) {
			this.lastPVCriterias += criterias;
			
			this.lastPVPHs = this.lastPVPHs || {};
			var phsList = placeHolders ? placeHolders.split(',') : [];
			for (var i = 0; i < phsList.length; i+=2) {
				var format = phsList[i];
				if (format)
					this.lastPVPHs[format] = (this.lastPVPHs[format] || 0) + parseInt(phsList[i+1]);
			}
			
			counts = counts.charCodeAt(0) - 65;
			this.counts.total = this.counts.total || ((counts & 1) != 0);
			this.counts.rm    = this.counts.rm    || ((counts & 2) != 0);
			this.counts.ph    = this.counts.ph    || ((counts & 4) != 0);
		}
	
		for (var i = 0; i < this.placeHolders.length; i++) {
			var ph = this.placeHolders[i];
			if (ph.serial == serial)
				ph.lock_time = null;
		}			
	},
	
	
	bannerShowing: function(serial, showingElement, format, adId, ph)
	{
		// TODO: REMOVE THIS!!!  Need to be done on the plugin !!!
		if (typeof(showingElement) == "string")
			showingElement = showingElement.replace(/(\(function\(\){ var pluginData = \(window.CM8MainWindow \|\| window\).CM8E\[)([^\]]*)(\].pluginsData\[[0-9]*\]; return pluginData.pluginWindow.CM8PluginProxy\(plugin)s(Data.pluginObject,.*)/, "$1'$2'$3$4");
	
	
	
		if (typeof(showingElement) != "string") {
			for (var parent = showingElement; parent != document.body; parent = parent.parentNode)
				if (! parent) {
					
					return false;
				}
		}
		else {
			try {
				var x = eval(showingElement);
				if (! x) {
					
					return false;
				}
			}
			catch(e) {
				
				return false;
			}
		}
			
		if (! ph) {
			var phs = [];
			for (var i = 0; i < this.placeHolders.length; i++) {
				ph = this.placeHolders[i];
				if (ph.serial == serial)
					phs.push(ph);
			}
			
			if (phs.length == 1) {
				ph = phs[0];
				
			}
			else if (typeof(showingElement) != "string") {

				for (var i = 0; i < phs.length; i++) {
					ph = phs[i];
					var phElement = ph.element;
					if (phElement && (phElement.tagName == 'SCRIPT') && phElement.nextSibling) {
						if (phElement.nextSibling.id && (phElement.nextSibling.id.indexOf("CM8_FORMAT_") == 0))
							phElement = phElement.nextSibling;
						if (phElement.nextSibling == showingElement) {
							
							break;
						}
					}
					ph = null;
				}

				if (! ph) {
					var bestDistance;
					for (var i = 0; i < phs.length; i++) {
						var candidatePh = phs[i];
						var phElement = candidatePh.element && ((candidatePh.element.tagName == 'SCRIPT') ? candidatePh.element.parentNode : candidatePh.element);
						for (var scan = showingElement, distance = 0;
						     scan && (scan != document.body) && (scan != phElement) && ((! ph) || (distance < bestDistance));
						     scan = scan.parentNode, distance++);
						if (scan == phElement) {
							
							ph = candidatePh;
							bestDistance = distance;
						}
					}
				}
			}
			else {
				ph = null;
				for (var i = 0; i < phs.length; i++) {
					ph = phs[i];
					if (ph.element == showingElement) {
						
						break;
					}
					ph = null;
				}
			}
		}
		
		if (ph) {
			ph.showingElement = showingElement;
			ph.format = format;
			ph.adId = adId;
			ph.own = true;
			ph.used = true;
			
			return true;
		}
		
		
		return false;
	},
	
	
	removeAd: function(adId)
	{
		var adData = this.spotlesses[adId];
		if (adData) {
			
			if (adData.cleanup)
				adData.cleanup();
			delete this.spotlesses[adId];
			return true;
		}
		
		for (var i = 0; i < (this.placeHolders || []).length; i++) {
			var ph = this.placeHolders[i];
			if (ph.adId == adId) {
				
				if (ph.cleanup)
					ph.cleanup();
				while (ph.element.lastChild)
					ph.element.removeChild(ph.element.lastChild);
				this.placeHolders.splice(i, 1);
				return true;
			}
		}		

		
		return false
	}
};
for (var CM8ETP in CM8ET)
	if ((typeof(CM8ET[CM8ETP]) == 'function') || (! CM8E[CM8XCat][CM8ETP]))
		CM8E[CM8XCat][CM8ETP] = CM8ET[CM8ETP];

(function(CM8CE){

	function CM8EncodeProfile(profile)
{
	var attrs = (profile || "").split("&");
	for (var i = 0; i < attrs.length; i++) {
		var index = attrs[i].indexOf("=");
		if (index == -1)
			index = attrs[i].length;
		var attr = attrs[i].substr(0, index);
		var value = attrs[i].substr(index + 1);
		if (attr) {
			attr = ((attr.toLowerCase().indexOf("encoded:") == 0) ? attr.substring(8) : encodeURIComponent(attr));
		}
		if (value) {
			value = ((value.toLowerCase().indexOf("encoded:") == 0) ? value.substring(8) : encodeURIComponent(value)).replace(/%2C/g, ",");
		}
		attrs[i] = attr + ((index < attrs[i].length) ? "=" : "") + value;
	}
	return ((attrs.length>0)?"&":"")+attrs.join("&");
}

	var serial = CM8CE.initRequest(false);
	var CM8CES = CM8CE.serialsData[serial];
		
	if ((typeof(CM8RichMedia) != "undefined") &&
	    ((CM8RichMedia.toString().toLowerCase() == "no") ||
		 (CM8RichMedia.toString().toLowerCase() == "false")))
		var rm = "";
	else
		var rm = "r";
	
	function preparePhFormat(PHE)
	{
		var PHF = PHE.title;
		if (PHF) {
			PHE.title = "";
			PHF = decodeURIComponent(((PHF.toLowerCase().indexOf("encoded:") == 0) ? PHF.substring(8) : encodeURIComponent(PHF)));
			PHE.CM8RawFormat = PHF;
			PHE.CM8Format = PHF.replace(/:.*/, "");
		}
		if (! PHE.CM8Format) {
			PHE.CM8Format = "";
			PHE.CM8RawFormat = "";
		}
	}
	
	CM8CE.CM8ErrorPopup = CM8CE.CM8ErrorPopup || '';
	
	CM8CES.formatsPHs = {};
	
	window.CM8ShowAd = function(F, I)
	{
		if ((!CM8CES) || (! CM8CES.CM8Titles))
			return false;
		
		var PHA = document.getElementsByTagName("DIV");
		for (var PHI = 0; PHI < PHA.length; PHI++) {
			var PHE = PHA[PHI];
			if (PHE.id.indexOf("CM8ShowAd") == 0)
				preparePhFormat(PHE);
		}
		
		F = decodeURIComponent(((F.toLowerCase().indexOf("encoded:") == 0) ? F.substring(8) : encodeURIComponent(F)));
		
		var currentScript = ((function(parentElement)
{
	if ((parentElement.tagName == "SCRIPT") && (parentElement.src.indexOf("http://sagedigital.checkm8.com") == 0))
		return parentElement;
	for (var childElement = parentElement.lastChild; childElement; childElement = childElement.prevSibling) {
		var recursiveResult = arguments.callee(childElement);
		if (recursiveResult)
			return recursiveResult;
	}
})(document.body) ||
(function(parentElement)
{
	return ((parentElement.tagName != 'SCRIPT') && parentElement.lastChild) ? arguments.callee(parentElement.lastChild) : parentElement;
})(document.body));
		if (! CM8CE.preparePH(CM8CE.lastRequestSerial, currentScript.parentNode, false, false, F))
			return false;

		if (document.getElementById('CM8_FORMAT_' + F) == null)
			document.write('<DIV ID="CM8_FORMAT_' + F + '" STYLE="display:none"></DIV>');
		
		if (CM8CE.formats1Call && (! CM8CE.formats1Call[F]))
			return false;
		if (CM8CE.xformats1Call && CM8CE.xformats1Call[F])
			return false;
		
		CM8CES.formatsPHs[F] = (CM8CES.formatsPHs[F] || 0) + 1;
		
		CM8CE.lastPVPHs[F] = (CM8CE.lastPVPHs[F] || 0) + 1;
	
		var C='CM8ShowAd('+F+((typeof(I)=='number')?(','+I):'')+')';
	
		for (var D = 0; CM8CES.CM8Titles[D] && (CM8CES.CM8Titles[D][0] != F); D++);
		if (! CM8CES.CM8Titles[D])
			return false;
		
		CM8CES.CM8Titles[D][3]++;
	
		var E = CM8CES.CM8Titles[D][4];
	
		if (typeof(I) != 'number')
			for (I = 0; E[I] && E[I].used; I++);
		else if (I < 1) {
			CM8CE.CM8ErrorPopup += '\\n'+C+' - Index '+I+' must be positive (1 and above)';
			return false;
		}
		else
			I--;
	
		if (E[I] && E[I].used) {
			CM8CE.CM8ErrorPopup += '\\n'+C+' - Index '+I+' is called twice';
			return false;
		}
	
		if (E[I] && (! E[I].used)) {
			E[I].used = true;
			E[I][0]();
			return true;
		}
		
		return false;
	};

	CM8CE.sumPHsOf1Call = function()
	{
		var S = '';
		for (var X in CM8CES.formatsPHs) {
			var Y=CM8CES.formatsPHs[X];
			S+='&S'+Y+','+((CM8CE.lastPVPHs[X]||0)-Y)+','+encodeURIComponent(X);
		}

		if (! CM8CE.counts.ph) {
			CM8CE.counts.ph = true;
			S+='&P';
		}
			
		return S;
	};

	function prepare1CallProfile(profile)
	{
		delete CM8CE.formats1Call;
		delete CM8CE.xformats1Call;

		var attrs = profile.split("&");
		for (var i = 0; i < attrs.length; i++) {
			var index = attrs[i].indexOf("=");
			if (index != -1) {
				var attr = attrs[i].substr(0, index);
				if (attr == "format")
					var member = "formats1Call";
				else if (attr == "xformat")
					var member = "xformats1Call";
				else
					var member = null;
				if (member) {
					var formats = attrs[i].substr(index + 1).split(",");
					for (var j = 0; j < formats.length; j++) {
						var format = formats[j];
						if (format) {
							CM8CE[member] = CM8CE[member] || {};
							CM8CE[member][decodeURIComponent(format)] = true;
						}
					}
				}
			}
		}
		
		return profile;
	}

	document.write("<SCR" + "IPT TYPE='text/javascript' SRC='" +
			CM8CE.buildRequest(
					serial,
					"/adam/detect",
					"req=f" + rm + "&" +
					prepare1CallProfile(CM8EncodeProfile(window.CM8Profile))) +
			"'></SCR" + "IPT>");

})(CM8E[CM8XCat]);
