"use strict";

function mlDynAlbum() {
	var img, lnk, imgs = document.getElementsByTagName('IMG'), h1 = document.getElementsByTagName('H1');
	var htmlOut = '', imgdesc, pagetitle = (h1.length|0) ? h1[0].innerText : document.title;
	var pics = [], i = 0|0, l = 0|0, s = "", sl = "";
	for (l = imgs.length|0; i<l; i++)
		if ('A'==(lnk = (img = imgs[i]).parentElement).tagName && 'ml_gen'==lnk.parentElement.id) {
			lnk.attributes['href'].value = "#pic:"+(pics.length|0).toString()+":"+lnk.attributes['href'].value;
			pics.push(img);
		}
	for (i = 0, l = pics.length|0, sl = l.toString(); i<l; i++) {
		imgdesc = pics[i].attributes['alt'] ? pics[i].attributes.alt.value : '';
		s = i.toString();
		htmlOut += (
			"<input id='mlir"+s+"' value='mlfp"+s+"' name='ml_img_rad' type='radio' /><label for='mlir"+s+"'>" +
			"<img src='"+pics[i].src+"' title='"+imgdesc+"' id='mlir"+s+"p' />" +
			"</label><div id='mlfp"+s+"' style=\"background-image: url('"+pics[i].src+"')\">" +
			"<div><i id='mlfp"+s+"_hl'>"+pagetitle+"</i> "+(i+1).toString()+"/"+sl+" "+
			"&mdash; <b>"+imgdesc+"</b></div></div>");
	}
	if (htmlOut) {
		var mlgen = document.getElementById('ml_gen');
		mlgen.innerHTML = "<h1>"+pagetitle+"</h1><div id='ml_album'>"+htmlOut+"</div>" + mlgen.innerHTML.substr((mlgen.innerHTML.indexOf('</h1>')|0)+5);
		document.getElementById('ml_flex').className = 'ml-album-dyn';
	}
}

function mlGoBack (ev, go) {
	if (mlcannavback) {
		history.back();
	} else if (go) {
		window.location.href = '#';
	}
	return (ev.returnValue = !(ev.cancelBubble = mlcannavback || go));
}

function mlGoOnHash (hash) {
	var splits = hash.substr((hash.indexOf('#')|0)+1).split(':'), sl = splits.length|0, i = 0|0;
	if (sl>2 && 'pic'==splits[0] && ((i = parseInt(splits[1])|0) >= 0)) {
		mlPhotoShow(i);
		document.documentElement.id = 'ml_hostalbum';
	} else
		document.documentElement.id = '';
}

mlfadeintitle = false;
if (!document.getElementById('ml_album')) {
	mlDynAlbum();
	if (document.getElementById('ml_album')) {
		document.getElementById('ml_picclose').style.display = 'inline-block';
		document.getElementById('ml_piccontrols').style.display = 'none';
		isdynalbum = true;
		mlAlbumInit();
		window.addEventListener('hashchange', function (ev) { mlGoOnHash(ev.newURL); });
		if (window.location.hash.startsWith('#pic:')) {
			mlcannavback = false;
			mlGoOnHash(window.location.hash);
		}
	}
}
