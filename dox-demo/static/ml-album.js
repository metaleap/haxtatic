"use strict";

var mlBack = -1|0;
var mlNext = 1|0;
var mlticking = false;
var mlhasui = true;
var mlmaxscroll = +(1.0);
var mlscrolly = +(0.0);
var mlfull = null, mlfullstyle = null;
var mlfullheadln = null, mlfullheadlnstyle = null;
var mlhasfull = false;
var isuserclick = false;
var iscontain = true, isdynalbum = false;
var numimages = 0|0, numloaded = 0|0;
var mlscrollperc = "0%";
var mlscrollalt = "50%";
var mlscrollhlop = "1";
var mlgen = document.getElementById('ml_gen');
var mlnav = document.getElementById('__hax_htmlLinks__');
var mlnavhtml = mlnav.innerHTML;
var mlcannavback = true;
var mlfadeintitle = true;

var elradios = null;
var elcounts = null;
var numradios = 0|0;
var hasradios = false;
var currad = (-1)|0;

function mlOnRadioClick (ev) {
	for (var i = 0|0; i < numradios; i++) { if (elradios[i] == ev.target) { currad = i; break; } }
	if (currad>=0 && currad<numradios)
		mlscrollalt = Math.round(44.4 + ((+(currad)) * (22.2 / numradios))).toString() + "%";
	mlSetBgPicPos(true);
	mlfull = document.getElementById(ev.target.value);
	mlfullstyle = mlfull.style;
	mlhasfull = mlfull ? true : false;
	mlfullheadln = document.getElementById(ev.target.value+"_hl");
	mlfullheadlnstyle = mlfullheadln.style;
	if (isuserclick) {
		//	weirdly, this is the only way i figured out (in BOTH gecko and webkit) to have
		//	css-transition-of-bgpos-delta ALSO animate WHEN radioclick was user-mouse-click
		//	AND calling radio.scrollIntoView() is to be avoided (which on user-mouse-click
		//	would destroy the UX, unlike the keyboard/on-screen radio-selection modes)
		setTimeout(mlFocusCheckedFalseTrue, 33);
	} else {
		mlFocusChecked(true, true);
	}
	isuserclick = false;
}

function mlOnScroll () {
	mlticking = false;
	mlSetBgPicPos();
}

function mlOnLoad () {
	mlmaxscroll = +(Math.max(document.body.scrollHeight, document.body.clientHeight, document.body.offsetHeight) - window.innerHeight);
	mlmaxscroll = +(100.0 * (1.0 / mlmaxscroll));
	mlFocusChecked(false, true);
}

function mlOnLoadImg (img, rad, err) {
	numloaded = numloaded + 1;
	img.style.visibility = err ? "hidden" : "";
	if (err) img.style.height = "3.11em";
	rad.disabled = err;
	img.parentElement.style.backgroundImage = err ? "url('files/img/nopic.svg')" : "";
	if (numloaded == numimages) {
		// mlnav.style.backgroundColor = "";
		mlnav.innerHTML = mlnavhtml;
	} else {
		// mlnav.style.backgroundColor = (mlnav.style.backgroundColor ? "" : "#FFB800");
		var percdone = Math.round(((+(numloaded)) / (+(numimages))) * 100.0)|0;
		mlnav.innerHTML = ((percdone < 10) ? "0" : "") + percdone.toString() + "%&hellip;";
	}
	mlOnLoad();
	mlUpdateCounts();
}

function mlUpdateCounts () {
	for (var i = 0|0, l = elcounts.length|0; i < l; i++) {
		elcounts[i].innerHTML = numloaded;
	}
}

function mlOnResize () {
	document.getElementById('ml_album').style.minHeight = iscontain ? "" : (window.innerHeight+"px");
	mlOnLoad();
}

function mlZoomIn () {
	iscontain = false;
	document.getElementById('ml_main').className = "ml-pic-cover";
	mlOnResize();
}

function mlZoomOut () {
	iscontain = true;
	document.getElementById('ml_main').className = "ml-pic-contain";
	mlOnResize();
}

function mlCheckedRadio () {
	for (var i = 0|0; i < numradios; i++) {
		if (elradios[i].checked) {
			return i;
		}
	}
	return -1|0;
}

function mlSetBgPicPos (forceclear) {
	if (mlhasfull) {
		mlscrollperc = (forceclear || iscontain) ? mlscrollalt : (Math.round(mlscrolly * mlmaxscroll).toString() + "%");
		if (mlfullstyle.backgroundPositionX != mlscrollperc) mlfullstyle.backgroundPositionX = mlscrollperc;
		if (mlfullstyle.backgroundPositionY != mlscrollperc) mlfullstyle.backgroundPositionY = mlscrollperc;
		if (mlhasui && mlfadeintitle) {
			// so that css renders swiftly, probly better to give 0 and 1 directly
			// (~99% of the time in this use-case) instead of a computed 0.00000001 or 0.9999999
			mlscrollhlop = (mlscrolly<10.0) ? "0" : (mlscrolly>70.0) ? "1" : ((mlscrolly-10.0)*0.0166666666).toString();
			if (mlfullheadlnstyle.opacity != mlscrollhlop) mlfullheadlnstyle.opacity = mlscrollhlop;
		}
	}
}

function mlFocusChecked (scrollin, ensurepicscroll) {
	var i = -1|0;
	var el, img;
	if (hasradios && (el = elradios[i = mlCheckedRadio()])) {
		if (scrollin) {
			var isbottom = i > (numradios-3);
			elradios[isbottom ? Math.max(i-2, 0) : Math.min(i+2, numradios-1)].scrollIntoView(isbottom);
		}
		if (ensurepicscroll) {
			mlSetBgPicPos();
		}
		// this if-test here gives better gecko ux:
		if (el !== document.activeElement) { el.focus(); }
		if ((img = document.getElementById(el.id+'p')) && img.width && img.height) {
			var canzoom = 0.1 < +(Math.abs(((+(window.innerWidth)) / (+(window.innerHeight))) - ((+(img.width)) / (+(img.height)))));
			document.getElementById('ml_piccontain').style.visibility =
				document.getElementById('ml_piccover').style.visibility =
					canzoom ? "" : "hidden";
		}
	}
}
//	for setTimeout
function mlFocusCheckedFalseTrue () {
	mlFocusChecked(false, true);
}

function mlPhotoGoTo (stepby) {
	stepby = stepby|0;
	if (hasradios) {
		var cur = mlCheckedRadio()|0, nu = nu = (cur+stepby)|0;
		for (; nu!=cur; nu+=stepby) {
			if (nu<0) { nu = numradios-1; } else if (nu >= numradios) { nu = 0; }
			if (!elradios[nu].disabled) break;
		}
		elradios[nu].click();
		mlFocusChecked(true, true);
	}
}

function mlPhotoShow (i) {
	if (hasradios) {
		elradios[Math.min(numradios-1, Math.max(0, i)|0)|0].click();
		mlFocusChecked(false, true);
	}
}

function mlIsTargetTag (ev, tags) {
	return (tags.indexOf(ev.target.tagName.toUpperCase())|0) >= 0;
}

function mlShowUi (showui) {
	mlgen.className = ((mlhasui = showui) ? "" : "ml-album-hideui");
	mlFocusChecked(showui, true);
}

function mlToggleUi () {
	mlShowUi(!mlhasui);
}

function mlAlbumInit () {
	var i = 0|0;
	elradios = document.getElementsByName('ml_img_rad');
	numradios = elradios.length|0;
	hasradios = numradios>0;
	if (hasradios) {
		var r = Math.max(0, mlCheckedRadio()|0)|0;
		mlfull = document.getElementById(elradios[r].value);
		mlfullstyle = mlfull.style;
		mlhasfull = mlfull ? true : false;
		mlfullheadln = document.getElementById(elradios[r].value+"_hl");
		mlfullheadlnstyle = mlfullheadln.style;
		elradios[r].checked = true;
		for (i = 0; i < numradios; i++) {
			elradios[i].disabled = (i != r);
			elradios[i].addEventListener("click", mlOnRadioClick);
		}
	}

	var fakedelay = false;
	elcounts = document.getElementsByName('ml_piccount');
	mlUpdateCounts();
	var imgs = document.getElementsByTagName('IMG'), ellab, elrad, radid;
	var imgonload = function (img, rad, err) { return function() { mlOnLoadImg(imgs[img], document.getElementById(rad), err); }; }
	if (fakedelay)
		imgonload = function (img, rad, err) { return function() { setTimeout(function() { mlOnLoadImg(imgs[img], document.getElementById(rad), err); }, 123 * img); }; }
	for (i = 0; i < imgs.length|0; i++) {
		if ((radid = (ellab = imgs[i].parentElement).attributes['for']) && (elrad = document.getElementById(radid.value))) {
			if (numimages == 0) mlnav.innerHTML = "00%&hellip;"; // set this only if it'll be updated (and just once, obviously)
			numimages++;
			imgs[i].style.visibility = elrad.disabled ? 'hidden' : '';
		}
	}
	for (i = 0; i < imgs.length|0; i++) {
		if ((radid = (ellab = imgs[i].parentElement).attributes['for']) && (elrad = document.getElementById(radid.value))) {
			//	some browsers won't always fire onload after-the-fact when img
			//	already done at this point (eg from cache)---so we fire if .complete:
			if ((!fakedelay) && imgs[i].complete) {
				mlOnLoadImg(imgs[i], elrad, false);
			} else {
				imgs[i].addEventListener('load', imgonload(i, radid.value, false));
				imgs[i].addEventListener('error', imgonload(i, radid.value, true));
			}
			ellab.addEventListener('click', function() { isuserclick = true; });
		}
	}

	mlgen.addEventListener('click', function (ev) {
		if (!mlIsTargetTag(ev, ['IMG', 'INPUT', 'LABEL'])) {
			if (isdynalbum && document.documentElement.id) {
				mlGoBack(ev, true);
			} else {
				mlPhotoGoTo(mlNext);
				// mlFocusChecked(false, false);
			}
		} else {
		}
	});
	window.addEventListener('resize', mlOnResize);
	window.addEventListener('load', function () {
		// mlnav.style.backgroundColor = "";
		mlnav.innerHTML = mlnavhtml;
		mlOnLoad();
	});
	window.addEventListener('scroll', function (ev) {
		if (!mlticking) {
			mlticking = true;
			mlscrolly = +(window.scrollY);
			window.requestAnimationFrame(mlOnScroll);
		}
	});
	window.addEventListener('hashchange', function (ev) {

	});
	mlFocusChecked(false, true);
	if (window['mlAut\oCover'] && mlAutoCover) mlZoomIn();
}

if (document.getElementById('ml_album')) {
	mlAlbumInit();
}
