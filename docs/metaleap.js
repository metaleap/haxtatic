"use strict";

var mlMenuShow = document.getElementById("menushow")
var mlMenuHide = document.getElementById("menuhide");

window.addEventListener('hashchange', function (ev) {
	if ('pagetoc'==ev.newURL.substr((ev.newURL.indexOf('#')|0)+1)) {
		mlMenuHide.style.display = "inline-block";
		mlMenuShow.style.display = "none";
	} else {
		mlMenuShow.style.display = "inline-block";
		mlMenuHide.style.display = "none";
	}
});
