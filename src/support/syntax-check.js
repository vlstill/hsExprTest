// (c) 2015 Juraj Major
// (c) 2017 Vladimír Štill

// vim: set noexpandtab

// Pouziti:
// * nalinkovat tento skript do zadani (lze vicekrat)
// * vlozit var predmet = 'xy123' do zadani
// * v zadani ulohy, kterou chci kontrolovat, volat syntax_check(id_otazky)

// volany skript musi posilat
// Access-Control-Allow-Origin: https://is.muni.cz
// Access-Control-Allow-Methods: POST

var syntax_check;

if (typeof syntax_check !== 'function') {
	var skripty = {
		ib015: 'https://www.fi.muni.cz/~xstill/hs/proxy.cgi',
		test: 'https://www.fi.muni.cz/~xmajor/cgi-bin/cors.pl'
	}, otazky = {};

	var onl = window.onload || function() {};
	window.onload = function() {
		onl();
		var url = typeof predmet == 'string' && predmet in skripty ? skripty[predmet] : skripty.test;
		var pole = document.getElementsByTagName('textarea');
		for (var n in otazky) {
			// cilova textarea je n-ta v poradi
			var txa = pole[n],
				wrap = document.createElement('p'),
				btn = document.createElement('input'),
				res = document.createElement('span');
			btn.type = 'button';
			btn.value = 'Zkontrolovat syntax';
			(function(n, txa, btn, res) {
				btn.onclick = function() {
					var xhr = new XMLHttpRequest(),
						data = 'id=' + otazky[n] + '&odp=' + encodeURIComponent(txa.value);
					if ('withCredentials' in xhr) {
						xhr.open('POST', url, true);
						xhr.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
						xhr.onreadystatechange = function() {
							if (this.readyState == 4 && this.status == 200) {
								var text, barva;
								var resp = "<pre>"
								           + this.responseText.replace( /^n?ok~~/, '' ).replace( /check_id=.*/, '' )
								           + "</pre>";
								if (/^ok/.test(this.responseText)) {
									text = 'V pořádku.\n' + resp;
									barva = 'green';
								} else {
									text = 'Vstup obsahuje syntaktické nebo typové chyby nebo překlep v názvu funkce.<br>'
									       + resp;
									barva = 'red';
								}
								res.innerHTML = text;
								res.style.color = barva;
							}
						};
						xhr.send(data);

						// odpocet 10 sekund do dalsi kontroly
						this.disabled = true;
						this.value += ' (10)';
						res.innerHTML = 'Probíhá dotazování na server...';
						res.style.color = 'gray';
						setTimeout(function() {
							var zbyva = parseInt(btn.value.match(/\((\d+)\)$/)[1], 10);
							if (zbyva > 1) {
								btn.value = btn.value.replace(/\(\d+\)$/, '(' + (zbyva - 1) + ')');
								setTimeout(arguments.callee, 1000);
							} else {
								btn.disabled = false;
								btn.value = btn.value.replace(/\s+\(\d+\)$/, '');
							}
						}, 1000);
					} else {
						this.disabled = true;
						this.value = 'Kontrola syntaxe není v tomto prohlížeči dostupná.';
					}
				};
			})(n, txa, btn, res);
			res.style.paddingLeft = '1em';

			wrap.appendChild(btn);
			wrap.appendChild(res);
			// textarea je zabalena v labelu
			txa.parentNode.insertBefore(wrap, txa.nextSibling);
		}
	}

	syntax_check = function(id) {
		// v okamziku volani funkce nase textarea jeste neexistuje, ale bude to hned ta nejblizsi
		otazky[document.getElementsByTagName('textarea').length] = id;
	};
}
