// (c) 2015 Juraj Major
// (c) 2017-2019 Vladimír Štill

// vim: set noexpandtab

// Pouziti:
// * nalinkovat tento skript do zadani (lze vicekrat)
// * vlozit var predmet = 'xy123' do zadani
// * v zadani ulohy, kterou chci kontrolovat, volat syntax_check(id_otazky)

// volany skript musi posilat
// Access-Control-Allow-Origin: https://is.muni.cz
// Access-Control-Allow-Methods: POST

var syntax_check, upload;

if (typeof syntax_check !== 'function') {
	var url = 'https://hesperia.fi.muni.cz/hint', otazky = {};

	var onl = window.onload || function() {};
	window.onload = function() {
		onl();
		var pole = document.getElementsByTagName('textarea');
		for (var n in otazky) {
			// cilova textarea je n-ta v poradi
			var txa = pole[n],
				wrap = document.createElement('p'),
                up = document.createElement('input'),
				res = document.createElement('span');
            up.type = 'file';

            if ( otazky[n] != null ) {
                var btn = document.createElement('input');
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
                wrap.appendChild(btn);
            }
            (function(txa, up) {
                up.addEventListener('change', function(evt) {
                        var file = evt.target.files[0];
                        if ( file ) {
                            var reader = new FileReader();
                            reader.onloadend = function ( ev ) {
                                txa.value = this.result;
                            };
                            reader.readAsText( file );
                        }
                    }, false );
            })(txa, up);

			res.style.paddingLeft = '1em';
            wrap.appendChild(up);
            wrap.appendChild( document.createElement('br') );
			wrap.appendChild(res);
			// insert the button right after the textarea
			txa.parentNode.insertBefore(wrap, txa.nextSibling);
		}
	}

	syntax_check = function(id) {
		// v okamziku volani funkce nase textarea jeste neexistuje, ale bude to hned ta nejblizsi
		otazky[document.getElementsByTagName('textarea').length] = id;
	};
    upload = function() { syntax_check( null ); };
}
