-- configuration file for program `pp'
-- define window size

theform = {
  f = form,
  setcaption = function (value)
    _setcaption(theform.f, value)
  end
}

width = 200
height = x1+x2+x3;
stringa = 'UPPERCASE='..delphiuppercase('daniele teti')..
	'Nome: '..daniele.nome..
	'Cognome: '..daniele.cognome..
	'Età: '..daniele.eta..
	'Il nome completo è: '..daniele:fullname()
   
--theform:setcaption("Hello World")
_setcaption( form, "ciao mondo" )   