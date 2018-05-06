all : emacs.el emacs.html

clean :
	rm -f emacs.el emacs.html

copy : all
	cp emacs.org emacs.el emacs.html ${DIST}

emacs.el : emacs.org
	#On génère un emacs.el fonctionnel mais pas forcément bien formaté. Ce
	#fichier permet de paramètrer Emacs correctement, et de relancer une
	#nouvelle génération. (Le premier emacs.el est écrasé par le deuxième)
	emacs --batch $< -f org-babel-tangle
	emacs --batch -l emacs.el $< -f org-babel-tangle

emacs.html : emacs.org emacs.el
	emacs --batch -l emacs.el $< -f org-html-export-to-html
