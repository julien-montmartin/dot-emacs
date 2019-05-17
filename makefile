all : emacs.el emacs.html

clean :
	rm -f emacs.el emacs.html

copy : all
	cp emacs.org emacs.el loader.el emacs.html ${DIST}

emacs.el : emacs.org
	#On génère un emacs.el fonctionnel mais pas forcément bien formaté. Ce
	#fichier permet de paramètrer Emacs correctement, et de relancer une
	#nouvelle génération. (Le premier emacs.el est écrasé par le deuxième)
	emacs --batch $< -f org-babel-tangle
	emacs --batch -l emacs.el $< -f org-babel-tangle

emacs.html : emacs.org emacs.el
	emacs --batch -l emacs.el $< -f org-html-export-to-html

travis.el :
	rm -f travis.el
	echo "(require 'package)"																		>> travis.el
	echo "(add-to-list 'package-archives '(\"MELPA\" . \"http://melpa.milkbox.net/packages/\") t)"	>> travis.el
	echo "(add-to-list 'package-archives '(\"Org\" . \"https://orgmode.org/elpa/\") t)"				>> travis.el
	echo "(package-initialize)"																		>> travis.el
	grep -A 100 'defun my-setup' emacs.org | grep -B 100 END_SRC | grep -v '^\#' | sed 's/    //'	>> travis.el

pages : travis.el
	#On installe les paquets nécessaire
	emacs --batch -l travis.el -f my-setup
	#On génère emacs.el en dux fois comme pour all
	emacs --batch $< -f org-babel-tangle
	emacs --batch -l emacs.el $< -f org-babel-tangle
