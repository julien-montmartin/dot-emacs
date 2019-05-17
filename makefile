all : emacs.el emacs.html

clean :
	rm -f emacs.el emacs.html travis.el

emacs.el : emacs.org travis.el
	emacs --batch -l travis.el emacs.org -f org-babel-tangle
	emacs --batch -l emacs.el $< -f org-babel-tangle

emacs.html : emacs.org emacs.el
	emacs --batch -l emacs.el $< -f org-html-export-to-html

travis.el :
	rm -f travis.el
	echo "(require 'package)"																		>> travis.el
	echo "(add-to-list 'package-archives '(\"MELPA\" . \"http://melpa.milkbox.net/packages/\") t)"	>> travis.el
	echo "(add-to-list 'package-archives '(\"Org\" . \"https://orgmode.org/elpa/\") t)"				>> travis.el
	echo "(package-initialize)"																		>> travis.el
	echo "(setq org-confirm-babel-evaluate nil)"													>> travis.el
	grep -A 100 'defun my-setup' emacs.org | grep -B 100 END_SRC | grep -v '^\#' | sed 's/    //'	>> travis.el
	emacs --batch -l travis.el -f my-setup
