#!/bin/sh

rm -f emacs.el emacs.html travis.el

cat > travis.el <<EOF
(require 'package)
(add-to-list 'package-archives '("MELPA" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("Org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(setq org-confirm-babel-evaluate nil)
EOF

grep -A 100 'defun my-setup' emacs.org | grep -B 100 END_SRC | grep -v '^\#' | sed 's/    //' >> travis.el

emacs --batch -l travis.el -f my-setup
emacs --batch -l travis.el emacs.org -f org-babel-tangle
emacs --batch -l emacs.el emacs.org -f org-babel-tangle
emacs --batch -l emacs.el emacs.org -f org-html-export-to-html
