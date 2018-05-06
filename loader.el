;; -*- mode: emacs-lisp ; fill-column: 80 -*-

;; Raccourci vers ce fichier de chargement via C-x r j l
(set-register ?l '(file . "~/.emacs"))

;; Raccourcis vers la configuration d'Emacs via C-x r j e et C-x r j e
(set-register ?e '(file . "/path/to/emacs.el"))
(set-register ?o '(file . "/path/to/emacs.org"))

;; Chargement du vrai .emacs
(load-file "/path/to/emacs.el")
