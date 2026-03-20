;; -*- mode: emacs-lisp ; fill-column: 80 -*-

;; Raccourci vers ce fichier de chargement via C-x r j l
(set-register ?l '(file . "~/.emacs"))

;; Raccourcis vers la configuration d'Emacs via C-x r j e
(set-register ?e '(file . "/path/to/small.el"))

;; Chargement du vrai .emacs
(load-file "/path/to/small.el")

;; Ci-dessous, ajouts de Custom
;;
