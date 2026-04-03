;; -*- mode: emacs-lisp ; fill-column: 80 ; indent-tabs-mode: t -*-

;;;; Configuration Emacs


;;; Prérequis et compilation

;; Compile les packages dans le bon ordre (dépendances). Recommandé par Helm.
;; https://github.com/emacs-helm/helm/wiki#upgrade-or-recompile
(setq async-bytecomp-allowed-packages '(all))


;; Réduit les temps de chargement (recommandé par la doc de use-package).
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Use-package installe automatiquement les packages manquants.
(setq use-package-always-ensure t)


;;; Généralités

;; Désactive l'écran d'accueil ; Emacs s'ouvre avec le buffer *scratch*.
(setq inhibit-startup-screen t)

;; Affiche le nom du fichier dans la barre de titre.
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;; En mode terminal, pas de barre de menus.
(unless window-system (menu-bar-mode -1))

;; En mode graphique, cache la barre d'outils.
(when window-system (tool-bar-mode -1))

;; En mode graphique, barre de défilement à droite.
(when window-system (set-scroll-bar-mode 'right))

;; Un buffer sans mode majeur s'ouvre en mode texte.
(setq-default major-mode 'text-mode)

;; Largeur de ligne à 80 colonnes (affecte fill-paragraph, whitespace-mode…).
(setq default-fill-column 80)

;; Indentation avec des tabulations de 4 caractères.
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)

;; En fin de buffer, les touches fléchées ne créent pas de nouvelle ligne.
(setq-default next-line-add-newlines nil)

;; Affiche la ligne et la colonne du curseur dans la mode line.
(line-number-mode t)
(column-number-mode t)

;; Active le raccourci clavier pour narrow-to-region.
(put 'narrow-to-region 'disabled nil)

;; Désactive la transient mark : la région est toujours active mais non
;; surlignée (ancien comportement d'Emacs).
(setq transient-mark-mode nil)

;; Met en évidence les paires de parenthèses, accolades, crochets…
(show-paren-mode 1)
(set-face-background 'show-paren-match "#b5d5ff")

;; Active UTF-8 à peu près partout. Voir discussion sur SO :
;; https://stackoverflow.com/a/2903256
(set-language-environment 'utf-8)

;; En mode graphique, lance Emacs en mode serveur (accès via emacsclient).
(when window-system (server-start))

;; Pour que require trouve les modules dans ce répertoire.
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Place les backups et les sauvegardes auto dans un répertoire temporaire
;; (/tmp sous Linux, %TEMP% sous Windows, /var/folders/… sous Mac).
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;; Raccourcis Fn

;; F1 : view-mode (lecture seule). C/S-F1 : revert-buffer.
;; À la place de view-order-manuals qui est normalement sur f1
(global-set-key (kbd "<f1>")	'view-mode)
(global-set-key (kbd "C-<f1>")	'revert-buffer)
(global-set-key (kbd "S-<f1>")	'revert-buffer)

;; F2 : bm-toggle. C-F2 / S-F2 : bm-next / bm-previous.
;; En mode terminal, S-F2 ouvre helm-bm (C-F2 ne fonctionne pas).
(global-set-key (kbd "<f2>")	'bm-toggle)
(global-set-key (kbd "C-<f2>")	'bm-next)
(global-set-key (kbd "S-<f2>")	'bm-previous)

(unless window-system
  (global-set-key (kbd "S-<f2>")	'helm-bm))

;; F3 : hl-line-mode (surlignage de la ligne courante).
(global-set-key (kbd "<f3>")	'hl-line-mode)

;; F4 : projectile-grep (recherche dans les fichiers du projet).
(global-set-key (kbd "<f4>")	'projectile-grep)

;; F5 : run-compilation. C/S-F5 : end-compilation.
(global-set-key (kbd "<f5>")	'run-compilation)
(global-set-key (kbd "C-<f5>")	'end-compilation)
(global-set-key (kbd "S-<f5>")	'end-compilation)

;; F6 : projectile-multi-occur. C/S-F6 : helm-occur.
(global-set-key (kbd "<f6>")	'projectile-multi-occur)
(global-set-key (kbd "C-<f6>")	'helm-occur)
(global-set-key (kbd "S-<f6>")	'helm-occur)

;; F8 : display-fill-column-indicator-mode. C/S-F8 : whitespace-mode.
(global-set-key (kbd "<f8>")	'display-fill-column-indicator-mode)
(global-set-key (kbd "C-<f8>")	'whitespace-mode)
(global-set-key (kbd "S-<f8>")	'whitespace-mode)

;; F9 : switch-theme (itère sur les thèmes préconfigurés, voir plus bas).
(global-set-key (kbd "<f9>")	'switch-theme)


;;; Navigation dans les buffers

;; Saute les buffers automatiques (*compilation*, *dired*, *helm*…).
;; navigate-nostar-buffer cherche le prochain buffer ne commençant pas par *.
(defun navigate-nostar-buffer (&optional previous)
  "Navigate to next \"no star\" buffer, or previous one if PREVIOUS is t."
  (let ((start-buffer (buffer-name))
	(step (if previous #'previous-buffer #'next-buffer)))
    (funcall step)
    (while
	(and (string-match-p "^\*" (buffer-name))
	     (not (equal start-buffer (buffer-name))))
      (funcall step))))

(defun navigate-next-nostar-buffer ()
  "Navigate to next \"no star\" buffer."
  (interactive)
  (navigate-nostar-buffer))

(defun navigate-previous-nostar-buffer ()
  "Navigate to previous \"no star\" buffer."
  (interactive)
  (navigate-nostar-buffer t))

;; Remplace les raccourcis C-x ← et C-x → par les variantes ci-dessus.
(global-set-key [remap next-buffer] 'navigate-next-nostar-buffer)
(global-set-key [remap previous-buffer] 'navigate-previous-nostar-buffer)


;;; Copier / coller

;; Le bouton du milieu colle au niveau du point, sans déplacer le curseur.
(setq mouse-yank-at-point t)

;; Le texte du presse-papier est ajouté au kill ring avant d'être remplacé.
(setq save-interprogram-paste-before-kill t)

;; Le texte surligné est automatiquement copié dans le kill ring.
(setq mouse-drag-copy-region t)


;;; Ajustements à l'OS

;; Ajustements selon la plateforme : police et raccourcis clavier.
(cond
 ((string-match "linux" system-configuration)
  (progn
	(message "Tweak Emacs for Linux")
	;; Police du terminal Xfce.
	(set-face-font
	 'default
	 "-UKWN-Adwaita Mono-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1")
	))
 ((string-match "apple" system-configuration)
  (progn
	(message "Tweak Emacs for Mac")

	;; Cmd → Meta, Alt → accès aux caractères spéciaux (|, ~, etc.).
	;; set-mac-keys / unset-mac-keys permettent de basculer entre un clavier
	;; Mac et un clavier PC.
	(defun set-mac-keys()
	  (interactive)
	  (setq mac-command-key-is-meta t)
	  (setq mac-command-modifier 'meta)

	  (setq mac-option-key-is-meta nil)
	  (setq mac-option-modifier nil)

	  (global-set-key (kbd "M-c") 'kill-ring-save)
	  (global-set-key (kbd "M-v") 'yank))

	(defun unset-mac-keys()
	  (interactive)

	  (setq mac-command-key-is-meta nil)
	  (setq mac-command-modifier 'super)

	  (setq mac-option-key-is-meta nil)
	  (setq mac-option-modifier 'meta)

	  (global-set-key (kbd "S-c") 'kill-ring-save)
	  (global-set-key (kbd "S-v") 'yank))

	(set-mac-keys))))


;;; Gestionnaire de paquets

;; Charge le gestionnaire de paquets avec les dépôts MELPA et Org.
(use-package package
  :config
  (add-to-list 'package-archives '("MELPA" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("Org" . "https://orgmode.org/elpa/") t)
  (package-initialize))


;;; Tree Sitter

;; Sources des grammaires Tree Sitter (parseurs compilés par langage).
;; Lancer my-ts-languages-setup une fois pour les installer.
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
					"master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (lua "https://github.com/Azganoth/tree-sitter-lua")
        (make "https://github.com/alemuller/tree-sitter-make")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (php "https://github.com/tree-sitter/tree-sitter-php"
			 "master" "php/src")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
			 "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
					"master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (zig "https://github.com/maxxnino/tree-sitter-zig")))

(defun my-ts-languages-setup ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75)))))

;; Active Tree Sitter dans tous les modes supportés.
(setq major-mode-remap-alist
'((bash-mode . bash-ts-mode)
  (c-mode . c-ts-mode)
  (c++-mode . c++-ts-mode)
  (css-mode . css-ts-mode)
  (go-mode . go-ts-mode)
  (html-mode . html-ts-mode)
  (js-mode . js-ts-mode)
  (javascript-mode . js-ts-mode)
  (json-mode . json-ts-mode)
  (python-mode . python-ts-mode)
  (ruby-mode . ruby-ts-mode)
  (rust-mode . rust-ts-mode)
  (toml-mode . toml-ts-mode)))


;;; Bookmarks

;; Marque-pages à la Visual Studio : ligne surlignée, navigation entre marques.
;; bm-cycle-all-buffers étend la navigation à tous les buffers ouverts.
(use-package bm
  :config
  (setq bm-cycle-all-buffers t)
  (custom-set-faces
   '(bm-face ((t (:background "#ffafff"))))))


;;; C & C++

;; Accolades ouvrantes alignées sous le mot-clé.
(c-set-offset (quote substatement-open) 0)

;; Ouvre les .h comme du C++, et non comme du C.
(setq auto-mode-alist (append '(("\\.h$" . c++-mode)) auto-mode-alist))


;;; CMake

;; Mode majeur pour les fichiers CMake. Désactive l'indentation automatique
;; (electric-indent-mode) qui gêne dans ce contexte.
(use-package cmake-mode
  :config (setq auto-mode-alist
				(append '(("CMakeLists\\.txt\\'" . cmake-mode))
						'(("\\.cmake\\'" . cmake-mode))
						auto-mode-alist))

  ;; Pas d'indentation automatique, en particulier après Enter
  :config (add-hook 'cmake-mode-hook (lambda () (electric-indent-mode -1))))


;;; Corfu

;; Complétion automatique en pop-up. Navigation avec Tab/S-Tab, insertion avec
;; RET. kind-icon ajoute des icônes dans le pop-up de complétion.
;; À tester et à terminer
(use-package corfu
  ;; :init
  ;; (global-corfu-mode)
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)
  (setq corfu-preview-current t)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("S-TAB" . corfu-previous)
              ("RET" . corfu-insert)))

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;;; Dired

;; Met en évidence la ligne courante dans Dired.
(add-hook 'dired-mode-hook 'hl-line-mode)

;; Sur Mac, Dired utilise GNU ls si disponible.
(when (eq system-type 'darwin)
  (if (file-executable-p "/usr/local/bin/gls")
	(setq insert-directory-program "/usr/local/bin/gls")))


;;; Ediff

;; Compare les fichiers côte à côte (et non l'un en dessous de l'autre).
(setq ediff-split-window-function 'split-window-horizontally)
;; Reste dans la fenêtre courante (n'ouvre pas de nouvelle fenêtre).
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;;; Graphviz

;; Mode majeur pour les fichiers .dot. C-c v pour prévisualiser le rendu.
(use-package graphviz-dot-mode
  :config
  (define-key graphviz-dot-mode-map (kbd "C-c v") 'graphviz-dot-preview))


;;; Helm

;; Framework de sélection interactive. Remplace M-x, C-x C-f, C-x b, etc.
;; Préfixe C-c h pour les commandes Helm (remplace C-x c).
(use-package helm
  :init
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x b" . helm-mini)
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (setq helm-ff-auto-update-initial-value t)
  (when (executable-find "curl") (setq helm-net-prefer-curl t))
  (setq helm-split-window-in-side-p t)
  (setq helm-move-to-line-cycle-in-source t)
  (custom-set-faces '(helm-ff-dotted-directory ((t (:foreground "DimGrey")))))
  (setq completion-styles '(flex))
  (helm-mode 1))


;;; Htmlize

;; Exporte un buffer en HTML en respectant la mise en évidence syntaxique.
(use-package htmlize)


;;; Idle Highlight Mode

;; Surligne toutes les occurrences du mot sous le curseur après un temps
;; d'inactivité. Activé dans tous les modes de développement.
(use-package idle-highlight-mode
  :hook
  (prog-mode . idle-highlight-mode))


;;; Lisp

;; Affiche les infos disponibles sur les fonctions/variables dans la zone
;; d'écho (eldoc-mode) pour les contextes Lisp.
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)


;;; Magit

;; On ne charge Magit que si git est trouvé, pour contourner un bug qui
;; paralyse Emacs au démarrage en l'absence de git.
(if (executable-find "git")
    (use-package magit
      :bind
      ("C-x g" . magit-status)))


;;; Markdown

(use-package mark-graf
  :config
  (setq auto-mode-alist (append '(("\\.md$" . mark-graf-mode)) auto-mode-alist)))


;;; MMM Mode

;; Fait cohabiter plusieurs modes majeurs dans un même buffer.
;; Ici : active shell-script-mode entre <<EOF et ^EOF (here documents)
;; dans les buffers text-mode.
(use-package mmm-mode
  :config
  (setq mmm-parse-when-idle t)
  (setq mmm-global-mode 'sometimes)
  (mmm-add-classes
   '((here-doc
	  :submode shell-script-mode
	  :front "<<EOF"
	  :back "^EOF")))
  (mmm-add-mode-ext-class 'text-mode nil 'here-doc))


;;; Org

;; On ne tronque pas les lignes ; indentation selon la profondeur.
(setq org-startup-truncated nil)
(setq org-startup-indented t)

;; Dans les blocs de code : coloration syntaxique, Tab pour indenter.
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Évalue les blocs de code et les liens shell/Lisp sans confirmation.
(setq org-confirm-babel-evaluate nil)
(setq org-confirm-shell-link-function nil)
(setq org-confirm-elisp-link-function nil)

;; À l'export : langue française, pas de numéros de section, titres sur 3
;; niveaux max, pas d'auteur.
(setq org-export-default-language "fr")
(setq org-export-with-section-numbers nil)
(setq org-export-headline-levels 3)
(setq org-export-with-author nil)


;;; Prettify Symbols

;; Remplace certaines séquences par un caractère composé à l'affichage
;; (≠, ≤, →, ⇒…). Les remplacements s'appliquent à tous les modes dérivés
;; de prog-mode. Les chaînes et commentaires ne sont pas affectés.
(add-hook 'prog-mode-hook
	    (lambda ()
	      (push '("/=" . ?≠) prettify-symbols-alist)
	      (push '("!=" . ?≠) prettify-symbols-alist)
	      (push '("==" . ?⩵) prettify-symbols-alist)
	      (push '("&&" . ?∧) prettify-symbols-alist)
	      (push '("||" . ?∨) prettify-symbols-alist)
	      (push '("<=" . ?≤) prettify-symbols-alist)
	      (push '(">=" . ?≥) prettify-symbols-alist)
	      (push '("<<" . ?≪) prettify-symbols-alist)
	      (push '(">>" . ?≫) prettify-symbols-alist)
	      (push '("::" . ?∷) prettify-symbols-alist)
	      (push '("->" . ?→) prettify-symbols-alist)
	      (push '("=>" . ?⇒) prettify-symbols-alist)
	      (push '("and" . ?∧) prettify-symbols-alist)
	      (push '("not" . ?¬) prettify-symbols-alist)
	      (push '("or" . ?∨) prettify-symbols-alist)
	      ))

(global-prettify-symbols-mode t)


;;; Projectile

;; Regroupe les fichiers d'un même projet (détection via .git, etc.).
;; C-t pour basculer entre fichiers associés (foo.h ↔ foo.c).
(use-package projectile
  :config
  (projectile-mode +1)
  :bind
  ("C-t" . projectile-find-other-file)
  :config
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :config
  (helm-projectile-on))


;;; Rainbow Delimiters

;; Colorie les parenthèses, accolades et crochets par paires imbriquées.
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))


;;; Rainbow Mode

;; Les chaînes représentant une couleur sont surlignées avec cette couleur.
(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))


;;; Related

;; Navigue facilement parmi les buffers qui vont ensemble (même base de nom).
;; Ex. : foo.h → foo.c → foo.org avec C-x <end>.
(use-package related
  :config
  (related-mode)
  :bind
  ("C-x <end>" . related-switch-buffer))


;;; Shell scripts

;; Les fichiers en .sh s'ouvrent avec shell-script-mode, indépendamment de leur
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))


;;; Souris

;; La molette défile de 2 lignes à la fois, à vitesse constante.
(setq mouse-wheel-scroll-amount '(2))
(setq mouse-wheel-progressive-speed nil)


;;; TabTab Minor Mode

;; Tab aligne sur l'arrêt de tabulation suivant ; S-Tab sur le précédent.
;; Activé initialement pour cmake-mode.
(defun prev-tab-to-tab-stop ()
  "Remove spaces or tabs to next defined tab-stop column."
  (interactive)
  (and abbrev-mode (= (char-syntax (preceding-char)) ?w)
	 (expand-abbrev))
  (let ((nexttab (indent-next-tab-stop (current-column) t)))
    (delete-horizontal-space t)
    (indent-to nexttab)))

(define-minor-mode tab-tab-mode
  "Tab-to-tab in both directions"
  :lighter " TTm"
  :keymap (let ((map (make-sparse-keymap)))
	      (define-key map (kbd "<tab>") 'tab-to-tab-stop)
	      (define-key map (kbd "S-<tab>") 'prev-tab-to-tab-stop)
	      map))

(add-hook 'cmake-mode-hook 'tab-tab-mode)


;;; Tcl

;; Ouvre les .tm comme des modules Tcl.
(setq auto-mode-alist (append '(("\\.tm$" . tcl-mode)) auto-mode-alist))


;;; Thèmes

;; standard-themes donne une vraie structure au thème par défaut d'Emacs, ce
;; qui évite des problèmes lors des changements de thème.
(use-package standard-themes
  :config (load-theme 'standard-light t))

;; Installés pour switch-theme, mais non chargés au démarrage (:defer t).
(use-package leuven-theme :defer t)
(use-package nord-theme   :defer t)

;; Itère sur standard-light → standard-dark → leuven → nord (touche F9).
(defun switch-theme ()
  "Itereate over some predefined themes"
  (interactive)
  (cond
   ((eq 'standard-light (car custom-enabled-themes))
    (load-theme 'standard-dark t))
   ((eq 'standard-dark (car custom-enabled-themes))
    (load-theme 'leuven t))
   ((eq 'leuven (car custom-enabled-themes))
    (load-theme 'nord t))
   (t
    (load-theme 'standard-light t))))


;;; Tramp

;; Utilise SSH pour les transferts de fichiers distants.
(setq tramp-default-method "ssh")


;;; Unfill

;; unfill-toggle (M-q) bascule entre fill et unfill sur la région ou le
;; paragraphe.
(use-package unfill
  :bind
  ("M-q" . unfill-toggle))


;;; Uniquify

;; Génère des libellés plus pertinents pour les buffers de mêmes noms.
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward))


;;; Whitespace

;; Visualise les espaces superflus : fins de ligne, lignes vides en début/fin
;; de fichier, lignes trop longues. Non activé par défaut pour éviter les diffs
;; malheureux.
(use-package whitespace
  :config
  (setq whitespace-style '(face trailing lines empty)))


;;; Which Key

;; Affiche les raccourcis disponibles après une touche de préfixe.
(use-package which-key
  :ensure nil
  :config
  (which-key-mode))


;;; XML

;; Remet en forme du XML. Fonction prise sur le site de Benjamin Ferrari :
;; http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    ;; split <foo><foo> or </foo><foo>, but not <foo></foo>
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
	(backward-char 2) (insert "\n") (setq end (1+ end)))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
	(backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end nil)
    (normal-mode))
  (message "All indented!"))
