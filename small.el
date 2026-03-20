;; -*- mode: emacs-lisp ; fill-column: 80 ; indent-tabs-mode: t -*-

;; Comme suggéré dans la doc de Helm, pour compiler les packages dans
;; de bonnes conditions (avec les dépendances dans le bon ordre).
;; https://github.com/emacs-helm/helm/wiki#upgrade-or-recompile

(setq async-bytecomp-allowed-packages '(all))


;; D'après la doc de use-package, réduit les temps de chargement.
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Use-package installe automatiquement les packages manquants.
(setq use-package-always-ensure t)


(setq inhibit-startup-screen t)

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

(unless window-system (menu-bar-mode -1))

(when window-system (tool-bar-mode -1))

(when window-system (set-scroll-bar-mode 'right))

(setq-default major-mode 'text-mode)

(setq default-fill-column 80)

(setq-default c-basic-offset 4
		tab-width 4
		indent-tabs-mode t)

(setq-default next-line-add-newlines nil)

(line-number-mode t)
(column-number-mode t)

(put 'narrow-to-region 'disabled nil)

(setq transient-mark-mode nil)

(when window-system (server-start))

;; Pour que require trouve les modules dans ce répertoire
(add-to-list 'load-path "~/.emacs.d/site-lisp/")


;; Place les backups et les sauvegardes auto dans un répertoire temporaire
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(add-hook 'dired-mode-hook 'hl-line-mode)


;; Sur Mac, Dired utilise GNU ls si disponible
(when (eq system-type 'darwin)
  (if (file-executable-p "/usr/local/bin/gls")
	(setq insert-directory-program "/usr/local/bin/gls")))


;; À la place de view-order-manuals qui est normalement sur f1
(global-set-key (kbd "<f1>")	'view-mode)
(global-set-key (kbd "C-<f1>")	'revert-buffer)
(global-set-key (kbd "S-<f1>")	'revert-buffer)

(global-set-key (kbd "<f2>")	'bm-toggle)
(global-set-key (kbd "C-<f2>")	'bm-next)
(global-set-key (kbd "S-<f2>")	'bm-previous)

(unless window-system
  (global-set-key (kbd "S-<f2>")	'helm-bm))

(global-set-key (kbd "<f3>")	'hl-line-mode)

(global-set-key (kbd "<f4>")	'projectile-grep)

(global-set-key (kbd "<f5>")	'run-compilation)
(global-set-key (kbd "C-<f5>")	'end-compilation)
(global-set-key (kbd "S-<f5>")	'end-compilation)

(global-set-key (kbd "<f6>")	'projectile-multi-occur)
(global-set-key (kbd "C-<f6>")	'helm-occur)
(global-set-key (kbd "S-<f6>")	'helm-occur)

(global-set-key (kbd "<f8>")	'display-fill-column-indicator-mode)
(global-set-key (kbd "C-<f8>")	'whitespace-mode)
(global-set-key (kbd "S-<f8>")	'whitespace-mode)

(global-set-key (kbd "<f9>")	'switch-theme)


(set-language-environment 'utf-8)


;; Saute les buffers automatiques (compilation, dired, helm, etc)
(defun navigate-nostar-buffer (&optional previous)
  "Navigate to next \"no star\" buffer, or previous one if PREVIOUS is t."
  (let ((start-buffer (buffer-name)))
    (cl-flet ((next-f () (if previous (next-buffer) (previous-buffer))))
	(next-f)
	(while
	    (and (string-match-p "^\*" (buffer-name))
		 (not (equal start-buffer (buffer-name))))
	  (next-f)))))

(defun navigate-next-nostar-buffer ()
  "Navigate to next \"no star\" buffer."
  (interactive)
  (navigate-nostar-buffer))

(defun navigate-previous-nostar-buffer ()
  "Navigate to previous \"no star\" buffer."
  (interactive)
  (navigate-nostar-buffer t))

(global-set-key [remap next-buffer] 'navigate-next-nostar-buffer)
(global-set-key [remap previous-buffer] 'navigate-previous-nostar-buffer)


(setq mouse-yank-at-point t)

;; Le texte du presse-papier est ajouté au kill ring avant d'être remplacé.
(setq save-interprogram-paste-before-kill t)

;; Le texte surligné est automatiquement copié dans le kill ring
(setq mouse-drag-copy-region t)

;; Met en évidence les paires de parenthèses, accolades, crochets...
(show-paren-mode 1)
(set-face-background 'show-paren-match "#b5d5ff")

;; Ajustements à l'OS
(cond
 ((string-match "linux" system-configuration)
  (progn
	(message "Tweak Emacs for Linux")
	(set-face-font 'default "-UKWN-Adwaita Mono-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1")
	))
 ((string-match "apple" system-configuration)
  (progn
	(message "Tweak Emacs for Mac")

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


;; Charge le gestionnaire de paquets avec les dépots Melpa et Org.
(use-package package
  :config (add-to-list 'package-archives
					   '("MELPA" . "http://melpa.org/packages/") t)
  :config (add-to-list 'package-archives
					   '("Org" . "https://orgmode.org/elpa/") t)
  :config (package-initialize))


(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (lua "https://github.com/Azganoth/tree-sitter-lua")
        (make "https://github.com/alemuller/tree-sitter-make")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
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


;; Gestion des marque-pages
(use-package bm
  :config
  (setq bm-cycle-all-buffers t)
  (custom-set-faces
   '(bm-face ((t (:background "#ffafff"))))))


;; Accolades ouvrantes alignées sous le mot clé
(c-set-offset (quote substatement-open) 0)

;; Ouvre les .h comme du C++, et non comme du C
(setq auto-mode-alist (append '(("\.h$" . c++-mode)) auto-mode-alist))


(use-package cmake-mode
  :config (setq auto-mode-alist
				(append '(("CMakeLists\\.txt\\'" . cmake-mode))
						'(("\\.cmake\\'" . cmake-mode))
						auto-mode-alist))

  ;; Pas d'indentation automatique, en particulier après Enter
  :config (add-hook 'cmake-mode-hook (lambda () (electric-indent-mode -1))))


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


;; Ediff compare les fichiers cote à cote (et non l'un en dessous de l'autre).
(setq ediff-split-window-function 'split-window-horizontally)
;; Reste dans la fenêtre courrante (n'ouvre pas de nouvelle fenêtre).
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


(use-package graphviz-dot-mode
  :config
  (define-key graphviz-dot-mode-map (kbd "C-c v") 'graphviz-dot-preview))


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


(use-package htmlize)


;; Surligne toutes les occurrences du mot se trouvant sous le curseur.
(use-package idle-highlight-mode
  :hook
  (prog-mode . idle-highlight-mode))


(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)


;; On ne charge magit que si git est trouvé pour contourner un bug qui paralyse
;; Emacs au démarrage.
(if (executable-find "git")
    (use-package magit
      :bind
      ("C-x g" . magit-status)))


;; Pour avoir le mode shell dans un here-document en mode texte.
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


(setq org-startup-truncated nil)
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-confirm-shell-link-function nil)
(setq org-confirm-elisp-link-function nil)
(setq org-export-default-language "fr")
(setq org-export-with-section-numbers nil)
(setq org-export-headline-levels 3)
(setq org-export-with-author nil)


;; Enjolive certaines séquences avec ce que certains appellent des "ligatures"
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


(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))


;; Les chaines représentant une couleur sont surlignées avec cette couleur
(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))


;; Pour naviguer facilement parmis les buffers qui vont ensemble
(use-package related
  :config
  (related-mode)
  :bind
  ("C-x <end>" . related-switch-buffer))


;; Les fichiers en .sh s'ouvrent avec shell-script-mode, indépendamment de leur
;; shebang.
;; TODO :
;; Cette ligne ne fonctionne pas quand elle se trouve au début de ce fichier ?

(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))


(setq mouse-wheel-scroll-amount '(2))
(setq mouse-wheel-progressive-speed nil)


;; Change le comportement de la touche Tab
;; - Tab ne sert plus à indenter la ligne, mais aligne le texte sur le prochain
;;   arrêt de tabulation
;; - Shift-Tab remplit la fonction opposée, et aligne le texte sur l'arrêt de
;;   tabulation précédent
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


(setq auto-mode-alist (append '(("\.tm$" . tcl-mode)) auto-mode-alist))


;; Le thème par défaut n'en est pas un à proprement parler. Il gagne à être
;; remplacé par un vrai thème d'apparence similaire.
(use-package standard-themes
  :config (load-theme 'standard-light t))

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


(setq tramp-default-method "ssh")



;; La fonction fill permet de découper correctement les lignes trop longues, et
;; le paquet unfill fournit la fonction inverse, celle qui recolle les lignes...
;; Et même mieux, il fournit également unfill-toggle, qui passe d'un état à
;; l'autre.
(use-package unfill
  :bind
  ("M-q" . unfill-toggle))


;; Génère des libellés plus pertinents pour les buffers de mêmes noms.
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward))


;; Nettoie les espaces, mais pas activé par défaut pour éviter les diffs
;; malheureux. Éventuellement, remplacer par ws-buttler ?
(use-package whitespace
  :config
  (setq whitespace-style '(face trailing lines empty)))


;; Une fonction pour remettre en forme du XML. Prise sur le site de B Ferrari :
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
	(backward-char 2) (insert "\n") (incf end))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
	(backward-char) (insert "\n") (incf end))
    (indent-region begin end nil)
    (normal-mode))
  (message "All indented!"))


(defun my-package-setup()
  (interactive)
  
  (add-to-list 'package-archives
			   '("MELPA" . "http://melpa.org/packages/") t)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package '(ac-capf
					 ac-helm
					 auto-complete-config
					 bm
					 cmake-mode
					 corfu
					 epa-file
					 fuzzy
					 graphviz-dot-mode
					 helm
					 helm-projectile
					 htmlize
					 idle-highlight-mode
					 kind-icon
					 leuven-theme
					 nord-theme
					 magit
					 mmm-mode
					 projectile
					 rainbow-delimiters
					 rainbow-mode
					 related
					 standard-themes
					 unfill
					 uniquify
					 whitespace
					 ws-butler
					 ))
    (message "---> %s" package)
    (unless (package-installed-p package)
	  (ignore-errors
		(package-install package)))))
