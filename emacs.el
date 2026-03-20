(defun reset-ts()
  (setq first-ts (current-time))
  (setq last-ts first-ts))

(defun step-duration()
  (let* ((ts (current-time))
	   (elapsed (float-time (time-subtract ts last-ts))))
    (setq last-ts ts)
    elapsed))

(defun total-duration()
  (let* ((ts (current-time))
	   (elapsed (float-time (time-subtract ts first-ts))))
    elapsed))

(defun last-step-duration(title)
  (message "Τ=%.3fs\tΔ=%.3fs\t%s" (total-duration) (step-duration) title))

(reset-ts)

(setq async-bytecomp-allowed-packages '(all))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

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

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(add-hook 'dired-mode-hook 'hl-line-mode)

(when (eq system-type 'darwin)
  (if (file-executable-p "/usr/local/bin/gls")
	(setq insert-directory-program "/usr/local/bin/gls")))

(require 'cl-lib)

(last-step-duration "Généralités")

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

(last-step-duration "Raccourcis Fn")

(set-language-environment 'utf-8)

(last-step-duration "UTF-8")

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

(last-step-duration "Navigation dans les buffers")

(setq mouse-yank-at-point t)

(setq save-interprogram-paste-before-kill t)

(setq mouse-drag-copy-region t)

(last-step-duration "Copier/coller")

(show-paren-mode 1)

(set-face-background 'show-paren-match "#b5d5ff")

(last-step-duration "Parenthèses et Cie")

(defun add-to-exec-paths(some-folder)
  (interactive)
  (cond
   ((file-exists-p some-folder)
    (add-to-list 'exec-path some-folder)
    (setenv "PATH" (concat (getenv "PATH") ":" some-folder))
    ;;(message "Added '%s' to PATH and exec-path" some-folder)
    )))

(add-to-exec-paths "~/bin")
(add-to-exec-paths "~/scripts")

(last-step-duration "Maj du path")

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

(set-mac-keys)

)))

(last-step-duration "Ajustements à l'OS")

(use-package package

:config (add-to-list 'package-archives
		       '("MELPA" . "http://melpa.org/packages/") t)

:config (add-to-list 'package-archives
		       '("Org" . "https://orgmode.org/elpa/") t)

:config (package-initialize))

(last-step-duration "Gestionnaire de paquets")

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

(last-step-duration "Tree Sitter")

(use-package bm

:config (setq bm-cycle-all-buffers t)

:config (custom-set-faces
	   '(bm-face ((t (:background "#ffafff")))))

)

(last-step-duration "Bookmarks")

(c-set-offset (quote substatement-open) 0)

(setq auto-mode-alist (append '(("\.h$" . c++-mode)) auto-mode-alist))

(last-step-duration "C & C++")

(use-package cmake-mode

:config (setq auto-mode-alist
		(append '(("CMakeLists\\.txt\\'" . cmake-mode))
			'(("\\.cmake\\'" . cmake-mode))
			auto-mode-alist))

:config (add-hook 'cmake-mode-hook (lambda () (electric-indent-mode -1)))

)

(last-step-duration "CMake")

(use-package corfu
  ;; :init
  ;; (global-corfu-mode)
  :config
  ;; Complétion automatique après 0.2s
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 2)

  ;; Cycle dans les candidats
  (setq corfu-cycle t)

  ;; Preview du candidat sélectionné
  (setq corfu-preview-current t)

  ;; Navigation avec Tab
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("S-TAB" . corfu-previous)
              ("RET" . corfu-insert))

)

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(last-step-duration "Corfu")

(setq epa-file-encrypt-to "julien.montmartin@fastmail.fm")

(last-step-duration "EasyPG Assistant")

(setq ediff-split-window-function 'split-window-horizontally)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(last-step-duration "Ediff")

(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :config
  ;; Désactive les messages verbeux
  (setq eglot-events-buffer-size 0)
  ;; Formate au save (optionnel)
  (add-hook 'before-save-hook
            (lambda ()
              (when (eglot-managed-p)
                (eglot-format-buffer))))
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc-doc-buffer))

)

(last-step-duration "Eglot")

(use-package graphviz-dot-mode

:config (define-key graphviz-dot-mode-map (kbd "C-c v") 'graphviz-dot-preview)

)

(last-step-duration "Graphviz")

(use-package helm

:init (global-set-key (kbd "C-c h") 'helm-command-prefix)
:init (global-unset-key (kbd "C-x c"))

:bind ("M-x" . helm-M-x)
:bind ("C-x C-f" . helm-find-files)
:bind ("C-x r b" . helm-filtered-bookmarks)

:bind ("C-x b" . helm-mini)

:config (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

:config (define-key helm-map (kbd "C-z") 'helm-select-action)

:config (setq helm-ff-auto-update-initial-value t)

:init (when (executable-find "curl")
	  (setq helm-net-prefer-curl t))

:config (setq helm-split-window-in-side-p t)

:config (setq helm-move-to-line-cycle-in-source t)

:config (custom-set-faces
	   '(helm-ff-dotted-directory ((t (:foreground "DimGrey")))))

:config (setq completion-styles '(flex))

:config (helm-mode 1))

(last-step-duration "Helm")

(use-package htmlize)

(last-step-duration "Htmlize")

(use-package idle-highlight-mode

:hook (prog-mode . idle-highlight-mode)

)

(last-step-duration "Idle HighLight Mode")

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(last-step-duration "Lisp")

(if (executable-find "git")
    (progn (when

(use-package magit

:bind ("C-x g" . magit-status)

))))

(last-step-duration "Magit")

(use-package mmm-mode

:config (setq mmm-parse-when-idle t)

:config (setq mmm-global-mode 'sometimes)

:config (mmm-add-classes
	   '((here-doc
	      :submode shell-script-mode
	      :front "<<EOF"
	      :back "^EOF")))

:config (mmm-add-mode-ext-class 'text-mode nil 'here-doc)

)

(last-step-duration "MMM Mode")

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

(last-step-duration "Org Mode")

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

(last-step-duration "Prettify Symbols")

(use-package projectile

:config (projectile-mode +1)

:bind ("C-t" . projectile-find-other-file)

:config (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :config (helm-projectile-on))

(last-step-duration "Projectile")

(use-package rainbow-delimiters

:hook (prog-mode . rainbow-delimiters-mode)

)

(last-step-duration "Rainbow delimiters")

(use-package rainbow-mode

:hook (prog-mode . rainbow-mode)

)

(last-step-duration "Rainbow mode")

(use-package related

:config (related-mode)

:bind ("C-x <end>" . related-switch-buffer)

)

(last-step-duration "Related")

(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))

(last-step-duration "Shell scripts")

(setq mouse-wheel-scroll-amount '(2))

(setq mouse-wheel-progressive-speed nil)

(last-step-duration "Souris")

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

(last-step-duration "TabTab minor mode")

(setq auto-mode-alist (append '(("\.tm$" . tcl-mode)) auto-mode-alist))

(use-package standard-themes
  :config (load-theme 'standard-light t))

(last-step-duration "Standard Themes")

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

(last-step-duration "Tramp")

(use-package unfill

:bind ("M-q" . unfill-toggle)

)

(last-step-duration "Unfill")

(use-package uniquify

:config (setq uniquify-buffer-name-style 'post-forward)

)

(last-step-duration "Uniquify")

(use-package whitespace

:config (setq whitespace-style '(face trailing lines empty))

)

(last-step-duration "Whitespace")

(last-step-duration "Ws-butler")

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

(last-step-duration "XML")

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
