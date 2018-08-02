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

(require 'cl)

(last-step-duration "Généralités")

;; À la place de view-order-manuals qui est normalement sur f1
(global-set-key (kbd "<f1>")	'view-mode)
(global-set-key (kbd "<C-f1>")	'revert-buffer)
(global-set-key (kbd "<S-f1>")	'revert-buffer)

(global-set-key (kbd "<f2>")	'bm-toggle)
(global-set-key (kbd "<C-f2>")	'bm-next)
(global-set-key (kbd "<S-f2>")	'bm-previous)

(unless window-system
  (global-set-key (kbd "<S-f2>")	'helm-bm))

(global-set-key (kbd "<f3>")	'hl-line-mode)

(global-set-key (kbd "<f4>")	'projectile-grep)

(global-set-key (kbd "<f5>")	'run-compilation)
(global-set-key (kbd "<C-f5>")	'end-compilation)
(global-set-key (kbd "<S-f5>")	'end-compilation)

(global-set-key (kbd "<f6>")	'projectile-multi-occur)
(global-set-key (kbd "<C-f6>")	'helm-occur)
(global-set-key (kbd "<S-f6>")	'helm-occur)

(global-set-key (kbd "<f8>")	'fci-mode)
(global-set-key (kbd "<C-f8>")	'whitespace-mode)
(global-set-key (kbd "<S-f8>")	'whitespace-mode)

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

(defun get-compilation-buffer ()
  "Get the compilation buffer, or nil if it does not exist."
  ;; Voir aussi `compilation-buffer-name-function'
  (car (cl-remove-if-not
		(lambda (b)
		  (equal "*compilation*" (buffer-name b)))
		(buffer-list))))

(defun get-compilation-layout-register ()
  "Returns the register used to save the layout before compilation,
and restore it later."
  (message "get-compilation-layout-register ()")
  ;; It seems we can use more than one letter register !
  'comp-layout-reg)

(defun start-new-compilation ()
  "Prompt for command and run a new compilation"
  ;; Passer en plein écran
  ;; (let ((current-prefix-arg '(4)))	; C-u
  ;; (call-interactively 'compile)))
  (call-interactively 'compile))

(defun return-to-compilation ()
  "Get compilation buffer back in full screen"
  ;; Relancer la compil automatiquement ?
  (switch-to-buffer (get-compilation-buffer))
  (delete-other-windows))

(setq ongoing-compilation-session nil)

(defun start-compilation-session ()
  "Save layout and start a new compilation session"
  (frame-configuration-to-register (get-compilation-layout-register))
  (start-new-compilation)
  (setq ongoing-compilation-session t))

(defun end-compilation-session ()
  "Restore pre-compilation layout and terminate compilation session"
  ;; Supprimer le buffer de compil ?
  (setq ongoing-compilation-session nil)
  (jump-to-register (get-compilation-layout-register)))

(defun run-compilation ()
  "Start new compilation session or restore an old one"
  (interactive)
  (if (and ongoing-compilation-session (get-compilation-buffer))
	  (return-to-compilation)
	(start-compilation-session)))

(defun end-compilation ()
  "Terminate a compilation session"
  (interactive)
  (if ongoing-compilation-session
	  (end-compilation-session)))

(setq compilation-scroll-output 'first-error)

(last-step-duration "Compilation")

(setq mouse-yank-at-point t)

(setq save-interprogram-paste-before-kill t)

(setq mouse-drag-copy-region t)

(last-step-duration "Copier/coller")

(show-paren-mode 1)

(set-face-background 'show-paren-match "#b5d5ff")

(last-step-duration "Parenthèses et Cie")

;; TODO - Filtrer les doublons
(defun add-to-exec-paths(some-folder)
  (interactive)
  (cond
   ((file-exists-p some-folder)
	(add-to-list 'exec-path some-folder)
	;;(message "exec-path=%s" exec-path)
	(setenv "PATH" (concat (getenv "PATH") ":" some-folder))
	;;(message "PATH=%s" (getenv "PATH"))
	(message "Added '%s' to PATH and exec-path" some-folder))))

(add-to-exec-paths "~/scripts")
(add-to-exec-paths "~/local/bin")
(add-to-exec-paths "/usr/bin")
(add-to-exec-paths "/usr/local/bin")
(add-to-exec-paths "/opt/local/bin")

(last-step-duration "Maj du path")

(setq my-proxy ())

(if (file-exists-p "~/.emacs.d/proxy.el")
	(progn
	  (load-file "~/.emacs.d/proxy.el")))

(unless my-proxy (setq my-proxy (getenv "http_proxy")))

(if my-proxy
	(progn
	  (setq url-proxy-services (list (cons "http" (symbol-name my-proxy))))
	  (message "Set HTTP proxy to '%s'" my-proxy)))

(last-step-duration "Valorisation du proxy")

(cond

((string-match "linux" system-configuration)
 (progn
   (message "Tweak Emacs for Linux")

(set-default-font "Liberation Mono 10")

))

((string-match "apple" system-configuration)
 (progn
   (message "Tweak Emacs for Mac")

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(set-face-font
 'menu "-apple-menlo-medium-r-normal--11-110-72-72-m-110-iso10646-1")
(set-face-font
 'default "-apple-menlo-medium-r-normal--11-110-72-72-m-110-iso10646-1")

)))

(last-step-duration "Ajustements à l'OS")

(defun my-require(feature)
  (condition-case nil
	  (let ((ts (current-time)))
		(progn
		  (require feature)
		  (let ((elapsed (float-time (time-subtract (current-time) ts))))
			(message "Successfully load '%s' in %.3fs" feature elapsed))))
	(file-error
	 (progn (message "Fail to load required feature '%s'" feature) nil))))

(last-step-duration "Require")

(when

(my-require 'package)

(add-to-list 'package-archives
             '("MELPA" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

)

(last-step-duration "Gestionnaire de paquets")

(if (member 'leuven-theme (mapcar 'car package-alist))
    (load-theme 'leuven t))

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
			(define-key map (kbd "<S-tab>") 'prev-tab-to-tab-stop)
			map))

(add-hook 'cmake-mode-hook 'tab-tab-mode)

(last-step-duration "TabTab minor mode")

(when

(my-require 'auto-complete-config)

(ac-config-default)

(add-to-list 'ac-modes 'cmake-mode)
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'text-mode)

(global-auto-complete-mode t)

(setq ac-auto-start nil)
(global-set-key "\M-/" 'auto-complete))

(last-step-duration "Auto Complete")

(when

(my-require 'bm)

(setq bm-cycle-all-buffers t)

(custom-set-faces
 '(bm-face ((t (:background "#ffafff")))))

)

(last-step-duration "Bookmarks")

(c-set-offset (quote substatement-open) 0)

(setq auto-mode-alist (append '(("\.h$" . c++-mode)) auto-mode-alist))

(last-step-duration "C & C++")

(last-step-duration "Column Marker")

(when

(my-require 'cmake-mode)

(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode))
              '(("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

(add-hook 'cmake-mode-hook (lambda () (electric-indent-mode -1)))

)

(last-step-duration "CMake")

(when

(my-require 'dtrace-script-mode)

(setq auto-mode-alist
      (append '(("\\.d\\'" . dtrace-script-mode))
              auto-mode-alist))

)

(last-step-duration "DTrace")

(when

(my-require 'epa-file)

(epa-file-enable)

(setq epa-file-encrypt-to "julien.montmartin@fastmail.fm")

)

(last-step-duration "EasyPG Assistant")

(setq ediff-split-window-function 'split-window-horizontally)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(last-step-duration "Ediff")

(ffap-bindings)

(setq ff-search-directories
      '("." ".."
        "./src" "./include"
        "../src" "../include"
        "../src/*"  "../include/*"
        "../../src" "../../include"))

(last-step-duration "Find File At Point")

(my-require 'fill-column-indicator)

(setq-default fci-rule-column 80)

(autoload 'gtags-mode "gtags" "" t)

(global-set-key "\M-." 'gtags-find-tag)
(global-set-key "\M-*" 'gtags-pop-stack)

(last-step-duration "Global")

(when

(my-require 'graphviz-dot-mode)

(define-key graphviz-dot-mode-map (kbd "C-c v") 'graphviz-dot-preview)

)

(last-step-duration "Graphviz")

(when

(my-require 'helm)

(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(define-key helm-map (kbd "C-z")  'helm-select-action)

(setq helm-ff-auto-update-initial-value t)

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(setq helm-split-window-in-side-p t)

(setq helm-move-to-line-cycle-in-source t)

(custom-set-faces
 '(helm-ff-dotted-directory ((t (:foreground "DimGrey")))))

(helm-mode t)

)

(last-step-duration "Helm")

(my-require 'htmlize)

(last-step-duration "Htmlize")

(when

(my-require 'idle-highlight-mode)

(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

)

(last-step-duration "Idle HighLight Mode")

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(last-step-duration "Lisp")

(if (executable-find "git")
    (progn (when

(my-require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

)))

(last-step-duration "Magit")

(when

(my-require 'mmm-auto)

(setq mmm-parse-when-idle t)

(setq mmm-global-mode 'sometimes)

(mmm-add-classes
 '((here-doc
    :submode shell-script-mode
    :front "<<EOF"
    :back "^EOF")))

(mmm-add-mode-ext-class 'text-mode nil 'here-doc)

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

(when

(my-require 'p4)

(setenv "P4CONFIG" ".P4CONFIG")

)

(last-step-duration "Perforce")

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

(when

(my-require 'projectile)

(projectile-global-mode)

(global-set-key (kbd "C-t") 'projectile-find-other-file)

(setq projectile-completion-system 'helm)

(helm-projectile-on)

)

(last-step-duration "Projectile")

(defun generate-qt-includes ()
    "Insert a list of Qt includes matching Qt types found in this buffer"
	(interactive)
	(shell-command-on-region
	 (point-min) (point-max)
	 ;; Pourquoi ne peut-on pas mettre le pipe en début de ligne ?
	 "sed 's/\#.*include.*<.*>/#include <header>/' |
sed 's://.*:// comment:' |
sed -n 's/.*\\(Q[A-Z][a-zA-Z]*\\).*/#include <\\1>/p' |
sort | uniq" )
  (insert-buffer "*Shell Command Output*"))

(global-set-key (kbd "M-#") 'generate-qt-includes)

(last-step-duration "Qt")

(when

(my-require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

)

(last-step-duration "Rainbow delimiters")

(when

(my-require 'rainbow-mode)

(add-hook 'prog-mode-hook 'rainbow-mode)

)

(last-step-duration "Rainbow mode")

(when

(my-require 'related)

(related-mode)

(global-set-key (kbd "C-x <end>") 'related-switch-buffer )

)

(last-step-duration "Related")

(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))

(last-step-duration "Shell scripts")

(setq mouse-wheel-scroll-amount '(2))

(setq mouse-wheel-progressive-speed nil)

(last-step-duration "Souris")

(setq auto-mode-alist (append '(("\.tm$" . tcl-mode)) auto-mode-alist))

(setq tramp-default-method "ssh")

(last-step-duration "Tramp")

(when

(my-require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward)

)

(last-step-duration "Uniquify")

(when

(my-require 'whitespace)

(setq whitespace-style '(face trailing lines empty))

(add-hook 'before-save-hook 'whitespace-cleanup)

)

(last-step-duration "Whitespace")

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

(defun my-setup()
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package '(auto-complete-config
                     bm
                     column-marker
                     cmake-mode
                     epa-file
                     fill-column-indicator
                     graphviz-dot-mode
                     helm
                     htmlize
                     idle-highlight-mode
                     leuven-theme
                     magit
                     mmm-auto
                     projectile
                     helm-projectile
                     rainbow-delimiters
                     rainbow-mode
                     related
                     uniquify
                     whitespace))
    (message "---> %s" package)
    (unless (package-installed-p package)
      (ignore-errors
        (package-install package)))))
