(defun my-setup()
  (add-to-list 'package-archives
               '("MELPA" . "http://melpa.milkbox.net/packages/") t)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package '(ac-capf
                     ac-helm
                     auto-complete-config
                     bm
                     cargo
                     cmake-mode
                     column-marker
                     epa-file
                     fill-column-indicator
                     flycheck-rust
                     fuzzy
                     ggtags
                     graphviz-dot-mode
                     helm
                     helm-projectile
                     htmlize
                     idle-highlight-mode
                     leuven
                     magit
                     mmm-mode
                     ob-rust
                     projectile
                     racer
                     rainbow-delimiters
                     rainbow-mode
                     related
                     rust-mode
                     unfill
                     uniquify
                     whitespace))
      (message "---> %s" package)
      (unless (package-installed-p package)
        (ignore-errors
          (package-install package)))))