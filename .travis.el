(require 'package)

(add-to-list 'package-archives
             '("MELPA" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("Org" . "https://orgmode.org/elpa/") t)

(package-initialize)


(defun my-setup()
  (add-to-list 'package-archives
               '("MELPA" . "http://melpa.milkbox.net/packages/") t)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package '(
                     htmlize
					 mmm-mode
					 rainbow-delimiters
                     rainbow-mode
					 ))
      (message "---> %s" package)
      (unless (package-installed-p package)
        (ignore-errors
          (package-install package)))))
