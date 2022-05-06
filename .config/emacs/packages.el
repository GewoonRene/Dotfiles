;;; packages.el --- Emacs packages configurations           -*- lexical-binding: t; -*-

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(defvar rhuib/package-contents-refreshed nil)

(defun rhuib/package-refresh-contents-once ()
    (when (not rhuib/package-contents-refreshed)
        (setq rhuib/package-contents-refreshed t)
        (package-refresh-contents)))

(defun rhuib/require-one-package (package)
    (when (not (package-installed-p package))
        (rhuib/package-refresh-contents-once)
        (package-install package)))

(defun rhuib/require (&rest packages)
    (dolist (package packages)
        (rhuib/require-one-package package)))

(defun rhuib/require-theme (theme)
    (rhuib/require theme)
    (load-theme theme t))

;; packages.el ends here.
