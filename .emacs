;;; package --- Summary: René Huiberts | Emacs Configuration.
;;; Commentary:
;;; Code:

;;; === My Emacs Configuration File =========================================
;; Emacs Package Manager
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Backup files
(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)

;;; === Emacs Startup =======================================================
;; Message in scratch buffer
(defun display-startup-echo-area-message ()
  "Disable Startup message."
    (message " "))

;; Initialization
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq inhibit-startup-message t)
(setq use-dialog-box nil)

;;; === Appearance ==========================================================
;; Window Appearance
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(internal-border-width . 5))
(add-to-list 'default-frame-alist '(height . 10))

(setq frame-title-format "\n")
(setq ns-use-proxy-icon nil)
(setq frame-size-history nil)
(setq ring-bell-function 'ignore)

(if (string-equal system-type "darwin")
    (menu-bar-mode 1)
  (menu-bar-mode 0))

(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)

;; Editor Appearance
(set-face-attribute 'default nil :font "Fira Code" :height 180)

;; Theme
(setq custom-safe-themes t)
(load-theme 'custom-gruvbox-dark-soft t)

;; Modeline
(mood-line-mode)



;;; === Editor Configurations ===============================================
;; Display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode +1)
(add-hook 'elisp-mode-hook 'display-line-numbers-mode +1)
(add-hook 'latex-mode-hook 'display-line-numbers-mode +1)
(setq display-line-numbers-type 'relative)

;; Autopair brackets
(autopair-global-mode)

;; === File management / Searching ==========================================
(use-package dired-x
    :config
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
        (rx (or (seq bol (? ".") "#")
            (seq bol "." eol)
			(seq bol ".DS_STORE" eol)
			(seq bol ".projectile" eol)
			(seq bol ".ccls-cache" eol)
			(seq bol ".log/" eol)
		    (seq bol ".localized" eol))))
    (setq dired-listing-switches "-laGh1v --group-directories-first")
    ;; Options https://oremacs.com/2015/01/13/dired-options/
    (setq dired-recursive-copies 'always))

;; Searching using the C- and M- commands
(use-package ido
    :init
    (setq default-directory "~/")
    (global-set-key (kbd "M-x") 'smex)
    :config
	(add-to-list 'ido-ignore-files "\\.DS_Store")
    (ido-everywhere 1)
    (ido-mode 1)
	(setq ido-file-extensions-order '(".emacs")))

;; Bookmarks
(global-set-key (kbd "C-x C-g")
    (lambda ()
        (interactive)
        (bookmark-jump
            (ido-completing-read "Jump to bookmark: "
                (bookmark-all-names)))))

;;; === Project Management ==================================================
(use-package projectile
    :ensure t
    :init
    (projectile-mode +1)
    :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
    :config
    (setq projectile-track-known-projects-automatically nil))

;;; === General Language Configurations =====================================
;; Indentations, Whitespaces and Brackets
(use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1))

;; Language Server Protocol
(use-package lsp-mode
    :init
    :hook ((c-mode . lsp)
           (c++-mode . lsp)
           (c++-mode . platformio-conditionally-enable)
		   (csharp-mode . lsp)
     	   (lua-mode . lsp)
		   (python-mode . lsp)
           (java-mode . lsp)
		   (web-mode . lsp)
		   (json-mode . lsp))
    :config
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-auto-guess-root t)
    (setq lsp-enable-folding nil)
    (setq lsp-idle-delay 0.5))

;; Syntax and Errors
(use-package flycheck
    :ensure t
    :init (global-flycheck-mode)
	:config
	(setq flycheck-check-syntax-automatically
        '(save idle-change mode-enabled))
	(setq flycheck-idle-change-delay 4))

;; Autocompletions
(require 'company-c-headers)
(require 'company-capf)
(use-package company
    :init
    (setq company-backends '((company-capf company-c-headers company-glsl)))
    :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-selection))
    :config
    (add-to-list 'company-backends 'company-c-headers)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)
    (global-company-mode t))

;;; === Language Specific Configurations ====================================
(use-package csharp-mode
  :ensure t)

(use-package glsl-mode
    :ensure t
    :mode (("\\.glsl\\'" . glsl-mode)))

(use-package lsp-java
  :ensure t
  :after lsp
  :hook (add-hook 'java-mode-hook #'lsp))

(use-package lua-mode
  :ensure t)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t))

(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
		 ("\\.ts\\'" . web-mode)
		 ("\\.jsx\\'" . web-mode)
		 ("\\.tsx\\'" . web-mode)
		 ("\\.html\\'" . web-mode))
  :commands web-mode)

(use-package json-mode
    :ensure t)

;;; === Specific Keybindings ================================================
;; MacBook command usages
(setq mac-function-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-pass-command-to-system t)
(setq select-enable-clipboard t)

(when (symbolp 'mac-control-modifier)
    (global-set-key (kbd "s-z") 'undo)
    (global-set-key (kbd "s-x") 'kill-region)
    (global-set-key (kbd "s-c") 'kill-ring-save)
    (global-set-key (kbd "s-v") 'yank))

(windmove-default-keybindings)

;; VIM-like keybindings
(use-package evil
    :ensure t
    :config
    (evil-mode 1))

;;; === Writing & Documentation  ============================================
;; Whitespace's and concentrations mode
(use-package olivetti
    :ensure t
    :init
    (add-hook 'org-mode-hook 'olivetti-mode 1)
    :custom
    (olivetti-body-width 76))

;; Deletions and indentation
(use-package hungry-delete
    :ensure t
    :config
    (setq hungry-delete-join-reluctantly t)
    (global-hungry-delete-mode 1))

;; Organizational files
(use-package org
    :ensure t
    :init
    (setq org-hide-emphasis-markers t)
    (setq org-startup-folded nil)
    (setq org-link-frame-setup '((file . find-file)))
	(setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-preserve-indentation t)
    :config
    (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.0)))

(latex-preview-pane-enable)

;; Display fill column indicator
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'latex-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(set-face-background 'fill-column-indicator "#3c3836")
(setq-default display-fill-column-indicator-column 77)
(setq-default display-fill-column-indicator-character '32)

;; Auto wrap
(visual-line-mode t)

;; Minor writing modes
(delete-selection-mode)

;;; === Compilation & Terminal ==============================================
;; Open compilations window horizontal
(defun my-compilation-hook ()
  "Compile window always at the bottom."
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
            (shrink-window (- h 15)))))))

(add-hook 'compilation-mode-hook 'my-compilation-hook)

;; Other compile commands
(setq compile-command "./build.sh")
(setq compilation-scroll-output t)
(add-hook 'compilation-mode-hook
          (lambda () (visual-line-mode nil)))

;; Execute path from shell using ZSH
(use-package exec-path-from-shell
    :ensure t
    :custom
    (shell-file-name "/bin/zsh")
    :init
    (if (string-equal system-type "darwin")
        (exec-path-from-shell-initialize)))

;; Enable Ansii colors
(use-package ansi-color
    :config
    (defun my-colorize-compilation-buffer ()
        (when (eq major-mode 'compilation-mode)
            (ansi-color-apply-on-region compilation-filter-start
                (point-max))))
    :hook (compilation-filter . my-colorize-compilation-buffer))

;; Mini Buffer
(add-hook 'minibuffer-setup-hook
		  (lambda () (setq truncate-lines t)))

;;; === SSH and Remote Configurations =======================================
;; Connection to Raspberrypi
(defun connect-hotspot-rasp ()
  "Connect to my raspberrypi."
  (interactive)
  (dired "/ssh:rhuib@192.168.43.114:/home/rhuib"))

(defun connect-local-rasp ()
  "Connect to my raspberrypi."
  (interactive)
  (dired "/sshx11:rhuib@169.254.194.102:/home/rhuib"))

(defun connect-babe-rasp ()
  "Connect to my raspberrypi."
  (interactive)
  (dired "/sshx11:rhuib@192.168.2.10:/home/rhuib"))

;; Allow X11 window share for raspberrypi
(with-eval-after-load 'tramp
  (add-to-list 'tramp-methods
      '("sshx11"
           (tramp-login-program        "ssh")
           (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
                                           ("-e" "none") ("-X") ("%h")))
           (tramp-async-args           (("-q")))
           (tramp-remote-shell         "/bin/sh")
           (tramp-remote-shell-login   ("-l"))
           (tramp-remote-shell-args    ("-c"))
           (tramp-gw-args
               (("-o" "GlobalKnownHostsFile=/dev/null")
                   ("-o" "UserKnownHostsFile=/dev/null")
                   ("-o" "StrictHostKeyChecking=no")
                   ("-o" "ForwardX11=yes")))
           (tramp-default-port         22)))
    (tramp-set-completion-function "sshx11"
        tramp-completion-function-alist-ssh))

;; === Custom Set Variables =================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(package-selected-packages
      '(editorconfig buffer-move omnisharp csharp-mode lsp-python-ms
           company-glsl glsl-mode hungry-delete web-mode json-mode
           lsp-javacomp tide typescript-mode lsp-mode latex-preview-pane
           mood-line centered-window olivetti writeroom-mode ccls
           platformio-mode magit lua-mode use-package flycheck
           exec-path-from-shell evil company-c-headers autopair yasnippet
           company smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; .emacs ends here
