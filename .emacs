;;; package --- Summary
;;; Commentary:
;;; Code:
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq initial-buffer-choice "~/startup.org")

(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)

(defun display-startup-echo-area-message ()
  "Disable Startup message."
  (message " "))

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)

;; === Appearance ====================================================================
(setq custom-safe-themes t)
(load-theme 'custom-gruvbox-dark-soft t)
(set-face-attribute 'default nil :font "Fira Code" :height 160)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(internal-border-width . 5))
(add-to-list 'default-frame-alist '(height . 10))
(when window-system
    (set-frame-position (selected-frame) 0 0)
	(set-frame-size (selected-frame) 168 48))

(setq-default frame-title-format '("%b"))

(setq ns-use-proxy-icon nil)
(setq frame-size-history nil)

(global-display-line-numbers-mode)

;; === Windows & Buffers =============================================
(windmove-default-keybindings)

;; === Mini Buffer
(add-hook 'minibuffer-setup-hook
		  (lambda () (setq truncate-lines t)))

;; === Terminal ======================================================
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq mac-function-modifier 'meta)

;; === File management / Searching ===================================
(use-package dired-x
  :config
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (setq dired-omit-files
      (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^.ccls-cache$\\|^.localized$"))
  (setq dired-listing-switches "-laGh1v --group-directories-first")
  ;; Options https://oremacs.com/2015/01/13/dired-options/
  (setq dired-recursive-copies 'always))

(use-package ido
    :init
    (setq default-directory "~/")
    (global-set-key (kbd "M-x") 'smex)
    :config
	(add-to-list 'ido-ignore-files "\\.DS_Store")
    (ido-everywhere 1)
    (ido-mode 1)
	(setq ido-file-extensions-order '(".emacs")))

;; === Auto Completing ===============================================
(use-package yasnippet
    :ensure t
    :init
    (setq yas-snippet-dirs '("~/.emacs.snippets/"))
    :config
    (yas-global-mode 1))

(require 'company-c-headers)
(require 'company-capf)
(use-package company
    :init
    (setq company-backends '((company-capf company-c-headers)))
    :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-selection))
    :config
    (add-to-list 'company-backends 'company-c-headers)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)
    (global-company-mode t))

(defun mars/company-backend-with-yas (backends)
      "Add :with company-yasnippet to company BACKENDS."
      (if (and (listp backends) (memq 'company-yasnippet backends))
		  backends
		(append (if (consp backends)
		  backends
		  (list backends))
		'(:with company-yasnippet))))

;; add yasnippet to all backends
(setq company-backends
	  (mapcar #'mars/company-backend-with-yas company-backends))

(autopair-global-mode)

;; === Syntax Checking ================================================
(require 'platformio-mode)
(use-package lsp-mode
  :init
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (c++-mode . platformio-conditionally-enable)
		 (java-mode . lsp)
		 (lua-mode . lsp))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-folding nil)
  (setq lsp-idle-delay 0.5))

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode)
	:config
	(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
	(setq flycheck-idle-change-delay 4))

;; === C / C++ / Obj-C Languages ======================================
(setq-default c-basic-offset 4)

;; === Java Language ==================================================
(use-package lsp-java
  :ensure t
  :after lsp
  :hook (add-hook 'java-mode-hook #'lsp))

;; === Lua Language ===================================================
(use-package lua-mode
    :ensure t
    :init)

;; === Org Mode =======================================================
(use-package org
  :ensure t
  :init
  (setq org-startup-folded nil)
  (setq org-link-frame-setup '((file . find-file))))


;; === Yasnippets & Company fix =======================================
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
    (backward-char 1)
    (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas/minor-mode)
        (null (do-yas-expand)))
    (if (check-expansion)
        (progn
          (company-manual-begin)
          (if (null company-candidates)
          (progn
            (company-abort)
            (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas/minor-mode)
      (null (do-yas-expand)))
      (if company-candidates
      (company-complete-selection)
    (if (check-expansion)
      (progn
        (company-manual-begin)
        (if (null company-candidates)
        (progn
          (company-abort)
          (yas-next-field))))
      (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas/minor-mode)
      (null (do-yas-expand))
      (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

(global-set-key [tab] 'tab-indent-or-complete)
(global-set-key (kbd "TAB") 'tab-indent-or-complete)
(global-set-key [(control return)] 'company-complete-common)

(define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
(define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(define-key yas-keymap [tab] 'tab-complete-or-next-field)
(define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
(define-key yas-keymap [(control tab)] 'yas-next-field)
(define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)

;; === Compilation ====================================================
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

;; === Packages =======================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(display-line-numbers-type 'relative)
 '(org-agenda-files '("~/docs/finance.org" "~/startup.org"))
 '(package-selected-packages
   '(lsp-javacomp ccls platformio-mode magit lua-mode use-package flycheck exec-path-from-shell evil company-c-headers autopair yasnippet company smex))
 '(safe-local-variable-values
   '((eval setq flycheck-clang-include-path
		   (list
			(expand-file-name "/usr/local/Cellar/glfw/3.3.2/include")))))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; .emacs ends here
