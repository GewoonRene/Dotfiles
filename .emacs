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
  (message " "))

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)

;; === Appearance ====================================================
(setq custom-safe-themes t)
(load-theme 'custom-gruvbox-dark-soft t)
(set-face-attribute 'default nil :font "Fira Code" :height 160)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; === Terminal ======================================================
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; === Dired =========================================================
(use-package dired-x
  :config
  (progn
(setq dired-omit-verbose nil)
(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq dired-omit-files
      (concat dired-omit-files "\\|^.DS_STORE$"))))

(defun mydired-sort ()
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  (mydired-sort))

;; === Editor ========================================================
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; === Vi mode =======================================================
(use-package evil
    :config
    (setq mac-function-modifier 'meta)
    (setq mac-option-modifier nil)
    (evil-mode 1))

;; === Searching =====================================================
(use-package ido
    :init
    (setq default-directory "~/")
    (global-set-key (kbd "M-x") 'smex)
    :config
    (ido-everywhere 1)
    (ido-mode 1))

;; === Auto Completing ================================================
(require 'company-c-headers)
(require 'company-capf)

(use-package company
    :init
    (setq company-backends '((company-capf company-c-headers)))
    :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
        ("<tab>" . company-complete-selection)
    :config
    (add-to-list 'company-backends 'company-c-headers)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)
    (global-company-mode t))

(use-package yasnippet
    :init
    (setq yas-snippet-dirs '("~/.emacs.snippets/"))
    :config
    (yas-global-mode 1))

(autopair-global-mode)

;; === Syntax Checking ================================================
(use-package lsp-mode
  :init
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (c++-mode . (platformio-conditionally-enable))
		 (lua-mode . lsp)
		 (java-mode . lsp)
		 (arduino-mode . lsp))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-folding nil)
  (setq lsp-idle-delay 0.5)
)

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

;; === Lua Language ===================================================
(use-package lua-mode
    :ensure t
    :init)

;; === Packages =======================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(org-agenda-files '("~/startup.org"))
 '(package-selected-packages
   '(magit lua-mode use-package flycheck exec-path-from-shell evil company-c-headers autopair yasnippet company lsp-mode smex))
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

