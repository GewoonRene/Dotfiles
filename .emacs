(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq create-lockfiles nil)
(setq auto-save-default nil)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(setq inhibit-startup-message t)

;; === Appearance ===================================================
(load-theme 'gruvbox t)
(set-face-attribute 'default nil :font "Fira Code" :height 160)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

;; === Terminal =====================================================
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; === Editor =======================================================
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
(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

;; === C / C++ / Obj-C ================================================
(use-package lsp-mode
  :init
  :hook ((c-mode . lsp)
         (c++-mode . lsp))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-folding nil)
  (setq lsp-idle-delay 0.5)
)

;; === Packages =======================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(custom-safe-themes
   '("d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" default))
 '(package-selected-packages
   '(arduino-cli-mode company-irony-c-headers company-arduino arduino-mode use-package flycheck exec-path-from-shell company-irony irony-eldoc evil company-c-headers autopair yasnippet company lsp-mode smex))
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

