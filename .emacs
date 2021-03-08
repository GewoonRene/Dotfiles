
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq indent-line-function 'insert-tab)
(setq-default c-basic-offset 4
	      c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

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
(require 'company-irony)
(require 'company-capf)

(use-package company
    :init
    (setq company-backends '((company-irony company-c-headers company-capf)))
    :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
        ("<tab>" . company-complete-selection)
    :config
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)
    (add-to-list 'company-backends 'company-c-headers)
    (add-to-list #'company-c-headers-path-system "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include")
    (add-to-list #'company-c-headers-path-system "/usr/local/include")
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
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq create-lockfiles nil)
(setq auto-save-default nil)

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
   '(use-package flycheck exec-path-from-shell company-irony irony-eldoc evil company-c-headers autopair yasnippet company lsp-mode smex))
 '(safe-local-variable-values
   '((eval setq flycheck-clang-include-path
           (list
            (expand-file-name "/usr/local/Cellar/glfw/3.3.2/include")))
     (flycheck-clang-include-path . "/usr/local/include/")
     (flycheck-clang-include-path . "/usr/local/include")
     (flycheck-clang-include-path . /usr/local/include))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

;;; .emacs ends here
