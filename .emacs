(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq create-lockfiles nil)

;; Appearance
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(setq inhibit-startup-message t)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

(load-theme 'gruvbox t)
(set-face-attribute 'default nil :font "Fira Code" :height 160)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Vi mode
(require 'evil)
(evil-mode 1)

(setq mac-function-modifier 'meta)
(setq mac-option-modifier nil)

;; Searching
(require 'ido)

(ido-mode 1)
(ido-everywhere 1)

(setq default-directory "~/")

;; Global modes
(autopair-global-mode)

;; LSP mode
(require 'lsp-mode)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-completion-provider :none)

(setq lsp-auto-guess-root t)
(setq lsp-diagnostic-package :none)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-enable-folding nil)
(setq lsp-enable-imenu nil)
(setq lsp-enable-snippet nil)
(setq lsp-enable-completion-at-point nil)
(setq lsp-idle-delay 0.5)
(setq lsp-prefer-capf t)

;; Yassnippets
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))

(yas-global-mode 1)

;; Company mode
(require 'company)
(require 'company-c-headers)
(require 'company-capf)
(push 'company-capf company-backends)

(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include")

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(define-key company-active-map (kbd "<return>") nil)
(define-key company-active-map (kbd "<tab>") #'company-complete-selection)

;; C mode
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c-mode-hook #'company-mode)

(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'company-mode)

(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

;; Packages
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
   '(company-arduino evil company-c-headers autopair yasnippet company lsp-mode smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

