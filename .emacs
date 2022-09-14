;;; .emacs --- Rhuibertsjr Emacs configuration            -*- lexical-binding: t; -*-

;; Initialize 
(package-initialize)

;; === Packages Management ==========================================================
(load "~/.config/emacs/packages.el")

(rhuib/require
    ; Packages
    'use-package
    ; Language Server Protocol
    'lsp-mode
    'ccls
    ; Search
    'smex
    'ido-completing-read+
    'ido-yes-or-no
    ; Lisps
    'sly
    'racket-mode
    'highlight-indent-guides
    ; Spelling & Grammer
    'company
    'company-c-headers
    'company-php
    'flycheck
    'yasnippet
    'clang-format
    ; Keyboard
    'evil
    'evil-collection
    ; Languages
    'platformio-mode
    'typescript-mode
    'web-mode
    'json-mode
    ; Writing
    'org-roam
    'latex-preview-pane
    ; Appearance
    'tree-sitter
    'tree-sitter-langs
    ; Miscellaneous
    'exec-path-from-shell
    'magit
    'autopair
    'hungry-delete
    'editorconfig
    'buffer-move
    'hl-todo)

;; === Emacs Startup ===============================================================
(defun display-startup-echo-area-message ()
  "Disable Startup message."
    (message " "))

(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq inhibit-startup-message t)
(setq use-dialog-box nil)
(setq truncate-lines t)

;; Backup files
(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)

(save-place-mode 1)
(set-default 'truncate-lines t)

;;; === Appearance =================================================================
(when (memq window-system '(mac ns))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(add-to-list 'default-frame-alist '(internal-border-width . 5))
(add-to-list 'default-frame-alist '(height . 10))

(set-frame-parameter (selected-frame) 'alpha '(100 . 100))

(setq frame-size-history nil)
(setq ring-bell-function 'ignore)
(setq ns-use-proxy-icon nil)

(if (string-equal system-type "darwin")
    (progn
        (menu-bar-mode 1)
        (setq frame-title-format "\n"))
    (progn
        (menu-bar-mode 0)
        (setq frame-title-format "")))

(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)

;; Editor Appearance
(set-face-attribute 'default nil :font "Fira Code" :height 175)
(setq-default line-spacing 0.0)

(setq custom-safe-themes t)
(setq custom-theme-load-path
    '("~/.config/emacs/themes/"
      "/Users/renehuiberts/Documentations/projects/project-gruver-boxer"
      "/Users/renehuiberts/.emacs.d/elpa/base16-theme-20220526.1015/"
      "/Users/renehuiberts/.emacs.d/elpa/gruvbox-theme-20220101.1208/"))
(load-theme 'gruverboxer t)

;; Modeline
(setq-default
 mode-line-format
 '(
   ;; point position
   (8
    (:propertize " %l:" face font-lock-defaults)
    (:eval (propertize "%c" 'face (if (>= (current-column) 80)
                                      'font-lock-warning-face
                                    'font-lock-defaults))))

   ;; major modes
   (:propertize "%m: " face font-lock-defaults
                help-echo buffer-file-coding-system)

   ;; shortened directory (if buffer have a corresponding file)
   (:eval
    (when (buffer-file-name)
      (propertize (shorten-directory default-directory 25)
                  'face 'font-lock-comment-face)))

   ;; buffer name
   (:propertize "%b" face font-lock-defaults)))

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "./" output)))
    output))

;; Highlight ToDo 
(use-package hl-todo
    :ensure t
    :config
    (setq hl-todo-keyword-faces
	    '(("TODO" . "#b8bb26")
	      ("BUG"  . "#fb4933")))
    (global-hl-todo-mode +1))

(use-package autothemer
    :ensure t)

;;; === Language grammer and semantics ==============================================

(use-package tree-sitter-langs
    :ensure t)

(use-package tree-sitter
    :ensure t
    :after tree-sitter-langs
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(load "~/.config/emacs/custom.el")

;;; === Editor Configurations ======================================================
;; Display line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)

;; Autopair brackets
(autopair-global-mode)

;; Show parenthesis
(add-hook 'lisp-mode-hook 'show-paren-mode t)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match (face-background 'default))

;; Scrolling
(setq scroll-margin 3 
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
    auto-window-vscroll nil)

;; === File management / Searching =================================================
(use-package dired-x
    :config
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
        (rx (or (seq bol (? ".") "#")
            (seq bol "." eol)
			(seq bol ".DS_STORE" eol)
			(seq bol ".projectile" eol)
			(seq bol ".ccls-cache" eol)
			(seq bol ".ccls" eol)
			(seq bol "compile_commands.json" eol)
			(seq bol ".cache" eol)
			(seq bol ".git" eol)
			(seq bol ".pio" eol)
			(seq bol "*.log" eol)
		    (seq bol ".localized" eol))))
    (setq dired-dwim-target t)
    (setq insert-directory-program "gls" dired-use-ls-dired t)
    (setq dired-listing-switches "-laGh1v --group-directories-first")
    ;; Options https://oremacs.com/2015/01/13/dired-options/
    (setq dired-recursive-copies 'always))

;; Searching using the C- and M- commands
(use-package ido
    :init
    (setq default-directory "~/")
    (global-set-key (kbd "M-x") 'smex)
    :config
    (setq ido-auto-merge-work-directories-length -1)
	(add-to-list 'ido-ignore-files "\\.DS_Store")
    (ido-yes-or-no-mode 1)
    (ido-ubiquitous-mode 1)
    (ido-everywhere 1)
    (ido-mode 1))

;; Bookmarks
(global-set-key (kbd "C-x C-g")
    (lambda ()
        (interactive)
        (bookmark-jump
            (ido-completing-read "Jump to bookmark: "
                (bookmark-all-names)))))

;;; === Project Management =========================================================
(use-package projectile
    :ensure t
    :init
    (projectile-mode +1)
    :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
    :config
    (setq projectile-track-known-projects-automatically nil))

;;; === General Language Configurations ============================================
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
           (csharp-mode . lsp)
     	   (lua-mode . lsp)
		   (python-mode . lsp)
           (java-mode . lsp)
		   (web-mode . lsp)
		   (json-mode . lsp)
           (latex-mode . lsp))
    :config
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-auto-guess-root t)
    (setq lsp-enable-folding nil)
    (setq lsp-idle-delay 0.5))

(use-package lsp-ui
    :ensure t
    :after lsp-mode
    :requires lsp-mode flycheck
    :custom
    (lsp-ui-sideline-show-diagnostics t))

;; C syntax
(setq c-default-style '((c++-mode . "linux")
                        (c++-mode . "stroustrup")))
(c-set-offset 'substatement-open 0)

(use-package yasnippet
    :ensure t
    :init
    (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
    :config
    (yas-global-mode 1))

;; Auto-completion
(use-package company-php
    :defer
    :after company)

(require 'company-c-headers)
(use-package company
    :ensure t
    :init
    (setq company-backends '((company-capf company-c-headers company-glsl company-ac-php-backend)))
    :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-selection))
    :config
   (add-to-list 'company-backends 'company-c-headers)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)
    (setq company-c-headers-path-system '(
                   "/Library/Developer/CommandLineTools/usr/include/c++/v1"))
    (global-company-mode t))



;;; === Language Specific Configurations ============================================
(use-package racket-mode
    :ensure t
    :hook (racket-mode . racket-xp-mode))

(use-package php-mode
    :ensure t
    :mode (("\\.php\\'" . php-mode)))

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

;;; === Specific Keybindings =======================================================
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

;; Ido insert space
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

;; Dired list to dired
(global-set-key (kbd "C-x C-d") #'ido-dired)

;; Compile keybinding
(global-set-key (kbd "C-x c") #'projectile-compile-project)
(global-set-key (kbd "C-x C-c") #'projectile-compile-project)

(global-set-key (kbd "C-x C-r") #'recompile)

;; VIM-like keybindings
(use-package evil
    :ensure t
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1))

(use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init))

;; Quality of life
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

;;; === Writing & Documentation  ===================================================

;; Markdown mode
(setq markdown-command "/usr/local/bin/pandoc")

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
    (setq org-completion-use-ido t)
    (setq org-link-frame-setup '((file . find-file)))
	(setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-preserve-indentation t)
    :config
    (setq org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-2d")
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-todo-list-sublevels '2)
    (setq org-log-done 'time)
    (setq org-agenda-files '("~/Documentations/agenda"))
    (setq org-agenda-use-time-grid nil)
    (setq org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t% s")
                                     (todo . " %i %-12:c")
                                     (tags . " %i %-12:c")
                                     (search . " %i %-12:c")))
    (setq org-refile-targets
        '(("~/Documentations/agenda/archive.org" :maxlevel . 1)))
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
    (setq org-emphasis-alist
        '(("*" (bold))
          ("/" (italic))
          ("_" (underline))))
    (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.0)))

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package org-roam
    :after org
    :ensure t
    :custom
    (org-roam-directory "~/Documentations/wiki")
    :init (setq org-roam-v2-ackt t)
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert))
    :config
    (org-roam-setup))

;; LATEX
(use-package lsp-ltex
    :ensure t
    :init
    (setq lsp-ltex-version "14.0.0")
    :hook (latex-mode . (lambda ()
                        (require 'lsp-ltex)
                        (lsp))))  

;; Display fill column indicator
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'latex-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(set-face-background 'fill-column-indicator "#3c3836")
(setq-default display-fill-column-indicator-column 85)
(setq-default display-fill-column-indicator-character '32)

;; Auto wrap
(visual-line-mode t)

;; Minor writing modes
(delete-selection-mode)

;;; === Org Capture ================================================================
(setq org-capture-templates
    '(("t" "Task" entry
          (file+headline "~/Documentations/agenda/schedular.org" "*Tasks* ")
          "** TODO %? [/]\n   + [ ] ")
         ("b" "Task bookmark" entry
             (file+headline "~/Documentations/agenda/schedular.org" "*Tasks* ")
          "** TODO %? [/]\n   + [ ] \n %a")
         ("f" "Fix" entry
             (file+headline "~/Documentations/agenda/projects.org" "*Tasks* ")
          "** TODO %? \n %i\n %a")
         ("a" "Appointment" entry
             (file+headline "~/Documentations/agenda/schedular.org" "*Schedule* ")
          "** %?")
         ("s" "Socials" entry
             (file+headline "~/Documentations/agenda/socials.org" "*Socials* ")
          "** %?")
         ("r" "Reminder" entry
             (file+headline "~/Documentations/agenda/schedular.org" "*Tasks* ")
          "** TODO %?")))

;;; === Yasnippets & Company fix ===================================================
(defun check-expansion ()
  "Check."
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
    (backward-char 1)
    (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  "Tab to indent."
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas-minor-mode)
        (null (do-yas-expand)))
    (if (check-expansion)
        (progn
          (company-manual-begin)
          (if (null company-candidates)
          (progn
            (company-abort)
            (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field ()
  "Tab to complete."
  (interactive)
  (if (or (not yas-minor-mode)
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
  "Expand the snippet or complete the selection."
  (interactive)
  (if (or (not yas-minor-mode)
      (null (do-yas-expand))
      (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  "Abort."
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

;;; === Compilation & Terminal =====================================================
;; Compilation buffer to horizontal
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

;; Remove autowrap in compile mode
(add-hook 'compilation-mode-hook
          (lambda () (visual-line-mode nil)))

;; Scroll on output
(setq compilation-scroll-output t)

;; Other compile commands
(setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig")
(setq compile-command "make")

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

;; === Custom Set Variables ========================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    '(ansi-color-names-vector
         ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(highlight-indent-guides-method 'column)
    '(package-selected-packages
         '(pdf-tools restart-emacs company-php php-mode lsp-ltex clang-format tree-sitter-langs tree-sitter ido-yes-or-no-mode ido-yes-or-no 'yasnippet 'yasnippet gruvbox-theme base16-theme org-roam hl-todo yasnippet racket-mode sly company-capf package-list ccls unicode-fonts highlight-indent-guides evil-collection eglot flycheck-pkg-config company-irony irony editorconfig buffer-move omnisharp csharp-mode lsp-python-ms company-glsl glsl-mode hungry-delete web-mode json-mode lsp-javacomp tide typescript-mode lsp-mode latex-preview-pane centered-window writeroom-mode platformio-mode magit lua-mode use-package flycheck exec-path-from-shell evil company-c-headers autopair company smex))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
