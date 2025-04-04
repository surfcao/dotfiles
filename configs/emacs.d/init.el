;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-
;;; Based on Aaron Beiber's Emacs configuration
;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:

;; Leave this here, or package.el will just add it again.
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-utils)
(require 'init-elpa)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
;(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
;(setq split-width-threshold nil)
; prefer split vertically
(setq split-height-threshold nil)
(setq split-width-threshold 0)
;(setq split-window-preferred-function nil)
(setq custom-safe-themes t)
(put 'narrow-to-region 'disabled nil)
; default maximize frame size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; silent the bell
(setq ring-bell-function 'ignore)

; add a margin on top of the buffer
;(setq header-line-format " ")
;(set-face-attribute 'header-line nil :background (face-attribute 'default :background))

(defun my-minibuffer-setup-hook ()
  "Increase GC cons threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Set GC cons threshold to its default value."
  (setq gc-cons-threshold 1000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq auto-save-file-name-transforms (list (list ".*" backup-dir )))
;(setq make-backup-files nil)
;(setq auto-save-default nil)

;;; File type overrides.
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))

;;; My own configurations, which are bundled in my dotfiles.
(require 'init-platform)
(require 'init-global-functions)

;; password
(use-package auth-source-pass
  :ensure t
  :config
  (auth-source-pass-enable)
  (setq auth-sources '(password-store))
  (cl-defun
      auth-get-passwd
      (&rest spec &allow-other-keys)
    "Helper to get the password given the SPEC from authsource."
    (let ((founds (apply 'auth-source-pass-search spec)))
      (when founds
        (funcall (plist-get (nth 0 founds) :secret))))))



(use-package popup
	     :ensure t)

(use-package ggtags
	     :ensure t
	     :diminish ggtags-mode
	     :config
	     (add-hook 'prog-mode-hook 'ggtags-mode) 
	     (add-hook 'matlab-mode-hook 'ggtags-mode))

;;; tiny-menu configuration
(use-package tiny-menu
     :ensure t
     :commands (tiny-menu-run-item)
     :config
     (setq tiny-menu-items
           '(("org-things"   ("Org Things"
                              ((?t "Tag"      org-tags-view)
                               (?i "ID"       air-org-goto-custom-id)
                               (?k "Keyword"  org-search-view)
                               (?h "Headings" helm-org-agenda-files-headings))))
             ("org-agendas"  ("Org Agenda Views"
                              ((?a "All"      air-pop-to-org-agenda)
                               (?c "Calendar" calendar)
                               )))
             ("org-links"    ("Org Links"
                              ((?c "Capture"   org-store-link)
                               (?l "Insert"    org-insert-link)
                               (?i "Custom ID" air-org-insert-custom-id-link))))
             ("org-files"    ("Org Files"
                              ((?t "TODO-Work"  (lambda () (air-pop-to-org-todo-work nil)))
                              (?f "TODO-Life"  (lambda () (air-pop-to-org-todo-life nil)))
                               (?n "Notes" (lambda () (air-pop-to-org-notes nil)))
                               (?m "Meetings" (lambda () (air-pop-to-org-meetings nil)))
                               (?i "Ideas" (lambda () (air-pop-to-org-ideas nil)))
                               (?d "Today" (lambda () (air-pop-to-org-today nil)))
                               (?v "Vault" (lambda () (air-pop-to-org-vault nil))))))
             ("org-captures" ("Org Captures"
                              ((?t "TODO"  air-org-task-capture)
                               (?n "Note"  (lambda () (interactive) (org-capture nil "n")))
                               (?m "Meeting"  (lambda () (interactive) (org-capture nil "m")))
                               (?i "Idea"  (lambda () (interactive) (org-capture nil "i")))
                               (?j "Journal"  (lambda () (interactive) (org-capture nil "j")))
                               (?k "Online"  (lambda () (interactive) (org-capture nil "k")))))))))

(use-package diminish
  :ensure t)

(require 'init-fonts)
(require 'init-gtags)
(require 'init-evil)
(require 'init-maps)
(require 'init-python)
(require 'init-ess)
(require 'init-matlab)
;(require 'init-twitter)
(require 'init-w3m)
;(require 'init-php)
;(require 'init-powerline)
(require 'init-flycheck)
;(require 'init-tmux)
(require 'init-llm)

;;; <EGLOT> configuration, pick this or the LSP configuration but not both.
;; Using Eglot with Pyright, a language server for Python.
;; See: https://github.com/joaotavora/eglot.
;default for pyright
(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-o" . python-sort-imports)
              ("C-c C-f" . eglot-format-buffer))
 :hook ((python-mode . eglot-ensure)
	(python-ts-mode . eglot-ensure)
         (python-ts-mode . flyspell-prog-mode)
         (python-ts-mode . superword-mode)
         (python-ts-mode . hs-minor-mode)
         (python-ts-mode . (lambda () (set-fill-column 88))))
 :config 
(add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
(add-to-list 'eglot-server-programs '(python-ts-mode .  ("pyright-langserver" "--stdio")))
(setq-default eglot-workspace-configuration
      	   (list (cons ':python (list ':venvPath conda-env-current-path ':pythonPath (concat conda-env-current-path "/bin/python"))))))


; disable flymake-mode, use flycheck
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
;; glues eglot and flycheck
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;(require 'markdown-preview-mode)
;(add-hook 'markdown-preview-mode-hook
;          (lambda ()
;            (setq markdown-preview-template
;                  (expand-file-name "~/.emacs.d/markdown-preview.html" user-emacs-directory))
;            (setq markdown-preview-style
;                  "http://aaronbieber.com/assets/styles/github-markdown.css")))

;(add-to-list 'load-path (expand-file-name "fence-edit" user-emacs-directory))
;(require 'fence-edit)

;; Utilities
(use-package s
  :ensure t
  :defer 1)
(use-package dash :ensure t)

;; map yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package visual-fill-column :ensure t) 

;; Org Mode
(require 'init-org)
;;(require 'init-org-cite)
(require 'init-org-ref)
(require 'init-pdf)
;(require 'init-notes)

(use-package org-pomodoro
    :ensure t
    :config
    (add-hook 'org-pomodoro-started-hook 
	      (lambda()
		(setq mode-line-format (default-value 'mode-line-format))
		(setq writeroom--mode-line-showing t))
	        (force-mode-line-update))
    (setq org-pomodoro-length 90)
    (setq org-pomodoro-short-break-length 0)
    (setq org-pomodoro-long-break-frequency 1)
    (setq org-pomodoro-long-break-length 0))

;(use-package rainbow-mode
;  :ensure t
;  :commands rainbow-mode)

;(use-package css-mode
;  :ensure t
;  :config
;  (add-hook 'css-mode-hook (lambda ()
;                             (rainbow-mode))))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (defadvice wgrep-change-to-wgrep-mode (after wgrep-set-normal-state)
    (if (fboundp 'evil-normal-state)
        (evil-normal-state)))
  (ad-activate 'wgrep-change-to-wgrep-mode)

  (defadvice wgrep-finish-edit (after wgrep-set-motion-state)
    (if (fboundp 'evil-motion-state)
        (evil-motion-state)))
  (ad-activate 'wgrep-finish-edit))

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup))

(use-package ag
  :ensure t
  :commands (ag ag-project)
  :config
  (add-hook 'ag-mode-hook
            (lambda ()
              (wgrep-ag-setup)
              (define-key ag-mode-map (kbd "n") 'evil-search-next)
              (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
  (setq ag-executable "/usr/local/bin/ag")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

;(use-package js2-mode
;  :ensure t
;  :mode "\\.js\\'")

;(use-package exec-path-from-shell
;  :ensure t
;  :defer t
;  :config
;  (when (memq window-system '(mac ns))
;    (exec-path-from-shell-initialize)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  ; cycling across multiple sources
  (setq helm-move-to-line-cycle-in-source nil)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)
  (setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t))

(defun my-text-mode-company-setup ()
  "Add company-ispell and company-dabbrev to company-backends for text-mode."
  (set (make-local-variable 'company-backends)
       (append '((company-yasnippet company-ispell)) company-backends)))

(add-hook 'text-mode-hook 'my-text-mode-company-setup)

;(use-package s :ensure t)
(use-package company
  :ensure t
  ;:defer t
  :init
  (global-company-mode)
  :config
  ;(setq company-tooltip-common-selection ((t (:inherit company-tooltip-selection :background "yellow2" :foreground "#c82829"))))
  ;(setq company-tooltip-selection ((t (:background "yellow2"))))
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "ESC") 'company-abort)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;(define-key company-active-map (kbd "s-f") 'company-files)
  ;(define-key company-active-map (kbd "C-y") 'company-yasnippet)
 ; (global-set-key (kbd "C-y") 'company-yasnippet)
  (global-set-key (kbd "s-f") 'company-files))

(use-package counsel :ensure t)
(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . counsel-grep-or-swiper)
  :config
  (require 'counsel)
  (setq counsel-grep-base-command "grep -niE \"%s\" %s")
  (setq ivy-height 10))

(use-package dictionary :ensure t)

;(use-package emmet-mode
;  :ensure t
;  :commands emmet-mode)

; for writing
(use-package abbrev
  :defer 1
  :ensure nil
  :custom
  ;(abbrev-file-name (expand-file-name ".abbrev_defs" user-emacs-directory))
  (abbrev-file-name "~/Dropbox/emacs/abbrev_defs")
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

  (dolist (hook '( latex-mode-hook
		 ;;markdown-mode-hook
		;; text-mode-hook
		 org-mode-hook))
  (add-hook hook 
	    (lambda ()
	    (writeroom-mode)
            (abbrev-mode)
	    (setq header-line-format " ")
	   (set-face-attribute 'header-line nil :background (face-attribute 'default :background)))))

  (dolist (hook '(matlab-mode-hook))
  (add-hook hook 
	    (lambda ()
	      (nlinum-relative-mode)
	      )))


;(use-package flycheck
;  :ensure t
;  :commands flycheck-mode
;  :config
;  ;;; Flycheck mode:
;  (add-hook 'flycheck-mode-hook
;          (lambda ()
;            (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
;            (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error))))

(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project)
  :ensure t
  :init
  (helm-projectile-on)
    (setq projectile-switch-project-action 'helm-projectile)
      (defvar helm-source-file-not-found
	    (helm-build-dummy-source
	              "Create file"
		            :action 'find-file))
        (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  ;;; Markdown mode:
  (setq markdown-enable-math t)
  (add-hook 'markdown-mode-hook (lambda ()
				  (yas-minor-mode t)
				  (set-fill-column 75)
				  (turn-on-auto-fill)
				  ;;(writeroom-mode)
				  ; hide the header line to increase top margin
				  ;(setq header-line-format " ")
				  ;(set-face-attribute 'header-line nil :background "white")
				  ;(set-face-attribute 'header-line nil :background (face-attribute 'default :background))
				  (setq-default line-spacing 5)
				  (flyspell-mode)))
  (setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  (define-key markdown-mode-map (kbd "C-\\")  'markdown-insert-list-item)
  (define-key markdown-mode-map (kbd "C-c '") 'fence-edit-code-at-point)
  (define-key markdown-mode-map (kbd "C-c 1") 'markdown-insert-header-atx-1)
  (define-key markdown-mode-map (kbd "C-c 2") 'markdown-insert-header-atx-2)
  (define-key markdown-mode-map (kbd "C-c 3") 'markdown-insert-header-atx-3)
  (define-key markdown-mode-map (kbd "C-c 4") 'markdown-insert-header-atx-4)
  (define-key markdown-mode-map (kbd "C-c 5") 'markdown-insert-header-atx-5)
  (define-key markdown-mode-map (kbd "C-c 6") 'markdown-insert-header-atx-6))


(use-package pandoc-mode
  :ensure t
  :hook (markdown-mode . pandoc-mode))

;;; MARKDOWN
;(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;;; R modes

(use-package poly-R
  :ensure t
  :defer t)

(use-package poly-markdown
  :ensure t
  :defer t)

(use-package polymode
  :ensure t
  :mode
    ;; R modes
    ("\\.Snw" . poly-noweb+r-mode)
    ("\\.Rnw" . poly-noweb+r-mode)
    ("\\.[rR]md" . Rmd-mode)
  :init 
   (progn
    (defun Rmd-mode ()			
      "ESS Markdown mode for Rmd files"
      (interactive)
      (require 'poly-R)
      (require 'poly-markdown)
      (R-mode)
      (poly-markdown+r-mode))))

;(use-package php-extras :ensure t :defer t)
;(use-package sunshine
;  :ensure t
;  :commands sunshine-forecast
;  :config
;  (defun get-string-from-file (file-path)
;    "Return FILE-PATH's contents."
;    (with-temp-buffer
;      (insert-file-contents file-path)
;      (buffer-string)))
;  (setq sunshine-appid (get-string-from-file
;                        (expand-file-name "sunshine-appid" user-emacs-directory)))
;  (setq sunshine-location "Brookline, MA")
;  (setq sunshine-show-icons t))

(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2))

;(use-package mmm-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  ;; Bind `SPC' to `yas-expand' when snippet expansion available (it
  ;; will still call `self-insert-command' otherwise).
  ;(define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
  ;(define-key yas-minor-mode-map [tab] yas-maybe-expand)
  ;; Bind `C-c y' to `yas-expand' ONLY.
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           "~/.emacs.d/remote-snippets"))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt))
  ; fix tab in org-mode
  (defun yas-org-very-safe-expand ()
     (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
   (add-hook 'org-mode-hook
      (lambda ()
        (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
        (define-key yas-keymap [tab] 'yas-next-field)))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :diminish ""
  :config
  (which-key-mode t))

(use-package projectile
  :ensure t
  :defer 1
  ;:diminish projectile-mode
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line
        '(:eval
          (format " Proj[%s]"
                  (projectile-project-name)))))

(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.5))

(use-package magit-popup
  :ensure t
  :defer t
  :config 
   (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent"))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))

;(use-package mmm-mode
;  :ensure t
;  :defer t
;  :config
;  (setq mmm-global-mode 'maybe)
;  (mmm-add-classes
;   '((markdown-cl
;      :submode emacs-lisp-mode
;      :face mmm-declaration-submode-face
;      :front "^~~~cl[\n\r]+"
;      :back "^~~~$")
;     (markdown-php
;      :submode php-mode
;      :face mmm-declaration-submode-face
;      :front "^```php[\n\r]+"
;      :back "^```$")))
;  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-cl)
;  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-php))

(use-package iceberg-theme :ensure t)
(use-package nordic-night-theme :ensure t)
(use-package nord-theme :ensure t)
(use-package sublime-themes :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(use-package gruvbox-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(load-theme 'nord t)
;(load-theme 'iceberg t)

;(use-package modus-themes
;	     :ensure
;	     :init
;	     ;; Configure the Modus Themes' appearance
;	     (setq modus-themes-mode-line '(accented borderless)
;		   modus-themes-bold-constructs t
;		   modus-themes-italic-constructs t
;		   modus-themes-variable-pitch-ui t
;		   modus-themes-mixed-fonts t
;		   modus-themes-fringes 'subtle
;		   modus-themes-to-toggle '(modus-vivendi-tinted modus-vivendi-deuteranopia)
;		   modus-themes-tabs-accented t
;		   modus-themes-paren-match '(bold intense)
;		   modus-themes-prompts '(bold intense)
;		   ;; Make the fringe invisible
;		   modus-themes-common-palette-overrides
;		   '((fringe unspecified))
;		   modus-themes-org-blocks 'tinted-background
;		   modus-themes-scale-headings t
;		   modus-themes-region '(bg-only)
;		   modus-themes-headings
;		   '((1 . (rainbow 1.3))
;		     (2 . (rainbow 1.2))
;		     (3 . (rainbow bold 1.1))
;		     (4 . (rainbow bold 1.0))
;		     (t . (semilight 1.0)))))
;(load-theme 'modus-vivendi t)



;(use-package nordic-night-theme
;  :ensure t
;  :config
;  ;; Use this for the darker version
;  ;; (load-theme 'nordic-midnight t)
;  (load-theme 'nordic-night t))


;(load-theme 'solarized-iceberg-dark t)
;(use-package iceberg-theme
;  :ensure t
;  :config
;  (iceberg-theme-create-theme-file)
;  )

(use-package undo-tree
  :ensure t
  :diminish t
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        (list (cons "." (expand-file-name "undo-tree-history" user-emacs-directory)))))

;;; Helpers for GNUPG, which I use for encrypting/decrypting secrets.
(require 'epa-file)
(epa-file-enable)
(setq-default epa-file-cache-passphrase-for-symmetric-encryption t)

(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")


;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "M-<return>") 'eval-last-sexp)))

;;; If `display-line-numbers-mode' is available (only in Emacs 26),
;;; use it! Otherwise, install and run nlinum-relative.
(if (functionp 'display-line-numbers-mode)
      (and (add-hook 'display-line-numbers-mode-hook
       	       (lambda () (setq display-line-numbers-type 'relative)))
           (add-hook 'prog-mode-hook #'display-line-numbers-mode))
  (use-package nlinum-relative
		    :ensure t
		    :config
		    (nlinum-relative-setup-evil)
		    (setq nlinum-relative-redisplay-delay 0)
		    (add-hook 'prog-mode-hook #'nlinum-relative-mode)))

;;; The Emacs Shell
(defun company-eshell-history (command &optional arg &rest ignored)
  "Complete from shell history when starting a new line.

Provide COMMAND and ARG in keeping with the Company Mode backend spec.
The IGNORED argument is... Ignored."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell-history))
    (prefix (and (eq major-mode 'eshell-mode)
                 (let ((word (company-grab-word)))
                   (save-excursion
                     (eshell-bol)
                     (and (looking-at-p (s-concat word "$")) word)))))
    (candidates (remove-duplicates
                 (->> (ring-elements eshell-history-ring)
                      (remove-if-not (lambda (item) (s-prefix-p arg item)))
                      (mapcar 's-trim))
                 :test 'string=))
    (sorted t)))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill term buffer when term is ended."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Eshell things
(defun air--eshell-clear ()
  "Clear an eshell buffer and re-display the prompt."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun air--eshell-mode-hook ()
  "Eshell mode settings."
  (define-key eshell-mode-map (kbd "C-u") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-l") 'air--eshell-clear)
  (define-key eshell-mode-map (kbd "C-d") (lambda () (interactive)
                                            (kill-this-buffer)
                                            (if (not (one-window-p))
                                                (delete-window))))
  (set (make-local-variable 'pcomplete-ignore-case) t)

;; disable company mode in remote terminal
  (if (file-remote-p default-directory)
       (company-mode -1)
       (set (make-local-variable 'company-backends)
             '((company-shell company-eshell-history)))))

(add-hook 'eshell-mode-hook 'air--eshell-mode-hook)

;;; Magit mode (which does not open in evil-mode):
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

;;; Git Commit Mode (a Magit minor mode):
(add-hook 'git-commit-mode-hook 'evil-insert-state)

;;; Emmet mode:
;(add-hook 'emmet-mode-hook
;          (lambda ()
;            (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
;            (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point)))
;
;; Web mode:
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-style-padding 2)
            (yas-minor-mode t)
            (emmet-mode)
            (flycheck-add-mode 'html-tidy 'web-mode)
            (flycheck-mode)))

(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-php-extras ac-source-yasnippet ac-source-gtags ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
        ("css" . (ac-source-css-property ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))))

(add-hook 'web-mode-before-auto-complete-hooks
          #'(lambda ()
             (let ((web-mode-cur-language (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;;; Emacs Lisp mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (yas-minor-mode t)
            (eldoc-mode)
            (highlight-symbol-mode)
            (define-key emacs-lisp-mode-map (kbd "M-<return>") 'eval-last-sexp)))

;;; SH mode:
(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 2)
                          (setq sh-indentation 2)))

;;; Twittering mode:
;(setq twittering-use-master-password t)
;(add-hook 'twittering-mode-hook (lambda ()
;                                  (define-key twittering-mode-map (kbd "C-c C-a") 'twittering-favorite)
;                                  (define-key twittering-mode-map (kbd ",b") 'helm-mini)))
;
;(add-hook 'twittering-edit-mode-hook (lambda ()
;                                       (flyspell-mode)))
;
;;;; Javascript mode:
;(add-hook 'javascript-mode-hook (lambda ()
;                                  (set-fill-column 120)
;                                  (turn-on-auto-fill)
;                                  (setq js-indent-level 2)))
;

;;; HTML mode:
;(add-hook 'html-mode-hook (lambda ()
;                            (setq sgml-basic-offset 2)
;                            (setq indent-tabs-mode nil)))

;(defun find-php-functions-in-current-buffer ()
;  "Find lines that appear to be PHP functions in the buffer.
;
;This function performs a regexp forward search from the top
;\(point-min) of the buffer to the end, looking for lines that
;appear to be PHP function declarations.
;
;The return value of this function is a list of cons in which
;the car of each cons is the bare function name and the cdr
;is the buffer location at which the function was found."
;  (save-excursion
;    (goto-char (point-min))
;    (let (res)
;      (save-match-data
;        (while (re-search-forward  "^ *\\(public \\|private \\|protected \\|static \\)*?function \\([^{]+\\)" nil t)
;          (let* ((fn-name (save-match-data (match-string-no-properties 2)))
;                 (fn-location (save-match-data (match-beginning 0))))
;            (setq res
;                  (append res
;                          (list `(,fn-name . ,fn-location)))))))
;      res)))

(put 'narrow-to-region 'disabled nil)

;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;; used in OS X. Disable sRGB before setting up Powerline.
(when (memq window-system '(mac ns))
  (setq ns-use-srgb-colorspace nil))

;; move around windows
(use-package buffer-move
  :ensure t
  :init
  (bind-key (kbd "<C-S-up>") 'buf-move-up)
  (bind-key (kbd "<C-S-down>") 'buf-move-down)
  (bind-key (kbd "<C-S-left>") 'buf-move-left)
  (bind-key (kbd "<C-S-right>") 'buf-move-right))

;(use-package nord-theme
;	     :ensure t)
;(load-theme 'nord t)
;;; GDB settings
(add-hook 'gud-mode-hook #'(lambda () 
			;;; on mouse: print value
			    (gud-tooltip-mode t)
			;;; buffers
			    (setq gdb-many-windows t)
			;;; io buffer
			;(setq gdb-use-separate-io-buffer t)
			;;; show mini buffer in gud
			(setq gud-tooltip-echo-area nil)))

;; Shortcuts for GDB/debugging in general, mimic VC shortcuts
(global-set-key (kbd "<f5>") 'gud-cont)
(global-set-key (kbd "<f9>") 'gud-break)
(global-set-key (kbd "S-<f9>") 'gud-remove)
(global-set-key (kbd "<f11>") 'gud-step)
(global-set-key (kbd "S-<f11>") 'gud-up)
(global-set-key (kbd "<f10>") 'gud-next) 
(global-set-key (kbd "S-<f5>") 'gud-finish) 

;; Distraction-free editing.
(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode)
  :config
  ;(global-writeroom-mode 1)
  (add-to-list 'writeroom-global-effects 'visual-line-mode)
  ;(add-to-list 'writeroom-major-modes 'latex-mode)
  ;(add-to-list 'writeroom-major-modes 'markdown-mode)
  ;(add-to-list 'writeroom-major-modes 'org-mode)
  ;(add-to-list 'writeroom-major-modes 'text-mode)

 ; (setq header-line-format " ")
  (setq writeroom-extra-line-spacing 0.5)
  (setq writeroom-restore-window-config t
        writeroom-width 80)

  (setq writeroom-mode-line-toggle-position 'mode-line-format)
  (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
  (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width))



(use-package writegood-mode
  :ensure t
  :config
  (global-set-key "\C-cg" 'writegood-mode))

(use-package smartparens
  :ensure t
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package rainbow-delimiters
	    :ensure t )

(require 'init-tex)

;(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
;(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

(eval-after-load 'comint
    '(progn      
         (define-key comint-mode-map (kbd "<up>") 
            #'comint-previous-matching-input-from-input)
         (define-key comint-mode-map (kbd "<down>")
            #'comint-next-matching-input-from-input)))

 (defun my-insert-file-name (filename &optional args)
    "Insert name of file FILENAME into buffer after point.
  
  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.
  
  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.
  
  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
    ;; Based on insert-file in Emacs -- ashawley 20080926
    (interactive "*fInsert file name: \nP")
    (cond ((eq '- args)
           (insert (file-relative-name filename)))
          ((not (null args))
           (insert (expand-file-name filename)))
          (t
           (insert filename))))
  
  (global-set-key (kbd "C-c b i") 'my-insert-file-name)

;;(use-package org-journal
;;  :ensure t
;;  :defer t
;;  :config
;;  (setq org-journal-dir "~/Dropbox/org/notes/journal/"
;;        org-journal-file-format "%Y%m%d.org"
;;        org-journal-date-format "%A, %d %B %Y")
;;
;;  (defun org-journal-save-entry-and-exit()
;;  "Simple convenience function.
;;  Saves the buffer of the current day's entry and kills the window
;;  Similar to org-capture like behavior"
;;  (interactive)
;;  (save-buffer)
;;  (kill-buffer-and-window))
;;  (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit))

(use-package deft
  :after org
  :bind
  ("<f7>" . deft)
  :init
  (setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")
        (case-fn . downcase)))
  :custom
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-extensions '("md" "txt" "org"))
  (deft-default-extension "org")
  (deft-directory (expand-file-name "~/Dropbox/org/")))

(provide 'init)
;;; init.el ends here
