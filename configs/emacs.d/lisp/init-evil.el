;;; init-evil.el -- My evil mode configuration.
;;; Commentary:
;;; Code:
(defun air--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","  'other-window
    "."  'mode-line-other-buffer
    ":"  'eval-expression
    "i"  'gptel-send
    "aa" 'align-regexp
    "a=" 'my-align-single-equals
    "b"  'helm-mini             ;; Switch to another buffer
    "B"  'magit-blame-toggle
    "c"  'comment-dwim
    "d"  'kill-this-buffer
    "D"  'open-current-line-in-codebase-search
    "f"  'helm-imenu            ;; Jump to function in buffer
    "g"  'magit-status
    "m"  'helm-bookmarks
    ;"h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    ;"l"  'whitespace-mode       ;; Show invisible characters
    ;"nn" 'air-narrow-dwim       ;; Narrow to region and enter normal mode
    "nw" 'widen
    "o"  'other-window  ;; C-w o
    "W"  'delete-window  ;; C-w o
    "O"  'delete-other-windows  ;; C-w o
    "p"  'helm-show-kill-ring
    "s"  'ag-project            ;; Ag search from project's root
    ;"r"  'helm-recentf ;; recent files
    ;"R"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display))
    "S"  'delete-trailing-whitespace
    "t"  'ggtags-find-tag-dwim ;'gtags-reindex
    "T"  'ggtags-create-tags ;'gtags-find-tag
    "w"  'save-buffer
    "x"  'helm-M-x
    "TAB"  'evil-switch-to-windows-last-buffer
    "y"  'yank-to-x-clipboard)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame))))

(defun air--config-evil ()
  "Configure evil mode."

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
		  ;MATLAB
		  ;dired-mode
		  eshell-mode
                  flycheck-error-list-mode
                  git-rebase-mode
                  ;octopress-mode
		  ;octopress-server-mode
		  ;octopress-process-mode
		  org-capture-mode
		  sunshine-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-set-initial-state 'calendar-mode 'emacs)
  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Use insert state in these additional modes.
  (dolist (mode '(twittering-edit-mode
		   inferior-ess-mode
		   magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
			  (kbd "/")       'evil-search-forward
			  (kbd "n")       'evil-search-next
			  (kbd "N")       'evil-search-previous
			  (kbd "C-d")     'evil-scroll-down
			  (kbd "C-u")     'evil-scroll-up
			  (kbd "C-w C-w") 'other-window)

  ;; Global bindings.
  ;(define-key evil-normal-state-map (kbd "<down>")  'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "j")  'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "0")  'evil-beginning-of-visual-line)
  (define-key evil-normal-state-map (kbd "$")  'evil-end-of-visual-line)
  ;(define-key evil-normal-state-map (kbd "<up>")    'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "k")    'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "-")       'helm-find-files)
  (evil-define-key 'normal global-map (kbd "C--")     (lambda ()
							(interactive)
							(dired (file-name-directory (buffer-file-name)))))
  (evil-define-key 'normal global-map (kbd "C-`")     (lambda ()
							(interactive)
							(dired (expand-file-name "~"))))
  (define-key evil-normal-state-map (kbd "C-]")     'gtags-find-tag-from-here)
  (define-key evil-normal-state-map (kbd "g/")      'occur-last-search)
  (define-key evil-normal-state-map (kbd "[i")      'show-first-occurrence)
  ;(define-key evil-normal-state-map (kbd "S-SPC")   'air-pop-to-org-agenda)
  (define-key evil-insert-state-map (kbd "C-e")     'end-of-line) ;; I know...
  ; kill to the left; guofeng
  (define-key evil-insert-state-map (kbd "C-u")     (lambda () (interactive) (kill-line 0)))
  (define-key evil-insert-state-map (kbd "C-h")     'backward-delete-char)
  ; period indicates the end of sentence
  (setq sentence-end-double-space nil)


  (evil-define-key 'normal global-map (kbd "C-p")   'helm-projectile)
  (evil-define-key 'normal global-map (kbd "C-S-p") 'helm-projectile-switch-project)
  (evil-define-key 'insert global-map (kbd "s-d")   ''other-windoweval-last-sexp)
  (evil-define-key 'normal global-map (kbd "s-d")   'eval-defun)
  (evil-define-key 'normal global-map (kbd "C-t")   'air-open-eshell)

  (evil-define-key 'normal global-map (kbd "z d")   'dictionary-lookup-definition)


  (evil-define-key 'normal global-map (kbd "\\ \\") 'tiny-menu)
  (evil-define-key 'normal global-map (kbd "\\ f") (tiny-menu-run-item "org-files"))
  (evil-define-key 'normal global-map (kbd "\\ a") (tiny-menu-run-item "org-agendas"))
  (evil-define-key 'normal global-map (kbd "\\ t") (tiny-menu-run-item "org-things"))
  (evil-define-key 'normal global-map (kbd "\\ c") (tiny-menu-run-item "org-captures"))
  (evil-define-key 'normal global-map (kbd "\\ l") (tiny-menu-run-item "org-links"))

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (defun evil-visual-line--mark-org-element-when-heading (&rest args)
    "When marking a visual line in Org, mark the current element.

This function is used as a `:before-while' advice on
`evil-visual-line'; if the current mode is derived from Org Mode and
point is resting on an Org heading, mark the whole element instead of
the line. ARGS are passed to `evil-visual-line' when text objects are
used, but this function ignores them."
    (interactive)
    (if (and (derived-mode-p 'org-mode)
             (org-on-heading-p))
        (not (org-mark-element))
      t))

  (advice-add 'evil-visual-line :before-while #'evil-visual-line--mark-org-element-when-heading)
  ;; My own Ex commands.
  ;(evil-ex-define-cmd "om" 'octopress-status)
  )

(defun air--apply-evil-other-package-configs ()
  "Apply evil-dependent settings specific to other packages."

  (defun next-conflict-marker ()
    (interactive)
    (evil-next-visual-line)
    (if (not (search-forward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-max) t))
        (evil-previous-visual-line))
    (move-beginning-of-line nil))

  (defun previous-conflict-marker ()
    (interactive)
    (search-backward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-min) t)
    (move-beginning-of-line nil))

  ;; PHP
  ;(evil-define-key 'normal php-mode-map (kbd "]n") 'next-conflict-marker)
  ;(evil-define-key 'normal php-mode-map (kbd "[n") 'previous-conflict-marker)
  ;(evil-define-key 'visual php-mode-map (kbd "]n") 'next-conflict-marker)
  ;(evil-define-key 'visual php-mode-map (kbd "[n") 'previous-conflict-marker)

  ;; Dired
  (evil-define-key 'normal dired-mode-map (kbd "C-e") 'dired-toggle-read-only))

(defmacro define-evil-or-global-key (key def &optional state)
  "Define a key KEY with DEF in an Evil map, or in the global map.

If the Evil map for STATE is defined (or `normal' if STATE is not
provided) the key will be defined in that map.  Failing that, it will
be defined globally.

Note that STATE should be provided as an unquoted symbol.

This macro provides a way to override Evil mappings in the appropriate
Evil map in a manner that is compatible with environments where Evil
is not used."
  (let* ((evil-map-name (if state
                            (concat "evil-" (symbol-name state) "-state-map")
                          "evil-normal-state-map"))
         (map (if (boundp (intern evil-map-name))
                  (intern evil-map-name)
                global-map)))
    `(define-key ,map ,key ,def)))

; fix tab key not working in org-mode
(setq evil-want-C-i-jump nil)
(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
; :init
; (setq evil-want-keybinding nil )
  :config
  (add-hook 'evil-mode-hook 'air--config-evil)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (air--config-evil-leader))


;  (use-package evil-magit
;	:ensure t)

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t)

  ; source:  https://github.com/company-mode/company-mode/issues/383
  (evil-declare-change-repeat 'company-complete)

  (evil-mode 1)
  (air--apply-evil-other-package-configs))

;use-package evil-collection
; :after evil
; :ensure t
; :config
; (evil-collection-init '(pdf)))


(provide 'init-evil)
;;; init-evil.el ends here
