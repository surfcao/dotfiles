;;; Character sets

;(when (memq window-system '(x))
;  (add-to-list 'load-path "/usr/local/Cellar/ess/15.09-2/share/emacs/site-lisp/ess/"))

(use-package ess
:load-path "/usr/local/Cellar/ess/15.09-2/share/emacs/site-lisp/ess/"
:mode (("\\.[rR]\\'" . R-mode)
         ("\\.Rnw\\'" . Rnw-mode)
         ("\\.Rmd\\'" . poly-mode))
:init 
(require 'popup)
;(require 'ess-R-object-popup)
(require 'ess-site)
;; No history, no saving!
;(setq inferior-ess-same-window nil)
;(setq inferior-ess-same-window t)
;(setq inferior-ess-own-frame nil)
(setq-default inferior-R-args "--no-restore-history --no-save ")
(setq inferior-ess-program "R")
(setq inferior-R-program-name "R")
:defer t
:commands R
:config 
(add-hook 'ess-mode-hook
          (lambda ()
            (setq ess-use-auto-complete t)
            (setq ess-set-style 'RStudio)
            (setq ess-R-argument-suffix "=")
            (ess-disable-smart-S-assign nil)
            (ess-disable-smart-underscore nil)))

;;; inferior ESS execution mode
(define-key inferior-ess-mode-map (kbd "C-o") 'ess-switch-to-inferior-or-script-buffer )

;; Comint related stuff
;(define-key inferior-ess-mode-map (kbd "C-e 0") 'comint-bol)
;(define-key inferior-ess-mode-map (kbd "C-e j") 'comint-next-input)
;(define-key inferior-ess-mode-map (kbd "C-e k") 'comint-previous-input)
;(define-key inferior-ess-mode-map (kbd "C-e J") 'comint-next-prompt)
;(define-key inferior-ess-mode-map (kbd "C-e K") 'comint-previous-prompt)
;(define-key inferior-ess-mode-map (kbd "C-e ?") 'comint-history-isearch-backward-regexp)
(define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-matching-input-from-input)
(define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-matching-input-from-input)
(define-key inferior-ess-mode-map (kbd "<up>") 'comint-previous-input)
(define-key inferior-ess-mode-map (kbd "<down>") 'comint-next-input)
;(define-key inferior-ess-mode-map (kbd "<up>") 'comint-next-matching-input-from-input)
;(define-key inferior-ess-mode-map (kbd "<down>") 'comint-previous-matching-input-from-input)

;; mimic RStudio shortcuts
(define-key ess-mode-map (kbd "<f4>") 'ess-debug-flag-for-debugging)
(define-key ess-mode-map (kbd "S-<f4>") 'ess-debug-unflag-for-debugging)
(define-key ess-mode-map (kbd "<f5>") 'ess-debug-command-continue)
(define-key ess-mode-map (kbd "<f9>") 'ess-bp-set)
(define-key ess-mode-map (kbd "S-<f9>") 'ess-bp-kill)
(define-key ess-mode-map (kbd "<f10>")  'ess-debug-command-next)
(define-key ess-mode-map (kbd "<f11>")  'ess-debug-command-next-multi)

(setq ess-use-tracebug t) ; permanent activation
;;; Tooltip included in ESS
(setq ess-describe-at-point-method 'tooltip) ; 'tooltip or nil (buffer)

;Help buffers all belong in the same frame.;
(setq ess-help-own-frame nil)
;When commands are executed, display their output within the current
;buffer, rather than to a new dedicated buffer for them.
(setq ess-execute-in-process-buffer +1)
(setq ess-switch-to-end-of-proc-buffer t)
(setq ess-tab-complete-in-script +1)
(setq ess-first-tab-never-complete 'symbol-or-paren-or-punct)
; Use `eldoc' for this mode. Always show it when the point is on a
; symbol. Try to keep help strings at 10 chars or less.
(setq ess-use-eldoc t)
(setq ess-eldoc-show-on-symbol t)
(setq ess-eldoc-abbreviation-style 'normal)

;; Key assignment for delete trailing whitespace
;(add-hook 'ess-mode-hook (setq ess-nuke-trailing-whitespace-p t))
;(define-key ess-mode-map (kbd "s-w") 'ess-nuke-trailing-whitespace)
;(global-set-key (kbd "s-w") 'ess-nuke-trailing-whitespace)

;; Underscore preservation in ESS
;; http://www.r-bloggers.com/a-small-customization-of-ess/
;(setq ess-S-assign-key (kbd "C-="))	; C-= gives <-
;(ess-toggle-S-assign-key t)		; enable above key definition
;(ess-toggle-underscore nil) ; leave my underscore key alone!

;; Must-haves for ESS
;; http://www.emacswiki.org/emacs/CategoryESS
(setq ess-eval-visibly 'nowait)		; New in 12.09-1
(setq ess-ask-for-ess-directory nil) ; Don't ask for directory
;;
;; Auto-scrolling of R console to bottom and Shift key extension
;; http://www.kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/ESSShiftEnter
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq-local comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t))

;(add-to-list 'load-path "/usr/local/Cellar/ess/15.09-2/share/emacs/site-lisp/ess/")
;(add-to-list 'load-path "/usr/share/emacs/site-lisp/ess/")
;(require 'ess-site)
;(require 'popup)


;;; ESS editing mode
;(evil-leader/set-key-for-mode 'ess-mode "l" 'ess-eval-line)
;(evil-leader/set-key-for-mode 'ess-mode "n" 'ess-eval-region-or-line-and-step)
;(evil-leader/set-key-for-mode 'ess-mode "g" 'ess-eval-line-and-go)
(evil-leader/set-key-for-mode 'ess-mode "r" 'ess-eval-region-or-function-or-paragraph-and-step)
(evil-leader/set-key-for-mode 'ess-mode "l" 'ess-eval-region-or-line-and-step)
;(define-key ess-mode-map [(control return)] nil)
;(define-key ess-mode-map (kbd "<s-return>") 'ess-eval-region-or-line-and-step)
;(evil-leader/set-key-for-mode 'ess-mode "bb" 'ess-eval-buffer)
(evil-leader/set-key-for-mode 'ess-mode "x" 'ess-eval-buffer-from-beg-to-here)
(evil-leader/set-key-for-mode 'ess-mode "X" 'ess-eval-buffer-from-here-to-end)

(evil-leader/set-key-for-mode 'ess-mode "q" 'ess-quit)
(evil-leader/set-key-for-mode 'ess-mode "f" 'ess-load-file)
(evil-leader/set-key-for-mode 'ess-mode "d" 'ess-use-this-dir)
(evil-leader/set-key-for-mode 'ess-mode "D" 'ess-set-working-directory)
(evil-leader/set-key-for-mode 'ess-mode "p" 'ess-install-library)
(evil-leader/set-key-for-mode 'ess-mode "P" 'ess-display-package-index)
(evil-leader/set-key-for-mode 'ess-mode "h" 'ess-display-help-on-object)
(evil-leader/set-key-for-mode 'ess-mode "H" 'ess-describe-object-at-point)
(evil-leader/set-key-for-mode 'ess-mode "t" 'ess-show-traceback)
(evil-leader/set-key-for-mode 'ess-mode "T" 'ess-show-call-stack)
(evil-leader/set-key-for-mode 'ess-mode "o" 'ess-switch-to-inferior-or-script-buffer)
;(define-key ess-mode-map (kbd "C-o") 'ess-switch-to-inferior-or-script-buffer )

;;; Show a popup by executing arbitrary commands on object at point.
;;; Inspiration:
;;; blogisticreflections.wordpress.com/2009/10/01/r-object-tooltips-in-ess/

;; emacs.stackexchange.com/questions/696/get-content-of-a-buffer
(defun asb-read-into-string (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun asb-ess-R-object-popup (r-func)
  "R-FUNC: The R function to use on the object.
Run R-FUN for object at point, and display results in a popup."
  (let ((objname (current-word))
        (tmpbuf (get-buffer-create "**ess-R-object-popup**")))
    (if objname
        (progn
          (ess-command (concat "class(" objname ")\n") tmpbuf)
          (let ((bs (asb-read-into-string tmpbuf)))
            (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                (progn
                  (ess-command (concat r-func "(" objname ")\n") tmpbuf)
                  (let ((bs (asb-read-into-string tmpbuf)))
                    (popup-tip bs)))))))
  (kill-buffer tmpbuf)))

(defun asb-ess-R-object-popup-str ()
  (interactive)
  (asb-ess-R-object-popup "str"))

(defun asb-ess-R-object-popup-names ()
  (interactive)
  (asb-ess-R-object-popup "names"))

(defun asb-ess-R-object-popup-nrow ()
  (interactive)
  (asb-ess-R-object-popup "nrow"))

(defun asb-ess-R-object-popup-interactive (r-func)
  (interactive "sR function to execute: ")
  (asb-ess-R-object-popup r-func))

(evil-leader/set-key-for-mode 'ess-mode "i" 'asb-ess-R-object-popup-str)
(evil-leader/set-key-for-mode 'ess-mode "n" 'asb-ess-R-object-popup-names)
(evil-leader/set-key-for-mode 'ess-mode "N" 'asb-ess-R-object-popup-nrow)
(evil-leader/set-key-for-mode 'ess-mode "I" 'asb-ess-R-object-popup-interactive)

;; debug mode doesn't have a hook, stick them straight in the appropriate map  
;;(define-key ess-debug-minor-mode-map (kbd "<f5>")  'ess-debug-command-next)
;;(define-key ess-debug-minor-mode-map (kbd "<f5>")  'ess-debug-command-next)
;;(define-key ess-debug-minor-mode-map (kbd "<f6>")  'ess-debug-command-next-multi)
;;(define-key ess-debug-minor-mode-map (kbd "<f7>")  'ess-debug-command-up)
;;(define-key ess-debug-minor-mode-map (kbd "<f8>") 'ess-debug-command-continue)


(provide 'init-ess)
