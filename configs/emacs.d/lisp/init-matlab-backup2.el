;; Setting up matlab-mode

(use-package matlab-load
 :load-path "~/.emacs.d/matlab-emacs-src"
 :init (load-library "matlab-load")
 :config
(add-hook 'matlab-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-matlab)))
(add-hook 'matlab-shell-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-matlab-shell)))

(add-hook 'matlab-mode 'auto-complete-mode)

(setq matlab-indent-function t)
(setq fill-column 75)
(turn-on-auto-fill)
(setq matlab-indent-function-body t)  ; if you want function bodies indented
(setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
(matlab-cedet-setup)

(setq-local comint-prompt-read-only nil)
(when (memq window-system '(mac ns))
(setq matlab-shell-command "/Applications/MATLAB_R2018b.app/bin/matlab"))
(when (memq window-system '(x))
(setq matlab-shell-command "matlab"))
(setq matlab-shell-command-switches (list "-nodesktop" "-nosplash")))

;(add-to-list 'load-path "~/.emacs.d/matlab-emacs")
;(load-library "matlab-load")

;(setq auto-mode-alist
;    (cons
;     '("\\.m$" . matlab-mode)
;     auto-mode-alist))
;(add-to-list 'company-backends 'company-matlab)
;(add-to-list (make-local-variable 'company-backends) 'company-matlab)


;(add-hook 'matlab-mode-hook
;          (lambda ()
;            (add-to-list (make-local-variable 'company-backends) '(company-matlab))))
;(add-hook 'matlab-shell-mode-hook
;          (lambda ()
;            (add-to-list (make-local-variable 'company-backends) '(company-matlab))))

;; To use the company completion engine, add company-matlab to company-mode backends list:
;; NOTE! In order for the completions to work Matlab shell must be started!
;;(add-to-list 'company-backends 'company-anaconda)

;; To use the flycheck mlint backedn, add the following to your init file:
;; NOTE! Matlab binary folder, e.g. /opt/MATLAB/R2013a/bin must be in your path for this to work.
(eval-after-load 'flycheck
  '(require 'flycheck-matlab-mlint))

;(add-hook 'matlab-mode-hook 'auto-complete-mode)
(evil-leader/set-key-for-mode 'matlab-mode "l" 'matlab-shell-run-region-or-line)
(evil-leader/set-key-for-mode 'matlab-mode "r" 'matlab-shell-run-region)
(evil-leader/set-key-for-mode 'matlab-mode "i" 'matlab-shell-describe-variable)
(evil-leader/set-key-for-mode 'matlab-mode "h" 'matlab-shell-describe-command)
(evil-leader/set-key-for-mode 'matlab-mode "c" 'matlab-shell-run-cell)
(evil-leader/set-key-for-mode 'matlab-mode "I" 'matlab-shell-run-command)

(provide 'init-matlab)
