;;; Python mode:
(use-package elpy
  :ensure t
  :config
;; For elpy
(setq elpy-rpc-python-command "python3")
;; For interactive shell
(setq python-shell-interpreter "python3")
(elpy-enable))

;(add-hook 'elpy-mode-hook
;(lambda ()
;(set (make-local-variable 'company-backends)
;'((company-dabbrev-code company-yasnippet elpy-company-backend)))))

; ;;; Python mode: virtualenvwrapper
; (use-package virtualenvwrapper
;   :ensure t
;   :config
;   (venv-initialize-interactive-shells)
;   (venv-initialize-eshell)
;   (setq venv-location
;         (expand-file-name "~/Projects/virtualenvs/")))

;;; conda environment management 
(use-package conda
     :ensure t
     :config
     ;; if you want interactive shell support, include:
     (conda-env-initialize-interactive-shells)
     ;; if you want eshell support, include:
     (conda-env-initialize-eshell)
     ;; if you want auto-activation (see below for details), include:
     ;(conda-env-autoactivate-mode t)
     (setq 
  	conda-env-home-directory (expand-file-name "~/opt/anaconda3/") ;; as in previous example; not required
  	conda-env-subdirectory "envs")

     ;; if you want to automatically activate a conda environment on the opening of a file:
     ;;(add-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path) (conda-env-activate-for-buffer))))
     )

(add-hook 'python-mode-hook
          (lambda ()
            ;; I'm rudely redefining this function to do a comparison of `point'
            ;; to the end marker of the `comint-last-prompt' because the original
            ;; method of using `looking-back' to match the prompt was never
            ;; matching, which hangs the shell startup forever.
;            (defun python-shell-accept-process-output (process &optional timeout regexp)
;              "Redefined to actually work."
;              (let ((regexp (or regexp comint-prompt-regexp)))
;                (catch 'found
;                  (while t
;                    (when (not (accept-process-output process timeout))
;                      (throw 'found nil))
;                    (when (= (point) (cdr (python-util-comint-last-prompt)))
;                      (throw 'found t))))))
;
            ;; Additional settings follow.
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(evil-leader/set-key-for-mode 'python-mode "l" 'elpy-shell-send-statement-and-step)
(evil-leader/set-key-for-mode 'python-mode "L" 'elpy-shell-send-statement-and-step-and-go)
(evil-leader/set-key-for-mode 'python-mode "r" 'elpy-shell-send-region-or-buffer-and-step)
(evil-leader/set-key-for-mode 'python-mode "R" 'elpy-shell-send-region-or-buffer-and-step-and-go)
(evil-leader/set-key-for-mode 'python-mode "h" 'elpy-doc)
(evil-leader/set-key-for-mode 'python-mode "c" 'elpy-shell-send-codecell-and-step)
(evil-leader/set-key-for-mode 'python-mode "C" 'elpy-shell-send-codecell-and-step-and-go)
(evil-leader/set-key-for-mode 'python-mode "f" 'elpy-format-code)
(evil-leader/set-key-for-mode 'python-mode "q" 'elpy-shell-kill)
(evil-leader/set-key-for-mode 'python-mode "o" 'elpy-shell-switch-to-shell)
(evil-leader/set-key-for-mode 'python-mode "g" 'elpy-goto-definition)
(evil-leader/set-key-for-mode 'python-mode "G" 'elpy-goto-definition-other-window)

; remove the warning by Guofeng per: https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
(setq python-shell-completion-native-enable nil)

(provide 'init-python)
