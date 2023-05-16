;;; conda environment management 
(use-package conda
     :ensure t
     :init
     (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
     (setq conda-env-home-directory (expand-file-name "~/miniconda3/"))

     ;;:config
     ;;;; if you want interactive shell support, include:
     ;;(conda-env-initialize-interactive-shells)
     ;;;; if you want eshell support, include:
     ;;(conda-env-initialize-eshell)
     ;;;; if you want auto-activation (see below for details), include:
     ;;;;(conda-env-autoactivate-mode t)
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

; remove the warning by Guofeng per: https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
(setq python-shell-completion-native-enable nil)

(provide 'init-python)
