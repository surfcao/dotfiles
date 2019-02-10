;;; Python mode:
(use-package elpy
  :ensure t
  :config
(elpy-enable))

;;; Python mode:
(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location
        (expand-file-name "~/Projects/virtualenvs/")))

(add-hook 'python-mode-hook
          (lambda ()
            ;; I'm rudely redefining this function to do a comparison of `point'
            ;; to the end marker of the `comint-last-prompt' because the original
            ;; method of using `looking-back' to match the prompt was never
            ;; matching, which hangs the shell startup forever.
            (defun python-shell-accept-process-output (process &optional timeout regexp)
              "Redefined to actually work."
              (let ((regexp (or regexp comint-prompt-regexp)))
                (catch 'found
                  (while t
                    (when (not (accept-process-output process timeout))
                      (throw 'found nil))
                    (when (= (point) (cdr (python-util-comint-last-prompt)))
                      (throw 'found t))))))

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

(provide 'init-python)
