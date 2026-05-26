;;; init-flycheck.el --- Initialize Flycheck
;;; Commentary:
;;; Code:
(use-package let-alist
  :ensure t)

(use-package flyspell
  ;:defer 1
  :after evil-leader
  :hook ((text-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (latex-mode . flyspell-mode)
         (LaTeX-mode . flyspell-mode)
         (python-mode . flyspell-prog-mode)
         (python-ts-mode . flyspell-prog-mode)
         (emacs-lisp-mode . flyspell-prog-mode))
  :init
  (setq ispell-program-name
        (or (executable-find "aspell")
            (executable-find "hunspell")
            (and (file-executable-p "/opt/homebrew/bin/aspell") "/opt/homebrew/bin/aspell")
            (and (file-executable-p "/usr/local/bin/aspell") "/usr/local/bin/aspell")))
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
 ; (flyspell-mode 1)
  :config
  (add-hook 'flyspell-mode-hook
            (lambda ()
              (evil-define-key 'normal flyspell-mode-map (kbd "]s") 'evil-next-flyspell-error)
              (evil-define-key 'normal flyspell-mode-map (kbd "[s") 'evil-prev-flyspell-error))))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-popup
  :after flyspell-correct)

; (use-package flyspell-correct-popup
;   :after flyspell
;   :bind (:map flyspell-mode-map
;         ("C-;" . flyspell-correct-wrapper))
;   :custom (flyspell-correct-interface 'flyspell-correct-popup))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)

  (defun my-flycheck-org-lint-start (checker callback)
    "Run Org lint and normalize string line numbers for Flycheck."
    (condition-case err
        (let ((errors
               (delq nil
                     (mapcar
                      (lambda (e)
                        (pcase e
                          (`(,_n [,line ,_trust ,desc ,_checker])
                           (flycheck-error-new-at
                            (if (stringp line) (string-to-number line) line)
                            nil 'info desc
                            :checker checker))
                          (_
                           (flycheck-error-new-at
                            1 nil 'warning
                            (format "Unexpected org-lint format: %S" e)
                            :checker checker))))
                      (org-lint)))))
          (funcall callback 'finished errors))
      (error (funcall callback 'errored
                      (error-message-string err)))))

  ;; Flycheck 20260320 passes Org's line value through unchanged, but recent
  ;; Org can return it as a string (for example "179").
  (put 'org-lint 'flycheck-start #'my-flycheck-org-lint-start)

  ;; Flycheck mode:
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (when (featurep 'evil)
                (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
                (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error))
              (when (featurep 'evil-leader)
                (evil-leader/set-key (kbd "e") 'flycheck-list-errors))))

  ;; Override default flycheck triggers
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8
        flycheck-disabled-checkers '(php-phpmd))

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
