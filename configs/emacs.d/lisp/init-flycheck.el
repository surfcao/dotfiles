;;; init-flycheck.el --- Initialize Flycheck
;;; Commentary:
;;; Code:
(use-package let-alist
  :ensure t)

(use-package flyspell
  :defer 1
  :after evil-leader
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
 ; (flyspell-mode 1)
  :config
  (add-hook 'flyspell-mode-hook
            'flyspell-buffer
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

  ;; Flycheck mode:
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (when (maybe-require-package 'evil)
                (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
                (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error))
              (when (maybe-require-package 'evil-leader)
                (evil-leader/set-key (kbd "e") 'flycheck-list-errors))))

  ;; Override default flycheck triggers
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8
        flycheck-disabled-checkers '(php-phpmd))

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
