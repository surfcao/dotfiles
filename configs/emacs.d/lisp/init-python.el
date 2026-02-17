
(setq python-shell-interpreter
      (or (executable-find "python3")
          (expand-file-name "~/miniconda3/bin/python")))
;;; conda environment management 
(use-package conda
     :ensure t
     :init
     (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
     (setq conda-env-home-directory (expand-file-name "~/miniconda3/"))
     (setq conda-env-subdirectory "envs")
     :config
     ;; comment out for now
     ;(conda-env-activate (getenv "CONDA_DEFAULT_ENV"))
     (setq-default mode-line-format (cons mode-line-format '(:exec conda-env-current-name)))
     ;; if you want interactive shell support, include:
     ;;(conda-env-initialize-interactive-shells)
     ;; if you want eshell support, include:
     ;;(conda-env-initialize-eshell)
     ;; if you want auto-activation (see below for details), include:
     ;;(conda-env-autoactivate-mode t)
     )

; (use-package pyvenv
;   :ensure t
;   :defer t
;   :init
;   (pyvenv-mode 1)
;   (setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs")))

;; `python.el' provides `python-mode' which is the builtin major-mode for the
;; Python language.
;; python.el vs python-mode.el
(use-package python-mode
  :defer t
;  :mode ("\\.py\\'" . python-ts-mode)
  :mode ("\\.py\\'" . python-mode)
  :config 
   ; remove the warning by Guofeng per: https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
  (setq python-shell-completion-native-enable nil)
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))


(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(evil-leader/set-key-for-mode 'python-mode "F" 'python-shell-send-file)
(evil-leader/set-key-for-mode 'python-mode "r" 'python-shell-send-region)

;; Hide the modeline for inferior python processes.  This is not a necessary
;; package but it's helpful to make better use of the screen real-estate at our
;; disposal. See: https://github.com/hlissner/emacs-hide-mode-line.
;(use-package hide-mode-line
;  :ensure t
;  :defer t
;  :hook (inferior-python-mode . hide-mode-line-mode))

;;<OPTIONAL> I use poetry (https://python-poetry.org/) to manage my python environments.
;; See: https://github.com/galaunay/poetry.el.
;; There are alternatives like https://github.com/jorgenschaefer/pyvenv.
;(use-package poetry
;  :ensure t
;  :defer t
;  :config
;  ;; Checks for the correct virtualenv. Better strategy IMO because the default
;  ;; one is quite slow.
;  (setq poetry-tracking-strategy 'switch-buffer)
;  :hook (python-mode . poetry-tracking-mode))

;; <OPTIONAL> Buffer formatting on save using black.
;; See: https://github.com/pythonic-emacs/blacken.
(use-package blacken
  :ensure t
  :defer t
  :custom
  (blacken-allow-py36 t)
  (blacken-skip-string-normalization t)
  :hook (python-mode-hook . blacken-mode))

;; <OPTIONAL> Numpy style docstring for Python.  See:
;; https://github.com/douglasdavis/numpydoc.el.  There are other packages
;; available for docstrings, see: https://github.com/naiquevin/sphinx-doc.el
;(use-package numpydoc
;  :ensure t
;  :defer t
;  :custom
;  (numpydoc-insert-examples-block nil)
;  (numpydoc-template-long nil)
;  :bind (:map python-mode-map
;              ("C-c C-n" . numpydoc-generate)))


(provide 'init-python)
