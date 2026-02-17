;;; set up llm and copilot 
(use-package gptel
	     :ensure t
	     :config
	     ;(setq gptel-debug t)
	     (auth-source-pass-enable)

	     ;; OPTIONAL configuration
	     (setq gptel-model 'gpt-4o
		   gptel-backend (gptel-make-openai "Github Models" ;Any name you want
						    :host "models.inference.ai.azure.com"
						    :endpoint "/chat/completions?api-version=2024-05-01-preview"
						    :stream t
						    :key (auth-get-passwd :host "github.com" :max 1)
						    :models '("gpt-4o")))

	     (gptel-make-gemini "gemini" 
				:key (auth-get-passwd :host "Gemini" :max 1) 
				:stream t
				:models '("gemini-2.5-pro" "gemini-2.5-flash"))

	     (gptel-make-ollama "Ollama"             ;Any name of your choosing
				:host "localhost:11434"               ;Where it's running
				:stream t                             ;Stream responses
				:models '("mistral:latest" "deepseek-r1:8b" "deepseek-r1:32b"))          ;List of models

	     )

(use-package copilot
  :ensure t 
  :custom
  (copilot-max-char -1)
  (copilot-indent-offset-warning-disable t)

  :config
  ;; Avoid blocking startup on server install checks.
  (run-with-idle-timer
   2 nil
   (lambda ()
     (unless (copilot-installed-version)
       (copilot-install-server))))

  (add-hook 'prog-mode-hook #'copilot-mode)
  (add-hook 'text-mode-hook #'copilot-mode)

  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion-by-line)
        ("M-<tab>" . copilot-accept-completion)
        ("M-k" . copilot-previous-completion)
        ("M-l" . copilot-next-completion)
        ("M-;" . copilot-accept-completion-by-word)))

(provide 'init-llm)
