;;; set up llm and copilot 
(use-package gptel
:config
;(setq gptel-api-key (auth-get-passwd :host "github.com" :max 1))

(gptel-make-gemini "Gemini" 
		   :key (auth-get-passwd :host "Gemini" :max 1) 
		   :stream t)

(gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "localhost:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '("mistral:latest" "deepseek-r1:8b" "deepseek-r1:32b"))          ;List of models

;; OPTIONAL configuration
(setq 
  gptel-model 'gpt-4o
  gptel-backend (gptel-make-openai "Github Models" ;Any name you want
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions"
    :stream t
    :key (auth-get-passwd :host "github.com" :max 1)
    :models '("gpt-4o" "o3-mini")))
)


(use-package copilot
  :ensure t 
  :custom
  (copilot-max-char -1)
  (copilot-indent-offset-warning-disable t)

  :config
  (unless (copilot-installed-version)
    (copilot-install-server))

; (add-hook 'prog-mode-hook 'copilot-mode)
; (add-hook 'text-mode-hook 'copilot-mode)

 (global-copilot-mode 1)

  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion-by-line)
        ("M-<tab>" . copilot-accept-completion)
        ("M-k" . copilot-previous-completion)
        ("M-l" . copilot-next-completion)
        ("M-;" . copilot-accept-completion-by-word)))

(provide 'init-llm)
