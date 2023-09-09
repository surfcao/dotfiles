;; init-platform --- Platform-specific settings
;;; Commentary:

;;; Code:
(require 'init-fonts)

;; This must run after window setup or it seems to have no effect.
(add-hook 'window-setup-hook
;(add-hook 'emacs-setup-hook
          (lambda ()
            (when (memq window-system '(mac ns x))
	      (use-package exec-path-from-shell
			   :ensure t
			   :config
			   (setq exec-path-from-shell-variables '("PATH" "BIBINPUTS" "BSTINPUTS" "TEXINPUTS"))
			   (exec-path-from-shell-initialize))

	      ;(add-to-list 'default-frame-alist '(font . "Inconsolata LGC"))
	      ;(set-face-attribute 'default nil :font "Inconsolata LGC")
	      (add-to-list 'default-frame-alist '(font . "iA Writer Mono V"))
	      (set-face-attribute 'default nil :font "iA Writer Mono V")
	      (sanityinc/set-frame-font-size 22)
	      (define-key global-map (kbd "S-<return>") 'toggle-frame-fullscreen))

	    (when (memq window-system '(x))
	      (add-to-list 'default-frame-alist '(font . "Inconsolata LGC"))
	      (set-face-attribute 'default nil :font "Inconsolata LGC")
	      (sanityinc/set-frame-font-size 26))

            (when (fboundp 'powerline-reset)
              (powerline-reset))))

;; Display emoji on Macs where the font is already there.
(when (memq window-system '(mac))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; Use option as meta
(when (memq window-system '(mac))
(setq mac-option-key-is-meta t
      mac-command-key-is-meta nil
      mac-command-modifier 'super
      mac-function-modifier 'hyper
      mac-option-modifier 'meta))

(provide 'init-platform)
;;; init-platform.el ends here
