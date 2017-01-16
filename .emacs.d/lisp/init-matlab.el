;; Setting up matlab-mode
(add-to-list 'load-path "~/.emacs.d/matlab-emacs")
 (load-library "matlab-load")
(setq auto-mode-alist
    (cons
     '("\\.m$" . matlab-mode)
     auto-mode-alist))
(setq matlab-indent-function t)

(matlab-cedet-setup)

(when (memq window-system '(mac ns))
(setq matlab-shell-command "/Applications/MATLAB_R2014b.app/bin/matlab"))
(when (memq window-system '(x))
(setq matlab-shell-command "matlab"))
(setq matlab-shell-command-switches (list "-nodesktop" "-nosplash"))
(add-hook 'matlab-mode-hook 'auto-complete-mode)
(evil-leader/set-key-for-mode 'matlab-mode "l" 'matlab-shell-run-region-or-line)
(evil-leader/set-key-for-mode 'matlab-mode "i" 'matlab-shell-describe-variable)
(evil-leader/set-key-for-mode 'matlab-mode "h" 'matlab-shell-describe-command)
(evil-leader/set-key-for-mode 'matlab-mode "c" 'matlab-shell-run-cell)
(evil-leader/set-key-for-mode 'matlab-mode "I" 'matlab-shell-run-command)

;; for GDB/debugging in general
(global-set-key (kbd "<f10>") 'gud-cont)
(global-set-key (kbd "<f12>") 'gud-break)
(global-set-key (kbd "S-<f12>") 'gud-remove)
(global-set-key (kbd "<f9>") 'gud-step);; equiv matlab step in
(global-set-key (kbd "S-<f9>") 'gud-up);; Up stack frame
(global-set-key (kbd "<f8>") 'gud-next) ;; equiv matlab step 1 
(global-set-key (kbd "<f7>") 'gud-finish) ;; equiv matlab step out

(provide 'init-matlab)
