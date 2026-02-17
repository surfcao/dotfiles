;;; init-pdf.el -- My Emacs configuration
(use-package pdf-tools
 :defer t
 :ensure t
 :mode (("\\.pdf\\'" . pdf-view-mode))
 :bind ("C-c C-g" . pdf-sync-forward-search)
; :pin manual ;; manually update
 :config
 (custom-set-variables
    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  ;(setq pdf-info-epdfinfo-program "~/.emacs.d/elpa/pdf-tools-1.0/epdfinfo")
 ;; initialise
 (pdf-tools-install)
 ;; open pdfs scaled to fit page
 (setq pdf-view-display-size 'fit-page)
 ;; automatically annotate highlights
 (setq pdf-annot-activate-created-annotations t)
 ;; turn off cua so copy works
; (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
 ;; more fine-grained zooming
 (setq pdf-view-resize-factor 1.1)
 ;; keyboard shortcuts
 (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
 (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
 (define-key pdf-view-mode-map (kbd "J") 'pdf-view-next-page)
 (define-key pdf-view-mode-map (kbd "K") 'pdf-view-previous-page)
 (define-key pdf-view-mode-map (kbd "g") 'pdf-view-first-page)
 (define-key pdf-view-mode-map (kbd "G") 'pdf-view-last-page)
 (define-key pdf-view-mode-map (kbd "u") 'pdf-view-scroll-down-or-previous-page)
 (define-key pdf-view-mode-map (kbd "d") 'pdf-view-scroll-up-or-next-page)
 (define-key pdf-view-mode-map (kbd "C-b") 'pdf-view-scroll-down-or-previous-page)
 (define-key pdf-view-mode-map (kbd "C-f") 'pdf-view-scroll-up-or-next-page)
 (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
 (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
 (define-key pdf-view-mode-map (kbd "0") 'image-bol)
 (define-key pdf-view-mode-map (kbd "$") 'image-eol)
 (define-key pdf-view-mode-map (kbd "q") 'quit-window)
 (define-key pdf-view-mode-map (kbd "Q") 
	     (lambda ()
	       (kill-this-buffer)
	       (quit-window)))
 (define-key pdf-view-mode-map (kbd "'") 'pdf-view-jump-to-register)
 (define-key pdf-view-mode-map (kbd "m") 'pdf-view-position-to-register)
 (define-key pdf-view-mode-map (kbd "r") 'revert-buffer)
 (define-key pdf-view-mode-map (kbd "/") 'isearch-forward)
 (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
 (define-key pdf-view-mode-map (kbd "?") 'isearch-backward)
 (define-key pdf-view-mode-map (kbd "n") 'isearch-repeat-forward)
 (define-key pdf-view-mode-map (kbd "N") 'isearch-repeat-backward)
; (define-key pdf-view-mode-map (kbd "o") 'pdf-outline)
; (define-key pdf-view-mode-map (kbd "M-s o") 'pdf-occur)

 (define-key pdf-view-mode-map (kbd "H") 'pdf-annot-add-highlight-markup-annotation)
 (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
 (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
 ; enable buffer-local auto-revert and disable blink in PDFs
 (add-hook 'pdf-view-mode-hook 
	  (lambda ()
	   (auto-revert-mode 1)
	   (setq-local auto-revert-interval 2)
	   (blink-cursor-mode -1))))

; ;; PDF links for org-mode
; (with-eval-after-load 'pdf-tools
;   (use-package org-pdftools
;     :config
;     ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-11/msg00169.html
;     ;; Before adding, remove it (to avoid clogging)
;     (delete '("\\.pdf\\'" . default) org-file-apps)
;     ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-11/msg00176.html
;     (add-to-list 'org-file-apps
; 		 '("\\.pdf\\'" . (lambda (file link)
; 				   (org-pdftools-open link))))))

;(use-package pdf-tools
;  :ensure t
;  :config
;  (custom-set-variables
;    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
;  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
;(pdf-tools-install)

;(use-package org-noter
;    :after org
;    :ensure t
;    :config (setq org-noter-default-notes-file-names '("notes.org")
;                  org-noter-notes-search-path '("~/Dropbox/bib/notes")))

;(add-to-list 'load-path "~/Downloads/org-pdftools/")
;(use-package org-pdftools 
;	     :config (setq org-pdftools-root-dir '("~/Dropbox/bib/pdfs")))

(provide 'init-pdf)
