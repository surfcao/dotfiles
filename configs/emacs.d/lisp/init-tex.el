;(use-package company-auctex
;	     :ensure t
;	     :config
;	     (company-auctex-init))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
 ; set master for multiple files
 ;(setq-default TeX-master "main")
 (setq-default TeX-master nil)
 (add-hook 'LaTeX-mode-hook
            (lambda ()
	      (rainbow-delimiters-mode)
	      (yas-minor-mode t)
	      (set-fill-column 80)
	      (turn-on-auto-fill)
	      (company-mode)
	      ;(company-auctex)

              (writeroom-mode)
	      ; hide the header line to increase top margin
	      (setq header-line-format " ")
	      ;(set-face-attribute 'header-line nil :background "white")
	      (set-face-attribute 'header-line nil :background (face-attribute 'default :background))
	      (setq-default line-spacing 5)

	      (flyspell-mode)
	      (smartparens-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
	      (reftex-isearch-minor-mode)
	      (setq TeX-PDF-mode t)
	      (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)))

;(add-hook 'LaTeX-mode-hook (lambda ()
;  (push
;    '("latexmk" "latexmk -outdir=./ -pdflatex='pdflatex -verbose -file-line-error -synctex=1' -pdf %s" TeX-run-TeX nil t
;      :help "Run latexmk on file")
;    TeX-command-list))) 
;
;(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;(require 'tex-buf)
(defun run-latexmk ()
  (interactive)
  (let ((TeX-save-query nil)
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk"
         ;(TeX-command-expand "latexmk -outdir=./output -pdflatex='pdflatex -file-line-error -synctex=1' -pdf %s" 'TeX-master-file)
         (TeX-command-expand "latexmk -outdir=./ -pdflatex='pdflatex -file-line-error -synctex=1' -pdf %s" 'TeX-master-file)
         master-file)
    (if (plist-get TeX-error-report-switches (intern master-file))
        (TeX-next-error t)
      (minibuffer-message "latexmk done"))))
(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "C-0") #'run-latexmk)))


;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
           #'TeX-revert-document-buffer)

; to use pdfview with auctex
;(add-hook 'LaTeX-mode-hook 'pdf-tools-install)

; to use pdfview with auctex
(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
       TeX-source-correlate-start-server t)
(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-default-bibliography '("~/Dropbox/bib/library.bib"))
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite


;(use-package org-ref
;  :after org
;  :init
;  (setq reftex-default-bibliography '("~/Dropbox/bib/library.bib"))
;  (setq org-ref-bibliography-notes "~/Dropbox/bib/notes.org"
;        org-ref-default-bibliography '("~/Dropbox/bib/library.bib")
;        org-ref-pdf-directory "~/Dropbox/bib/pdfs")
;
;  (setq helm-bibtex-bibliography "~/Dropbox/bib/library.bib")
;  (setq helm-bibtex-library-path "~/Dropbox/bib/pdfs")
;
;  (setq helm-bibtex-pdf-open-function
;        (lambda (fpath)
;          (start-process "open" "*open*" "open" fpath)))
;
;  (setq helm-bibtex-notes-path "~/Dropbox/bib/notes.org")
;  :config
;  (key-chord-define-global "uu" 'org-ref-cite-hydra/body)
;  ;; variables that control bibtex key format for auto-generation
;  ;; I want firstauthor-year-title-words
;  ;; this usually makes a legitimate filename to store pdfs under.
;  (setq bibtex-autokey-year-length 4
;        bibtex-autokey-name-year-separator "-"
;        bibtex-autokey-year-title-separator "-"
;        bibtex-autokey-titleword-separator "-"
;        bibtex-autokey-titlewords 2
;        bibtex-autokey-titlewords-stretch 1
;        bibtex-autokey-titleword-length 5))

(provide 'init-tex)
