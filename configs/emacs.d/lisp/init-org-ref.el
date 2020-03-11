;;; init-org-ref.el --- Set up Org-Ref

(use-package helm-bibtex
  :ensure t
  :bind ("C-c h h" . helm-bibtex)
  :config
  (setq bibtex-completion-bibliography 
        '("~/Dropbox/bib/library.bib"))
  (setq bibtex-completion-library-path 
        '("~/Dropbox/bib/pdfs/"))
  ;; using bibtex path reference to pdf file
  (setq bibtex-completion-pdf-field "file") )

(use-package ivy-bibtex
  :ensure t
  :bind ("C-c b b" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography 
        '("~/Dropbox/bib/library.bib"))
  (setq bibtex-completion-library-path 
        '("~/Dropbox/bib/pdfs/"))

  ;; using bibtex path reference to pdf file
  (setq bibtex-completion-pdf-field "file")

  ;;open pdf with external viwefoxit
;  (setq helm-bibtex-pdf-open-function
;        (lambda (fpath)
;          (start-process "open" "*open*" "open" fpath)))

(ivy-set-actions
  'ivy-bibtex
  '(("p" ivy-bibtex-open-any "Open PDF, URL, or DOI")
    ("s" bibtex-completion-show-entry "Show entry")
    ("e" ivy-bibtex-edit-notes "Edit notes")))

(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))


(use-package org-ref
  :after org
  :init
  (setq reftex-default-bibliography '("~/Dropbox/bib/library.bib"))
  (setq org-ref-bibliography-notes "~/Dropbox/bib/notes.org"
        org-ref-default-bibliography '("~/Dropbox/bib/library.bib")
        org-ref-pdf-directory "~/Dropbox/bib/pdfs")

  (setq helm-bibtex-bibliography "~/Dropbox/bib/library.bib")
  (setq helm-bibtex-library-path "~/Dropbox/bib/pdfs")
  ;(setq helm-bibtex-notes-path "~/Dropbox/bib/notes.org")
  (setq bibtex-completion-notes-path "~/Dropbox/bib/notes.org")

  :config
  (key-chord-define-global "uu" 'org-ref-cite-hydra/body)
  ;; variables that control bibtex key format for auto-generation
  ;; I want firstauthor-year-title-words
  ;; this usually makes a legitimate filename to store pdfs under.
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5)

  (setq bibtex-completion-pdf-field "file")
  (setq org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename)

  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎"))

;; PDF links for org-mode
(with-eval-after-load "pdf-tools"
  (use-package org-pdfview
    :config
    ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-11/msg00169.html
    ;; Before adding, remove it (to avoid clogging)
    (delete '("\\.pdf\\'" . default) org-file-apps)
    ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-11/msg00176.html
    (add-to-list 'org-file-apps
		 '("\\.pdf\\'" . (lambda (file link)
				   (org-pdfview-open link))))))

(provide 'init-org-ref)
;;; init-org.el ends here
