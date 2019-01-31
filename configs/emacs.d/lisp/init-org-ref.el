;;; init-org-ref.el --- Set up Org-Ref

(use-package org-ref
  :ensure t
  :config
  (setq org-ref-default-bibliography '("~/Dropbox/bib/library.bib")
        org-ref-pdf-directory "~/Dropbox/bib/pdfs/"
        org-ref-bibliography-notes "~/Dropbox/bib/notes.org")

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

(provide 'init-org-ref)
;;; init-org.el ends here
