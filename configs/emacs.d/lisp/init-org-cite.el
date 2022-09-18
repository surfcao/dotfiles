;;; init-org-cite.el --- Set up Org-Cite

; setup org-cite
(use-package citeproc
    :ensure t)
(require 'oc)
(require 'oc-basic)
(require 'oc-csl)
(require 'oc-natbib)
(require 'oc-biblatex)

(setq org-cite-global-bibliography
 '("~/GDrive/bib/library.bib"))
(setq org-cite-csl-styles-dir "~/Zotero/styles")
(setq org-cite-export-processors
 '((latex biblatex)
   (t csl "apa.csl")))

(use-package helm-bibtex
  :after (helm)
  :ensure t
  :bind ("C-c h h" . helm-bibtex)
  :config
  (setq bibtex-completion-bibliography 
        ;'("~/Dropbox/bib/library.bib"))
        '("~/GDrive/bib/library.bib"))
  (setq bibtex-completion-library-path 
        ;'("~/Dropbox/bib/pdfs/"))
        '("~/GDrive/bib/pdfs/"))
  ;; using bibtex path reference to pdf file
  (setq bibtex-completion-pdf-field "file") 
 ; (setq bibtex-completion-cite-default-command "citep")

  (setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-org-cite)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default)))

  (setq bibtex-completion-notes-path "~/GDrive/bib/notes.org"))

(use-package ivy-bibtex
  :ensure t
  :bind ("C-c b b" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography 
        ;'("~/Dropbox/bib/library.bib"))
        '("~/GDrive/bib/library.bib"))
  (setq bibtex-completion-library-path 
        ;'("~/Dropbox/bib/pdfs/"))
        '("~/GDrive/bib/pdfs/"))
  (setq bibtex-completion-notes-path "~/GDrive/bib/notes.org")

  ;; using bibtex path reference to pdf file
  (setq bibtex-completion-pdf-field "file")

  ;;open pdf with external viwefoxit
;  (setq helm-bibtex-pdf-open-function
;        (lambda (fpath)
;          (start-process "open" "*open*" "open" fpath)))

(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-org-cite)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default)))

;(setq bibtex-completion-cite-default-command "citep")
(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(provide 'init-org-cite)
