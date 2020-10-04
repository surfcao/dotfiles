;;; init-org-ref.el --- Set up Org-Ref
(use-package helm-bibtex
  :after org-ref
  :ensure t
  :bind ("C-c h h" . helm-bibtex)
  :config
  (setq bibtex-completion-bibliography 
        '("~/Dropbox/bib/library.bib"))
  (setq bibtex-completion-library-path 
        '("~/Dropbox/bib/pdfs/"))
  ;; using bibtex path reference to pdf file
  (setq bibtex-completion-pdf-field "file") 
  (setq bibtex-completion-pdf-field "file") 
  (setq bibtex-completion-cite-default-command "citep")
  (setq bibtex-completion-notes-path "~/Dropbox/org/notes/readings/")
  (setq bibtex-completion-notes-template-multiple-files 
   (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(org-ref-get-mendeley-filename \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journal}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
   )))

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

(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
(setq bibtex-completion-cite-default-command "citep"))

(use-package org-ref
  :after org
  :init
  (setq reftex-default-bibliography '("~/Dropbox/bib/library.bib"))
  (setq org-ref-bibliography-notes "~/Dropbox/bib/notes.org"
        org-ref-default-bibliography '("~/Dropbox/bib/library.bib")
        org-ref-pdf-directory "~/Dropbox/bib/pdfs/")

  (setq helm-bibtex-bibliography "~/Dropbox/bib/library.bib")
  (setq helm-bibtex-library-path "~/Dropbox/bib/pdfs/")
  ;(setq helm-bibtex-notes-path "~/Dropbox/bib/notes.org")
  ;(setq bibtex-completion-notes-path "~/Dropbox/bib/notes.org")

  (setq org-ref-default-citation-link "citep")
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

  ;; Tell org-ref to let helm-bibtex find notes for it
  (setq org-ref-notes-function
      (lambda (thekey)
	(let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
	  (bibtex-completion-edit-notes
	   (list (car (org-ref-get-bibtex-key-and-file thekey)))))))

 ;; consistent note format between helm-bibtex and org-ref
  (defun my/org-ref-notes-function (candidates)
  (let ((key (helm-marked-candidates)))
    (funcall org-ref-notes-function (car key))))

  (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
  ;; Note that 7 is a magic number of the index where you want to insert the   command. You may need to change yours.
  (helm-add-action-to-source "Edit notes" 'my/org-ref-notes-function helm-source-bibtex 9)

  ; open pdf with pdf-tools
;  (defun my/org-ref-open-pdf-at-point ()
;  "Open the pdf for bibtex key under point if it exists."
;  (interactive)
;  (let* ((results (org-ref-get-bibtex-key-and-file))
;         (key (car results))
;         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
;    (if (file-exists-p pdf-file)
;        (find-file pdf-file)
;      (message "No PDF found for %s" key))))
; (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)
;
;  (setq bibtex-completion-pdf-symbol "⌘")
;  (setq bibtex-completion-notes-symbol "✎")
;  (define-key org-mode-map (kbd "C-c i") 'org-ref-insert-ref-link))


(defun org-export-latex-no-toc (depth)
    (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))
  (setq org-export-latex-format-toc-function 'org-export-latex-no-toc)

(setq org-latex-pdf-process
  '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

;; prefer use latex label
(setq org-latex-prefer-user-labels t)

(provide 'init-org-ref)
;;; init-org.el ends here
