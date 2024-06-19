(require 'org-ref-ivy)
(require 'oc-csl)

(use-package org-ref
  :after org
  :config
  ;(setq reftex-default-bibliography '("~/Dropbox/bib/library.bib"))
  ;(setq org-ref-bibliography-notes "~/GDrive/bib/notes.org"
  ;      org-ref-default-bibliography '("~/GDrive/bib/library.bib")
  ;      org-ref-pdf-directory "~/GDrive/bib/pdfs/")
  (setq bibtex-completion-bibliography '("~/OneDrive/bib/library.bib"))
  ;(setq	bibtex-completion-notes-template-one-file "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n")
  (setq bibtex-completion-notes-template-one-file "\n* ${author-or-editor} (${year}): ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :END:\n\n  See [[cite:&${=key=}]]\n")
  (setq bibtex-completion-library-path '("~/OneDrive/bib/pdfs/"))
  (setq bibtex-completion-notes-path "~/OneDrive/bib/notes.org")
  ;(setq org-ref-default-citation-link "citep")
  ;(setq helm-source-bibtex org-ref-helm-source-bibtex)

  (setq bibtex-completion-pdf-field "file")

  ; for Mendeley
 ;(setq org-ref-get-pdf-filename-function 'org-ef-get-mendeley-filename)
(setq org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-bibtex-completion)

;; sync with org-cite
(setq org-ref-insert-cite-function
      (lambda ()
	(org-cite-insert nil)))

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

  ; comment out because
  ;(helm-delete-action-from-source "Edit notes" helm-source-bibtex)
  ;; Note that 7 is a magic number of the index where you want to insert the   command. You may need to change yours.
  ;(helm-add-action-to-source "Edit notes" 'my/org-ref-notes-function helm-source-bibtex 9)

; ; open pdf with pdf-tools
; (defun my/org-ref-open-pdf-at-point ()
; "Open the pdf for bibtex key under point if it exists."
; (interactive)
; (let* ((results (org-ref-get-bibtex-key-and-file))
;        (key (car results))
;        (pdf-file (funcall org-ref-get-pdf-filename-function key)))
;   (if (file-exists-p pdf-file)
;       (find-file pdf-file)
;     (message "No PDF found for key %s" key))))
;  (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")
(define-key org-mode-map (kbd "C-c i") 'org-ref-insert-ref-link))

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
