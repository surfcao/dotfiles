;;; init-org-ref.el --- Set up notes systems

(setq
   org_notes "~/Dropbox/org/notes/"
   zot_bib "~/Dropbox/bib/library.bib"
   deft-directory org_notes
   org-roam-directory org_notes
   org-default-notes-file (concat org_directory "/inbox.org")
   )

(use-package  org-noter
	:after (:any org pdf-view)
	:config
	(setq
	  ; The WM can handle splits
	  ;org-noter-notes-window-location 'other-frame
	  ; Please stop opening frames
	  org-noter-always-create-frame nil
	  ; I want to see the whole file
	  org-noter-hide-other nil
	  ; Everything is relative to the main notes file
	  org-noter-notes-search-path (list (concat org_notes "/readings/"))))

(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions#'org-noter-pdftools-jump-to-note)))


;; org-roam
;(use-package org-roam
;  :hook (org-load . org-roam-mode)
;  :commands (org-roam-buffer-toggle-display
;             org-roam-find-file
;             org-roam-graph
;             org-roam-insert
;             org-roam-switch-to-buffer
;             org-roam-dailies-date
;             org-roam-dailies-today
;             org-roam-dailies-tomorrow
;             org-roam-dailies-yesterday)
;  :preface
;  ;; Set this to nil so we can later detect whether the user has set a custom
;  ;; directory for it, and default to `org-directory' if they haven't.
;  (defvar org-roam-directory nil)
;  :init
;  :config
;  (setq org-roam-directory (expand-file-name (or org-roam-directory "roam")
;                                             org-directory)
;        org-roam-verbose nil  ; https://youtu.be/fn4jIlFwuLU
;        org-roam-buffer-no-delete-other-windows t ; make org-roam buffer sticky
;        org-roam-completion-system 'default
;)
;
;  ;; Normally, the org-roam buffer doesn't open until you explicitly call
;  ;; `org-roam'. If `+org-roam-open-buffer-on-find-file' is non-nil, the
;  ;; org-roam buffer will be opened for you when you use `org-roam-find-file'
;  ;; (but not `find-file', to limit the scope of this behavior).
;  ;(add-hook 'find-file-hook
;  ;  (defun +org-roam-open-buffer-maybe-h ()
;  ;    (and +org-roam-open-buffer-on-find-file
;  ;         (memq 'org-roam-buffer--update-maybe post-command-hook)
;  ;         (not (window-parameter nil 'window-side)) ; don't proc for popups
;  ;         (not (eq 'visible (org-roam-buffer--visibility)))
;  ;         (with-current-buffer (window-buffer)
;  ;           (org-roam-buffer--get-create)))))
;
;  ;; Hide the mode line in the org-roam buffer, since it serves no purpose. This
;  ;; makes it easier to distinguish among other org buffers.
;  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode))


;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
;(use-package org-roam-protocol
;  :after org-protocol)
;
;
;(use-package company-org-roam
;  :after org-roam
;  :config
;  (push 'company-org-roam company-backends)
;  (push 'company-yasnippet company-backends)
;  (push 'company-dabbrev company-backends))
;
;
;(use-package org-roam-bibtex
;  :after (org-roam)
;  :hook (org-roam-mode . org-roam-bibtex-mode)
;  :config
;  (setq org-roam-bibtex-preformat-keywords
;   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
;  (setq orb-templates
;        '(("r" "ref" plain (function org-roam-capture--get-point)
;           ""
;           :file-name "${slug}"
;           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}
;
;- tags ::
;- keywords :: ${keywords}
;
;\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
;
;           :unnarrowed t))))



(provide 'init-notes)
;;; init-org.el ends here
