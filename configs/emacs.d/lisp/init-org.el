;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

;; Helper functions
(defun air-org-bulk-copy-headlines (&optional strip-tags)
  "Copy the headline text of the marked headlines in an agenda view.

This function is designed to be called interactively from an agenda
view with marked items.

If STRIP-TAGS is not nil, remove tags and trailing spaces from
the headlines."
  (interactive "P")
  (unless org-agenda-bulk-marked-entries (user-error "No entries are marked"))
  (let ((entries "")
        entry)
    (dolist (entry-marker (reverse org-agenda-bulk-marked-entries))
      (with-current-buffer (marker-buffer entry-marker)
        (save-excursion
          (goto-char (marker-position entry-marker))
          (when (re-search-forward org-heading-regexp (line-end-position) t)
            (setq entry (match-string-no-properties 2))
            (if strip-tags
                (setq entry (replace-regexp-in-string
                             (rx (0+ " ")
                                 (0+ (any alpha ":"))
                                 line-end)
                             "" entry)))
            (setq entries (concat entries entry "\n"))))))
    (if (length entries)
        (kill-new entries)))
  (message "Acted on %s entries%s"
           (length org-agenda-bulk-marked-entries)
           (if org-agenda-persistent-marks
               " (kept marked)" ""))
  (unless org-agenda-persistent-marks
    (org-agenda-bulk-unmark-all)))

(defun air-org-agenda-next-header ()
  "Jump to the next header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header))

(defun air-org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header t))

(defun air--org-agenda-goto-header (&optional backwards)
  "Find the next agenda series header forwards or BACKWARDS."
  (let ((pos (save-excursion
               (goto-char (if backwards
                              (line-beginning-position)
                            (line-end-position)))
               (let* ((find-func (if backwards
                                     'previous-single-property-change
                                   'next-single-property-change))
                      (end-func (if backwards
                                    'max
                                  'min))
                      (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                         (funcall find-func (point) 'org-agenda-date-header)))
                      (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                      (prop-pos (if all-pos (apply end-func all-pos) nil)))
                 prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))

(defun air--org-display-tag (tag &optional focus)
  "Display entries tagged with TAG in a fit window.

Do not make the new window current unless FOCUS is set."
  (org-tags-view nil tag)
  (fit-window-to-buffer)
  (unless focus
    (other-window 1)))

(defun air-org-display-directs (&optional focus)
  "Display entries tagged with `direct'.

Do not make the new window current unless FOCUS is set."
  (interactive "P")
  (air--org-display-tag "direct" focus))

(defun air-org-display-managers (&optional focus)
  "Display entries tagged with `manager'.

Do not make the new window current unless FOCUS is set."
  (interactive "P")
  (air--org-display-tag "manager" focus))

(defun air-org-skip-if-not-closed-today (&optional subtree)
  "Skip entries that were not closed today.

Skip the current entry unless SUBTREE is not nil, in which case skip
the entire subtree."
  (let ((end (if subtree (subtree-end (save-excursion (org-end-of-subtree t)))
               (save-excursion (progn (outline-next-heading) (1- (point))))))
        (today-prefix (format-time-string "%Y-%m-%d")))
    (if (save-excursion
          (and (re-search-forward org-closed-time-regexp end t)
               (string= (substring (match-string-no-properties 1) 0 10) today-prefix)))
        nil
      end)))

(defun air-org-skip-if-habit (&optional subtree)
  "Skip an agenda entry if it has a STYLE property equal to \"habit\".

Skip the current entry unless SUBTREE is not nil, in which case skip
the entire subtree."
  (let ((end (if subtree (subtree-end (save-excursion (org-end-of-subtree t)))
                (save-excursion (progn (outline-next-heading) (1- (point)))))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        end
      nil)))

(defun air-org-skip-if-priority (priority &optional subtree)
  "Skip an agenda item if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C.

Skips the current entry unless SUBTREE is not nil."
  (let ((end (if subtree (subtree-end (save-excursion (org-end-of-subtree t)))
                (save-excursion (progn (outline-next-heading) (1- (point))))))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        end
      nil)))

(defun air--org-global-custom-ids ()
  "Find custom ID fields in all org agenda files."
  (let ((files (org-agenda-files))
        file
        air-all-org-custom-ids)
    (while (setq file (pop files))
      (with-current-buffer (org-get-agenda-file-buffer file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (re-search-forward "^[ \t]*:CUSTOM_ID:[ \t]+\\(\\S-+\\)[ \t]*$"
                                      nil t)
              (add-to-list 'air-all-org-custom-ids
                           `(,(match-string-no-properties 1)
                             ,(concat file ":" (number-to-string (line-number-at-pos))))))))))
    air-all-org-custom-ids))

(defun air-org-goto-custom-id ()
  "Go to the location of CUSTOM-ID, or prompt interactively."
  (interactive)
  (let* ((all-custom-ids (air--org-global-custom-ids))
         (custom-id (completing-read
                     "Custom ID: "
                     all-custom-ids)))
    (when custom-id
      (let* ((val (cadr (assoc custom-id all-custom-ids)))
             (id-parts (split-string val ":"))
             (file (car id-parts))
             (line (string-to-number (cadr id-parts))))
        (pop-to-buffer (org-get-agenda-file-buffer file))
        (goto-char (point-min))
        (forward-line line)
        (org-reveal)
        (org-up-element)))))

(defun air-org-insert-custom-id-link ()
  "Insert an Org link to a custom ID selected interactively."
  (interactive)
  (let* ((all-custom-ids (air--org-global-custom-ids))
         (custom-id (completing-read
                     "Custom ID: "
                     all-custom-ids)))
    (when custom-id
      (let* ((val (cadr (assoc custom-id all-custom-ids)))
             (id-parts (split-string val ":"))
             (file (car id-parts))
             (line (string-to-number (cadr id-parts))))
        (org-insert-link nil (concat file "::#" custom-id) custom-id)))))

(defun air-org-nmom-capture-template ()
  "Return a Nine Minutes on Monday weekly agenda template suitable for capture."
  (let* ((deadline-timestamp (format-time-string "<%Y-%m-%d %a>"
                                                 (air-calendar-next-day-of-week 5)))
         (deadline (format "DEADLINE: %s\n\n" deadline-timestamp)))
    (concat (format "* Week %02d\n\n" (org-days-to-iso-week (org-today)))
            (concat "** TODO Care: \n" deadline
                    "** TODO Mastery: \n" deadline
                    "** TODO Recognition: \n" deadline
                    "** TODO Purpose: \n" deadline))))

(defun air-org-set-category-property (value)
  "Set the category property of the current item to VALUE."
  (interactive (list (org-read-property-value "CATEGORY")))
  (org-set-property "CATEGORY" value))

(defun air-org-insert-heading (&optional subheading)
  "Insert a heading or a subheading.

If the optional SUBHEADING is t, insert a subheading.  Inserting
headings always respects content."
  (interactive "P")
  (if subheading
      (org-insert-subheading t)
    (org-insert-heading t)))

(defun air-org-insert-scheduled-heading (&optional subheading)
  "Insert a new org heading scheduled for today.

Insert the new heading at the end of the current subtree if
FORCE-HEADING is non-nil."
  (interactive "P")
  (if subheading
      (org-insert-subheading t)
    (org-insert-todo-heading t t))
  (org-schedule nil (format-time-string "%Y-%m-%d")))

(defun air-org-task-capture (&optional vanilla)
  "Capture a task with my default template.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (org-capture nil "t")))

(defun air-org-agenda-capture (&optional vanilla)
  "Capture a task in agenda mode, using the date at point.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (let ((org-overriding-default-time (org-get-cursor-date)))
      (org-capture nil "t"))))

(defun air-org-agenda-toggle-date (current-line)
  "Toggle `SCHEDULED' and `DEADLINE' tag in the capture buffer."
  (interactive "P")
  (save-excursion
    (let ((search-limit (if current-line
                            (line-end-position)
                          (point-max))))

      (if current-line (beginning-of-line)
        (beginning-of-buffer))
      (if (search-forward "DEADLINE:" search-limit t)
          (replace-match "SCHEDULED:")
        (and (search-forward "SCHEDULED:" search-limit t)
             (replace-match "DEADLINE:"))))))

(defun air-pop-to-org-todo-life (split)
  "Visit my main TODO list, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file org-default-notes-file-life split))

(defun air-pop-to-org-todo-work (split)
  "Visit my main TODO list, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file org-default-notes-file-work split))

(defun air-pop-to-org-today (split)
  "Visit my main TODO list, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file (expand-file-name (format-time-string "W%W-%Y.org") org-journal-dir) )
split)

(defun air-pop-to-org-notes (split)
  "Visit my main notes file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/Dropbox/org/notes/notes.org" split))

(defun air-pop-to-org-ideas (split)
  "Visit my main notes file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/Dropbox/org/notes/ideas.org" split))

(defun air-pop-to-org-meetings (split)
  "Visit my main exertion file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/Dropbox/org/notes/meetings.org" split))

(defun air-pop-to-org-vault (split)
  "Visit my encrypted vault file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/Dropbox/org/notes/vault.gpg" split))

(defun air-pop-to-org-agenda (split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda nil "d")
  (when (not split)
    (delete-other-windows)))

(defun air--org-insert-list-leader-or-self (char)
  "If on column 0, insert space-padded CHAR; otherwise insert CHAR.

This has the effect of automatically creating a properly indented list
leader; like hyphen, asterisk, or plus sign; without having to use
list-specific key maps."
  (if (= (current-column) 0)
      (insert (concat " " char " "))
    (insert char)))

(defun air--org-swap-tags (tags)
  "Replace any tags on the current headline with TAGS.

The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
  (let ((old-tags (org-get-tags-string))
        (tags (if tags
                  (concat " " tags)
                "")))
    (save-excursion
      (beginning-of-line)
      (re-search-forward
       (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
       (line-end-position) t)
      (replace-match tags)
      (org-set-tags t))))

(defun air-org-goto-first-child ()
  "Goto the first child, even if it is invisible.

Return t when a child was found.  Otherwise don't move point and
return nil."
  (interactive)
  (let ((pos (point))
        (re (concat "^" outline-regexp))
        level)
    (when (condition-case nil (org-back-to-heading t) (error nil))
      (setq level (outline-level))
      (forward-char 1)
      (if (and (re-search-forward re nil t) (> (outline-level) level))
          (progn (goto-char (match-beginning 0)) t)
        (goto-char pos) nil))))

(defun air--org-select-tag ()
  "Interactively select or enter a single tag."
  (let ((org-last-tags-completion-table
         (if (derived-mode-p 'org-mode)
             (org-uniquify
              (delq nil (append (org-get-buffer-tags)
                                (org-global-tags-completion-table))))
           (org-global-tags-completion-table))))
    (completing-read
     "Tag: " 'org-tags-completion-function nil nil nil
     'org-tags-history)))

(defun air-org-agenda-filter-tag-interactive ()
  "Filter the agenda view by a tag selected interactively."
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      (let* ((tag (air--org-select-tag))
             (org-tag-alist-for-agenda (list (cons tag ?z))))
        (if tag
            (org-agenda-filter-by-tag nil ?z)))
    (error "Tag filtering only works in an agenda view")))

(defun air-org-set-tags (tag)
  "Add TAG if it is not in the list of tags, remove it otherwise.

TAG is chosen interactively from the global tags completion table."
(interactive (list (air--org-select-tag)))
  (let* ((cur-list (delq "" (org-get-tags)))
         (new-tags (mapconcat 'identity
                              (if (member tag cur-list)
                                  (delete tag cur-list)
                                (append cur-list (list tag)))
                              ":"))
         (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
                nil)))
    (org-set-tags new)))


;;; Code:
(use-package org
  :ensure t
  :defer t
  :commands (org-capture)
  :bind (("C-c c" .   org-capture)
         ("C-c l" .   org-store-link)
         ("C-c t" .   org-todo)
         ("C-c f k" . org-search-view)
         ("C-c f t" . org-tags-view)
         ("C-c f i" . air-org-goto-custom-id))
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-modules
        '(ol-bbdb ol-bibtex ol-docview org-habit ol-info ol-w3m))
  (setq org-todo-keywords
        '((sequence "☛ TODO" "⚑ WAITING" "SOMEDAY" "|" "✓ DONE(!)" "✗ CANCELED") (sequence "IDEA")))
  (setq org-blank-before-new-entry '((heading . auto)
                                     (plain-list-item . auto)))

  (setq org-capture-templates
        '(("t" "My TODO task format." entry
           (file org-default-notes-file-work)
           "* ☛ TODO %?\n :PROPERTIES:\n :CREATED:  %u\n :END:\n\n %i"
	    :empty-lines 1)

          ("n" "A (work-related) note." entry
           (file "notes/notes.org")
           "* %?\n :PROPERTIES:\n :CREATED: %u\n :END:\n\n"
	   :empty-lines 1) 

          ("m" "A meeting note." entry
           (file "notes/meetings.org")
	   "* Meeting with %?\n :PROPERTIES:\n :CREATED: %u\n :END:\n\n"
	   :empty-lines 1)

          ("i" "An idea entry." entry
           (file "notes/ideas.org")
           "* IDEA %?\n :PROPERTIES:\n :ORDERED: t\n :CREATED: %u\n :END:\n\n"
	   :empty-lines 1)

	  ("k" "Cliplink capture task" entry (file "inbox.org")
      "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)))

  ;; use this as default for emacs, and org-default-notes-file for random
  ;; notes 
  (setq org-default-notes-file-life "~/Dropbox/org/todo-life.org")
  (setq org-default-notes-file-work "~/Dropbox/org/todo-work.org")
  (setq org-directory "~/Dropbox/org/")
  ;(setq org-journal-dir "~/Dropbox/org/journal/2023")
  (setq org-journal-dir (concat org-directory "journal/" (format-time-string "%Y")))

;;The special marker ‘,’ inside of the argument to backquote indicates
;;a value that isn’t constant. The Emacs Lisp evaluator evaluates the
;;argument of ‘,’, and puts the value in the list structure:

;;  (add-to-list 'org-capture-templates
;;           `("j" "A journal entry" entry (file+olp+datetree ,(expand-file-name (format-time-string "W%W-%Y.org") org-journal-dir))
;;                           "* %<%H:%M:%S> %? \n" :tree-type week))

  (add-to-list 'org-capture-templates
           '("j" "A journal entry" entry (file+olp+datetree (lambda () (expand-file-name (format-time-string "W%W-%Y.org") org-journal-dir))) "* %<%H:%M:%S> %? \n" :tree-type week))

;;(add-to-list  'org-capture-templates 
;;	      '("y" "Journal entry" entry (file+olp+datetree (lambda() (org-journal-find-location)))
;;                               "* %<%H:%M:%S> %? \n" :tree-type week))
;;
  ;; Logging of state changes
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  (setq org-log-into-drawer t)

  (setq org-pretty-entities t)
  (setq org-insert-heading-respect-content t)
  (setq org-ellipsis "...")
  (setq org-startup-with-inline-images nil)
  (setq org-export-initial-scope 'subtree)
  (setq org-use-tag-inheritance nil) ;; Use the list form, which happens to be blank
  (setq org-todo-keyword-faces
        '(("OPEN" . org-done)
	("PAUSED" . org-upcoming-deadline)))

  ;; Agenda configuration
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-agenda-files '("~/Dropbox/org/"))
  ;(setq org-agenda-files (directory-files-recursively "~/Dropbox/org/notes/" "\\.org$"))
  (setq my-note-files (directory-files-recursively "~/Dropbox/org/notes/" "\\.org$"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2) (my-note-files :maxlevel . 2)))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-skip-scheduled-if-done t)

  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-ndays 1)))
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-if-habit)
                                                     (air-org-skip-if-priority ?A)
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:")))

            (todo "✓ DONE"
                     ((org-agenda-skip-function 'air-org-skip-if-not-closed-today)
                      (org-agenda-overriding-header "Closed today:"))
                     )
            )
           ((org-agenda-compact-blocks t)))))

  (set-face-attribute 'org-upcoming-deadline nil :foreground "gold1")

(defun air--org-element-motion (count)
  "Return the bounds of an element; traverse upward COUNT levels."
  (save-excursion
    ;; get to the top of the tree
    (org-with-limited-levels
      (cond ((org-at-heading-p) (beginning-of-line))
	    ((org-before-first-heading-p) (user-error "Not in a subtree"))
	    (t (outline-previous-visible-heading 1))))

    (decf count)
    (when count (while (and (> count 0) (org-up-heading-safe)) (decf count)))

    ;; extract the beginning and end of the tree
    (let* ((element (org-element-at-point))
	   (begin (org-element-property :begin element))
	   (end (org-element-property :end element)))
      (list end begin))))

(evil-define-text-object evil-org-outer-element (count &optional beg end type)
			 "One whole org element, from headline to final newline."
			 :type line
			 (air--org-element-motion count))

(evil-define-text-object evil-org-inner-element (count &optional beg end type)
			 "An Org subtree, minus its header and concluding line break.  Uses code from `org-mark-subtree`"
			 :type line
			 (let* ((outer-points (air--org-element-motion count))
				(outer-begin (cadr outer-points))
				(outer-end (car outer-points))
				(begin (save-excursion
					 (goto-char outer-begin)
					 (next-line)
					 (while (and (< (point) outer-end)
						     (string-match-p "^\\s-*$"
								     (buffer-substring (line-beginning-position)
										       (line-end-position))))
						(forward-line 1))
					 (point)))
				(end (save-excursion
				       (goto-char outer-end)
				       (backward-char 1)
				       (while (and (> (point) outer-begin)
						   (string-match-p "^\\s-*$"
								   (buffer-substring (line-beginning-position)
										     (line-end-position))))
					      (forward-line -1))
				       (goto-char (line-end-position))
				       (point))))
			   (list end begin)))

(define-key evil-outer-text-objects-map "*" 'evil-org-outer-element)
(define-key evil-inner-text-objects-map "*" 'evil-org-inner-element)
  (evil-leader/set-key-for-mode 'org-mode
    "$"  'org-archive-subtree
    "a"  'org-agenda
    "c"  'air-org-set-category-property
    "D"  'org-deadline
    "ns" 'org-narrow-to-subtree
    "p"  'org-set-property
    "s"  'org-schedule
    "t"  'air-org-set-tags)

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq org-habit-graph-column 50)
              (define-key org-agenda-mode-map "H"          'beginning-of-buffer)
              (define-key org-agenda-mode-map "j"          'org-agenda-next-item)
              (define-key org-agenda-mode-map "k"          'org-agenda-previous-item)
              (define-key org-agenda-mode-map "J"          'air-org-agenda-next-header)
              (define-key org-agenda-mode-map "K"          'air-org-agenda-previous-header)
              (define-key org-agenda-mode-map "n"          'org-agenda-next-date-line)
              (define-key org-agenda-mode-map "p"          'org-agenda-previous-date-line)
              (define-key org-agenda-mode-map "c"          'air-org-agenda-capture)
              (define-key org-agenda-mode-map "R"          'org-revert-all-org-buffers)
              (define-key org-agenda-mode-map "y"          'air-org-bulk-copy-headlines)
              (define-key org-agenda-mode-map "/"          'counsel-grep-or-swiper)
              (define-key org-agenda-mode-map (kbd "RET")  'org-agenda-switch-to)

              (define-prefix-command 'air-org-run-shortcuts)
              (define-key air-org-run-shortcuts "f" (tiny-menu-run-item "org-files"))
              (define-key air-org-run-shortcuts "t" (tiny-menu-run-item "org-things"))
              (define-key air-org-run-shortcuts "c" (tiny-menu-run-item "org-captures"))
              (define-key air-org-run-shortcuts "l" (tiny-menu-run-item "org-links"))
              (define-key org-agenda-mode-map (kbd "\\") air-org-run-shortcuts)))

  (add-hook 'org-capture-mode-hook
            (lambda ()
              (evil-define-key '(normal insert) org-capture-mode-map (kbd "C-d") 'air-org-agenda-toggle-date)
              (evil-define-key 'normal org-capture-mode-map "+" 'org-priority-up)
              (evil-define-key 'normal org-capture-mode-map "-" 'org-priority-down)
              (evil-define-key '(normal insert) org-capture-mode-map (kbd "C-=" ) 'org-priority-up)
              (evil-define-key '(normal insert) org-capture-mode-map (kbd "C--" ) 'org-priority-down)
              ;; TODO this seems like a hack
              (evil-insert-state)))

  (add-hook 'org-mode-hook
            (lambda ()
	      ; enable yas, guofeng
	      (yas-minor-mode t)
	      ;; Special plain list leader inserts
	      (dolist (char '("+" "-"))
		(define-key org-mode-map (kbd char)
			    `(lambda ()
			       (interactive)
			       (air--org-insert-list-leader-or-self ,char))))

	      ;; Normal maps
	      (define-key org-mode-map (kbd "C-c d")   (lambda ()
							 (interactive) (air-org-agenda-toggle-date t)))
	      (define-key org-mode-map (kbd "C-c ,")   'org-time-stamp-inactive)
	      (define-key org-mode-map (kbd "C-|")     'air-org-insert-scheduled-heading)
	      ;; unset the keybindings 'C-\\' in global-map
	      (local-unset-key (kbd "C-\\"))
	      (define-key org-mode-map (kbd "C-\\")    'air-org-insert-heading)
	      (define-key org-mode-map (kbd "C-c C-\\")    (lambda()
							     (interactive) (air-org-insert-heading t)))
	      (define-key org-mode-map (kbd "s-r")     'org-revert-all-org-buffers)
	      (define-key org-mode-map (kbd "C-c C-l") (tiny-menu-run-item "org-links"))

	      (define-key org-mode-map (kbd "C-<")                'org-shiftmetaleft)
	      (define-key org-mode-map (kbd "C->")                'org-shiftmetaright)

	      ;; These are set as evil keys because they conflict with
	      ;; existing commands I don't use, or are superseded by
	      ;; some evil function that org-mode-map is shadowed by.
	      (defun company-complete-or-org-cycle ()
		"Call `company-complete' then `org-cycle'."
		(interactive)
		(or (and (looking-back "\\w" (line-beginning-position))
			 (company-complete))
		    (org-cycle)))
	      (evil-define-key 'normal org-mode-map (kbd "<tab>")   'org-cycle)
	      (evil-define-key 'insert org-mode-map (kbd "<tab>") 'company-complete-or-org-cycle)

	      (evil-define-key 'normal org-mode-map (kbd "C-,")   'org-metaleft)
	      (evil-define-key 'normal org-mode-map (kbd "C-.")   'org-metaright)

	      (evil-define-key 'normal org-mode-map (kbd "C-S-l") 'org-shiftright)
	      (evil-define-key 'normal org-mode-map (kbd "C-S-h") 'org-shiftleft)

	      (evil-define-key 'insert org-mode-map (kbd "C-S-l") 'org-shiftright)
	      (evil-define-key 'insert org-mode-map (kbd "C-S-h") 'org-shiftleft)

	      ;; Navigation
	      (define-key org-mode-map (kbd "M-h") 'org-up-element)
	      (define-key org-mode-map (kbd "M-j") 'org-forward-heading-same-level)
	      (define-key org-mode-map (kbd "M-k") 'org-backward-heading-same-level)
	      (define-key org-mode-map (kbd "M-l") 'air-org-goto-first-child)
	      ;; "gh" goes up a level, and is defined by org-evil-mode.
	      ;; "gH" goes to the top level, and is defined by org-evil-mode.
	      (evil-define-key 'normal org-mode-map (kbd "gl") 'air-org-goto-first-child)
	      ;; Use fill column, but not in agenda
	      (setq fill-column 75)
	      (when (not (eq major-mode 'org-agenda-mode))
		(visual-line-mode)
		(visual-fill-column-mode))
	      (flyspell-mode)
	      (org-evil-mode)
	      (org-indent-mode))))

(use-package org-evil
  :ensure t
  :config  
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "T" 'org-set-tags-command)
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "t" 'org-todo)
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "@" 'org-refile)
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "#" 'org-add-note)
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "+" 'org-shiftup)
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "=" 'org-shiftdown))

(use-package org-bullets
  :ensure t
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("○" "◉" "◎" "☉" )))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)
   (matlab . t)
   (lisp . t)))

(use-package org-download
   :ensure t
   :config
   (add-hook 'dired-mode-hook 'org-download-enable)
   ;(org-download-method 'directory)
   ;(org-download-heading-lvl nil)
   ;(org-image-actual-width 300)
   (setq-default org-download-image-dir "./images")
   ;(org-download-screenshot-method "/usr/local/bin/pngpaste %s")
   :bind
   ("C-M-y" . org-download-clipboard))

(use-package org-cliplink
     :ensure t
    :config
   (global-set-key (kbd "C-x p i") 'org-cliplink))


;;(use-package org-journal
;;  :ensure t
;;  :init
;;  ;; Change default prefix key; needs to be set before loading org-journal
;;  (setq org-journal-prefix-key "C-c j")
;;  :config
;;  (setq org-journal-file-type 'weekly)
;;  (setq org-journal-dir "~/Dropbox/org/journal/2023")
;;  (setq org-journal-file-format "W%W-%Y.org")
;;  (setq org-journal-date-format "%Y-%m-%d")
;;  (setq org-journal-skip-carryover-drawers (list "LOGBOOK")))
;;
;;(defun org-journal-find-location ()
;;    ;; Open today's journal, but specify a non-nil prefix argument in order to
;;    ;; inhibit inserting the heading; org-capture will insert the heading.
;;    (org-journal-new-entry t)
;;    (unless (eq org-journal-file-type 'daily)
;;      (org-narrow-to-subtree))
;;    (goto-char (point-max)))

(provide 'init-org)
;;; init-org.el ends here
