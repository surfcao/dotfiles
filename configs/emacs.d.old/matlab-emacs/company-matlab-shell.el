;;; company-matlab-shell.el --- a matlab-shell-mode completion back-end
;;
;; Copyright (C) 2009 David Engster
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ignore-errors
  (require 'company))
(eval-when-compile (require 'cl))
(require 'matlab)

;; the following code is mostly taken from matlab.el, (C) Eric M. Ludlam
(defun company-matlab-shell-tab (prefix)
  "Send [TAB] to the currently running matlab process and retrieve completion."
  (let ((orig-point (point))
        (end-with-period-p (looking-back "\\."))
        tempcmd
        completions)
    (re-search-backward " ")
    (unless (or end-with-period-p
                (not (company-matlab--keyword-at-same-line-p)))
      (re-search-backward " \\(cd\\|help\\) *"))
    (forward-char)
    (setq tempcmd (buffer-substring-no-properties (point) orig-point))
    ;; double double every single quote!!
    (setq tempcmd (replace-regexp-in-string "'" "''''" tempcmd))
    ;; collect the list
    (setq completions (matlab-shell-completion-list tempcmd))
    (goto-char orig-point)
    completions))

(defun company-matlab--at-last-line-p ()
  (let ((orig-line (line-number-at-pos)))
    (save-excursion
      (goto-char (point-max))
      (= orig-line (line-number-at-pos)))))

(defun company-matlab--keyword-at-same-line-p ()
  (save-excursion
    (when (looking-back " \\(cd\\|help\\) *")
      (re-search-backward " \\(cd\\|help\\) *")
      (company-matlab--at-last-line-p))))

(defun company-matlab-shell-grab-symbol ()
  (when (and
         (string= (buffer-name (current-buffer)) (concat "*" matlab-shell-buffer-name "*"))
         (company-matlab--at-last-line-p))
    (let ((orig-point (point)))
      (re-search-backward " ")
      (when (company-matlab--keyword-at-same-line-p)
        (re-search-backward " \\(cd\\|help\\) *"))
      (prog1
          (when (company-matlab--at-last-line-p)
            (forward-char)
            (let* ((lastcmd (buffer-substring-no-properties
                             (point) orig-point))
                   limitpos)
              (unless (string= lastcmd "")
                (setq limitpos
                      ;; Must add \\. here
                      (if (string-match ".*\\([(\\. /[,;=']\\)" lastcmd)
                          (1+ (match-beginning 1))
                        0))
                (substring-no-properties lastcmd limitpos))))
        (goto-char orig-point)))))


(defun company-matlab-shell-get-completions (arg)
  (when (string= (buffer-name (current-buffer)) (concat "*" matlab-shell-buffer-name "*"))
    (mapcar 'car (company-matlab-shell-tab arg))))

;;;###autoload
(defun company-matlab-shell (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for Matlab-Shell."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-matlab-shell))
    ('prefix (and (eq 'matlab-shell-mode major-mode) (or (company-matlab-shell-grab-symbol) 'stop)))
    ('candidates (company-matlab-shell-get-completions arg))
    ('sorted t)))

(provide 'company-matlab-shell)
;;; company-matlab-shell.el ends here
