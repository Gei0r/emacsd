;;; ebed-bib.el --- Bibliography functions      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adrian Ebeling

;; Author: Adrian Ebeling <devl@adrian-ebeling.de>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for Siemens bibliography

;;; Code:

(defcustom ebed:bibDatabase "" "Verzeichnis der Literaturdatenbank"
  :type 'directory)

(defun ebed:call_getbibentryJs (entry what)
  (interactive)
  (let (builder)
    (with-temp-buffer
      (call-process "node" nil t nil
                    (concat (file-name-directory
                             (symbol-file 'ebed:call_getbibentryJs))
                            "/getbibentry.js")
                    what
                    ebed:bibDatabase (encode-coding-string entry 'latin-1))
      (buffer-string))))

(defun ebed:getBibIdAtPoint ()
  (let (result)
  (save-excursion
    (re-search-backward "[[:space:]{,]")
    (re-search-forward "[^[:space:],{}]+")
    (setq result (match-string 0))
    (set-text-properties 0 (length result) nil result)
    result)))

(defun ebed:getBibentryAtPoint ()
  (interactive)
  (let (entry)
    (setq entry (thing-at-point 'line t))
    (string-match "{\\([^,]+\\)," entry)
    (setq entry (match-string 1 entry))
    (setq entry (ebed:call_getbibentryJs entry "--info"))
    (message entry)
    (kill-new entry)))

(defun ebed:gotoBibentryAtPoint ()
  (interactive)
  (let ((entry (ebed:getBibIdAtPoint)) result splits)
    (setq result (ebed:call_getbibentryJs entry "--pos"))
    (message result)
    (setq splits (split-string result "@" t "[:space:]"))
    (find-file-other-window (nth 0 splits))
    (goto-char (string-to-number (nth 1 splits)))))

(defun ebed:call_getbibentryJs_async (entry what)
  (start-process "getbibentryJs" nil
                 "node"
                 (concat (file-name-directory
                          (symbol-file 'ebed:call_getbibentryJs))
                         "/getbibentry.js")
                    what
                    ebed:bibDatabase
                    (encode-coding-string entry 'latin-1)))

(defface ebed:helmBib_ID
  '((t
     :foreground "green"))
  "Face for the Bibliography ID in helm"
  )

(defun ebed:helmBibCandidates ()
  (let (a)
    (setq Value "A\nAffe")
    (put-text-property 0 1 'face 'ebed:helmBib_ID Value)
    `((,Value . a) ("B" . b))))

(defun ebed:helmBib ()
  (interactive)

  (helm
   :sources (helm-build-sync-source "test"
              :candidates 'ebed:helmBibCandidates
              :multiline t)
        :buffer "*helm sync source*"))

(provide 'ebed-bib)
;;; ebed-bib.el ends here
