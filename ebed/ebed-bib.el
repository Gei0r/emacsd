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

(defface ebed:helmBib_ID
  '((t
     :foreground "#4fc24f"))
  "Face for the Bibliography ID in helm"
  )

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

(defun ebed:getBibentry (&optional id)
  (let ((entry (if id id (ebed:getBibIdAtPoint))))
    (ebed:call_getbibentryJs entry "--info")))

(defun ebed:copyBibentry (&optional id)
  (interactive)
  (let ((result (ebed:getBibentry id)))
    (message result)
    (kill-new result)))

(defun ebed:getSapid (&optional id)
  (interactive)
  (let* ((entry (if id id (ebed:getBibIdAtPoint)))
         (result (ebed:call_getbibentryJs entry "--sapid")))
    (message result)
    (kill-new result)))

(defun ebed:getSaplink (&optional id)
  (interactive)
  (let* ((entry (if id id (ebed:getBibIdAtPoint)))
         (result (ebed:call_getbibentryJs entry "--saplink")))
    (message result)
    (kill-new result)
    (browse-url result)))

(defun ebed:gotoBibentry (&optional id)
  (interactive)
  (let ((entry (if id id (ebed:getBibIdAtPoint)))
        result splits)
    (setq result (ebed:call_getbibentryJs entry "--pos"))
    (message result)
    (setq splits (split-string result "@" t "[:space:]"))
    (find-file-other-window (nth 0 splits))
    (goto-char (string-to-number (nth 1 splits)))))

(defun ebed:helmBibCandidates ()
  (interactive)
  (let* (
         (json-object-type 'alist)
         (json-array-type 'list)
         (json (json-read-from-string (ebed:call_getbibentryJs "" "--all")))
         (props '((id ebed:helmBib_ID) (type) (title) (subtitle) (sapid) (scn) (geprueft))))
    (mapcar (lambda (entry)
              ;; transform json object to (DISP . REAL) pair
              (let (disp real)
                (setq real (cdr (assoc 'id entry)))

                (mapcar (lambda (prop_face)
                          (let ((prop (nth 0 prop_face))
                                (face (nth 1 prop_face))
                                tmp)
                            (setq tmp (cdr (assoc prop entry)))
                            (when tmp
                              (when face
                                (put-text-property 0 (length tmp) 'face face tmp))
                              (setq disp (concat disp tmp "\n"))))) props)

                `(,(string-trim disp) . ,real)
                )) json)))

(defun ebed:helmBib ()
  (interactive)

  (helm
   :sources (helm-build-sync-source "Literaturdatenbank"
              :candidates 'ebed:helmBibCandidates
              :multiline t
              :action (helm-make-actions
                       "Insert ID at point"
                       (lambda (id) (insert id))
                       "Goto definition"
                       'ebed:gotoBibentry
                       "Add ID to kill ring"
                       (lambda (id) (kill-new id))
                       "Add Entry to kill ring"
                       'ebed:copyBibentry
                       "Insert at point"
                       (lambda (id) (insert (ebed:getBibentry id)))
                       "Copy SAP-ID"
                       'ebed:getSapid
                       "Get SAP link"
                       'ebed:getSaplink
                       ))
   :buffer "*helm sync source*"))

(provide 'ebed-bib)
;;; ebed-bib.el ends here
