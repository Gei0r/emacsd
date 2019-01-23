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

(defun ebed:getBibEntry (entry)
  (interactive)
  (let (builder)
    (with-temp-buffer
      (call-process "node" nil t nil
                    (concat (file-name-directory
                             (symbol-file 'ebed:getBibEntry))
                            "/getbibentry.js")
                     ebed:bibDatabase (encode-coding-string entry 'latin-1))
      (buffer-string))))

(defun ebed:getBibentryAtPoint ()
  (interactive)
  (let (entry)
    (setq entry (thing-at-point 'line t))
    (string-match "{\\([^,]+\\)," entry)
    (setq entry (match-string 1 entry))
    (setq entry (ebed:getBibEntry entry))
    (message entry)
    (kill-new entry)))


(provide 'ebed-bib)
;;; ebed-bib.el ends here