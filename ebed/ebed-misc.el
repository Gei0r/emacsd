;;; ebed-misc.el --- miscellaneous functions     -*- lexical-binding: t; -*-

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

;; Some small convenience functions to help with emacs.

;;; Code:

(defun ebed:newline-with-semicolon ()
  (interactive)
  (move-end-of-line nil)
  (when (not (looking-back ";"))
    (insert ";"))
  (funcall indent-line-function))

(defun ebed:insert-path()
  (interactive)
  (let ((to-paste (current-kill 0 t)))
    (setq to-paste (replace-regexp-in-string "\\\\" "/" to-paste))
    (insert to-paste)))

(defun ebed:printHash(hash)
  (maphash (lambda (key value) (message (format "%s -> %s" key value))) hash))

(defun ebed:remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(provide 'ebed-misc)
;;; ebed-misc.el ends here
