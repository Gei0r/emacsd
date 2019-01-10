;;; ebed-clearcase.el --- helper functions for ClearCase

;; Copyright (C) 2019 Adrian Ebeling

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

;; Miscellaneous helper function for IBM Rational ClearCase.

;;; Code:

(defun ebed:cc-checkout-file-in-buffer()
  (interactive)
  (shell-command
   (concat "cleartool checkout -unreserved -ncomment " buffer-file-name))
  (shell-command (concat "chmod 0777 " buffer-file-name))
  (revert-buffer nil t)
  (read-only-mode 0))

(defun ebed:cc-hijack-file-in-buffer()
  (interactive)
  (shell-command (concat "chmod 0777 " buffer-file-name))
  (revert-buffer nil t)
  (read-only-mode 0))


(provide 'ebed-clearcase)
;;; ebed-clearcase.el ends here
