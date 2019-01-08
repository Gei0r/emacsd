;;; ebed-eshell-here --- Eshell at current dir                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adrian Ebeling

;; Author: Adrian Ebeling
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

;; Opens eshell at the current default-directory.

;;; Code:

(use-package eshell)

(defun ebed:eshell-here()
  (interactive)
  (let ((dest-dir default-directory))
    (eshell)
    (eshell/cd dest-dir)
    (insert "")
    (eshell-send-input)
    ))

(provide 'ebed-eshell-here)
;;; ebed-eshell-here.el ends here