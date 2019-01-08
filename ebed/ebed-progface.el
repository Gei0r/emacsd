;;; ebed-progface --- Face for programming modes                   -*- lexical-binding: t; -*-

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

;; Sets some basic programming faces for a programming buffer.

;;; Code:

(use-package idle-highlight-mode)

(use-package column-marker :load-path "ebed"
  :commands column-marker-1)

;; Use monospaced font faces in current buffer
(defun ebed:progface ()
   "Sets fonts and other visual stuff for programming buffers"
   (interactive)
   (setq buffer-face-mode-face '(:family "Consolas" :height 140))
   (buffer-face-mode)
   (column-marker-1 79)
   (show-paren-mode)
   (auto-fill-mode)
   (idle-highlight-mode))

(provide 'ebed-progface)
;;; ebed-progface.el ends here
