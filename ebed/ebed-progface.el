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

(use-package git-gutter+ :commands git-gutter+-mode)

;; Use monospaced font faces in current buffer
(defun ebed:progface ()
   "Sets fonts and other visual stuff for programming buffers"
   (interactive)
   (if (eq system-type 'windows-nt)
       (setq buffer-face-mode-face '(:family "Consolas" :height 160))
     (setq buffer-face-mode-face '(:family "Inconsolata" :height 180)))
   
   (buffer-face-mode)
   (display-fill-column-indicator-mode)
   (show-paren-mode)
   (auto-fill-mode)
   (idle-highlight-mode)
   (git-gutter+-mode)
   (highlight-regexp "[[:nonascii:]]" 'hi-pink)
   )

(provide 'ebed-progface)
;;; ebed-progface.el ends here
