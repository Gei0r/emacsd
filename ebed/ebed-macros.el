;;; ebed-macros.el --- Keyboard macros      -*- lexical-binding: t; -*-

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

;; Own keyboard macros

;;; Code:

(defun ebed:setup-keyboard-macros()
  (interactive)
  (fset 'review-antwort
        (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([end return home 64 64 64 64 32 134217786 40 102 111 114 109 97 116 45 116 105 109 101 45 115 116 114 105 110 103 32 34 37 100 backspace 89 37 109 37 100 end S-home 40 105 110 115 101 114 116 32 return 32 101 98 101 100 58 32] 0 "%d")) arg)))
  (define-key global-map (kbd "C-ä") 'review-antwort))

(provide 'ebed-macros)
;;; ebed-macros.el ends here
