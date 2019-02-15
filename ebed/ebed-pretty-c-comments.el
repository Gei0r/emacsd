;;; ebed-pretty-c-comments.el --- Pretty C comments

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

;; Automatically create neat c++ comment blocks:
;; https://emacs.stackexchange.com/q/14563/7004

;;; Code:

(require 'cc-mode)

(defun ebed:prettify-c-block-comment (orig-fun &rest args)
  (let* ((first-comment-line (looking-back "/\\*\\s-*.*"))
         (star-col-num (when first-comment-line
                         (save-excursion
                           (re-search-backward "/\\*")
                           (1+ (current-column))))))
    (apply orig-fun args)
    (when first-comment-line
      (save-excursion
        (newline)
        (dotimes (cnt star-col-num)
          (insert " "))
        (move-to-column star-col-num)
        (insert "*/"))))
  ;; Ensure one space between the asterisk and the comment
  (when (not (looking-back " "))
    (insert " ")))

(defun ebed:new-line-and-maybe-prettify-comment ()
  (interactive)
  (if (nth 4 (syntax-ppss))
      (c-indent-new-comment-line)
    (newline-and-indent)))

(defun ebed:activate-pretty-c-block-comments ()
  (interactive)
  (advice-add 'c-indent-new-comment-line
              :around #'ebed:prettify-c-block-comment))


(provide 'ebed-pretty-c-comments)
;;; ebed-pretty-c-comments.el ends here
