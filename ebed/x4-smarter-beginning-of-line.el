;;; x4-smarter-beginning-of-line.el --- Smart movement to <home>

;; Copyright (C) 2012 x4lldux

;; Author: x4lldux <x4lldux@vectron.io>
;; Keywords: lisp
;; Version: 0.0.1

;; (No license)

;;; Commentary:

;; Move point to beginning-of-line or first non-whitespace character or first
;; non-whitespace after a comment sign.
;;
;; From https://github.com/X4lldux/emacs-prelude-personal/blob/bc13fe99414542fa9e32c41c328083a7b23756ce/x4-funs.el

;;; Code:

;; Smart beginning of the line
(defun x4-smarter-beginning-of-line ()
  "Move point to beginning-of-line or first non-whitespace character or first
   non-whitespace after a comment sign."
  (interactive "^")
  (let (
        (oldpos (point))
        (indentpos (progn
                     (back-to-indentation)
                     (point)
                     )
                   )
        (textpos (progn
                   (beginning-of-line-text)
                   (point)
                   )
                 )
        )
    (cond
     ((> oldpos textpos) (beginning-of-line-text))
     ((and (<= oldpos textpos) (> oldpos indentpos))  (back-to-indentation))
     ((and (<= oldpos indentpos) (> oldpos (line-beginning-position))) (beginning-of-line))
     (t (beginning-of-line-text))
     )
    )
  )


(provide 'x4-smarter-beginning-of-line)
;;; x4-smarter-beginning-of-line.el ends here
