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


(use-package dash)

(defun ebed:find-other-file-from-list(l1 l2)
  "Internal function.

  L1: first list of extensions.
  L2: second list of extensions."
  (interactive)

  (let (ext file-name-no-ext (target-file nil))

    (setq ext
          (-first (lambda(x)(string-suffix-p x buffer-file-name)) l1))
    (when ext
      ;; currently opened file has suffix "ext" from l1

      (setq file-name-no-ext
            (substring
             (file-name-nondirectory buffer-file-name)
             0
             (- (length ext))))

      ;; try to find a corresponding file with suffix from l2
      (setq ext
            (-first
             (lambda(x)(file-exists-p (concat file-name-no-ext x)))
             l2))
      (setq target-file (concat file-name-no-ext ext)))

    ;; return value is target-file
    target-file
    ))

(defun ebed:find-other-file()
  "If current buffer is a .cpp file, open the corresponding .h file and vice
  versa"
  (interactive)

  (let ((impl-exts '(".cpp" ".c" ".cxx" "_asm.asm"))
        (hdr-exts  '(".h" ".hpp" ".hxx" "_h.asm"))
        tmp)

    (setq tmp (ebed:find-other-file-from-list impl-exts hdr-exts))
    (if tmp (find-file tmp)
      (setq tmp (ebed:find-other-file-from-list hdr-exts impl-exts))
      (if tmp (find-file tmp)
        (message "No other file found")
        nil))))

(defun ebed:insert-path()
  (interactive)
  (let ((to-paste (current-kill 0 t)))
    (setq to-paste (replace-regexp-in-string "\\\\" "/" to-paste))
    (insert to-paste)))

(defun ebed:printHash(hash)
  (maphash (lambda (key value) (message (format "%s -> %s" key value))) hash))

(provide 'ebed-misc)
;;; ebed-misc.el ends here
