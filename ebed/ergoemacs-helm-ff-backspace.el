;;; ergoemacs-helm-ff-backspace.el --- Ido-style backspace in helm      -*- lexical-binding: t; -*-

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

;; In helm-read-file: Use backspace to delete the whole directory.
;; Bind to DEL in helm-find-files-map and helm-read-file-map.
;;
;; This comes from https://github.com/emacs-helm/helm/pull/327, but
;; was reverted so it is added back here.

;;; Code:

(defun ergoemacs-helm-ff-backspace ()
  "Call backsapce or `helm-find-files-up-one-level'.
If sitting at the end of a file directory, backspace goes up one
level, like in `ido-find-file'. "
  (interactive)
  (let (backspace)
    (looking-back "^.*")
    (cond
     ((looking-back "[/\\]")
      (helm-find-files-up-one-level 1))
     (t
      (setq backspace (lookup-key
                       (current-global-map)
                       (read-kbd-macro "DEL")))
      (call-interactively backspace)))))

(provide 'ergoemacs-helm-ff-backspace)
;;; ergoemacs-helm-ff-backspace.el ends here
