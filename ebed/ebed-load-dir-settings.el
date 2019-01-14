;;; ebed-load-dir-settings.el --- Load dir settings recursively

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

;; Provides a better implementation of .dir-locals.el

;;; Code:

(defun recursive-load-dir-settings (currentfile)
  (let ((lds-dir (locate-dominating-file currentfile "settings.el")))
  (when lds-dir
      (progn
        (setq settings-list (cons (concat lds-dir "settings.el")
                                  settings-list))
        (recursive-load-dir-settings (file-truename(concat lds-dir "..")))))))

(defun load-dir-settings()
  (interactive)
  (let (settings-list)
  (when buffer-file-name
    (recursive-load-dir-settings buffer-file-name)
    (dolist (setfile settings-list) (load-file setfile)))))


(provide 'ebed-load-dir-settings)
;;; ebed-load-dir-settings.el ends here
