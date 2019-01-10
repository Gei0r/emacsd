;;; ebed-helm-buffers-persistent-kill.el --- Killing buffers in helm

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

;; From helm-mini, kill the selected frames.

;;; Code:

(use-package helm)

(defun ebed:helm-buffers-persistent-kill ()
  (interactive)
  (let ((marked (helm-marked-candidates)))
    (unwind-protect
         (cl-loop for b in marked
               do (progn (helm-preselect
                          (format "^%s"
                                  (helm-buffers--quote-truncated-buffer b)))
                          (helm-buffers-persistent-kill-1 b)
                          (message nil)))
      (with-helm-buffer
        (setq helm-marked-candidates nil
              helm-visible-mark-overlays nil))
      (helm-force-update (helm-buffers--quote-truncated-buffer
                          (helm-get-selection))))))


(provide 'ebed-helm-buffers-persistent-kill)
;;; ebed-helm-buffers-persistent-kill.el ends here
