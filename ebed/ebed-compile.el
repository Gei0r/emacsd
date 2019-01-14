;;; ebed-compile.el --- Compilation chain      -*- lexical-binding: t; -*-

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

;; Compiliation chain

;;; Code:

(defadvice compile (around split-horizontally activate)
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    ad-do-it))

(defun ebed:buffer-has-codecheck-violations(buffer)
  (let ((error-found nil))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward
              "CHECKER_VIOLATION:\\([a-zA-Z]:[^:]+\\):\\([0-9]+\\)" nil t)
        (when (not (search-forward "[Suppressed]" (line-end-position) t))
          (setq error-found t))))
    error-found))

(defun ebed:compile-has-error(buffer msg)
  (if (not (and buffer msg))
      nil
    (if (not (string= (string-trim msg) "finished"))
        t
      (with-current-buffer buffer
        (save-excursion
        (beginning-of-buffer)
        (if (or (search-forward "ABBRUCH DER PRODUKTION" nil t)
                (ebed:buffer-has-codecheck-violations buffer))
            t nil))))))

(defun ebed:compile-chain-last(buffer msg)
  (if (ebed:compile-has-error buffer msg)
      (with-current-buffer buffer
        (progn
          (select-window (get-buffer-window buffer))
          (switch-to-buffer buffer)
          (goto-char (point-min))
          (compilation-next-error 1)))
      (delete-window (get-buffer-window buffer)))
  (setq compilation-finish-functions nil))

(defun ebed:compile-chain-element (inbuffer msg)
  (let (next-command new-function)

    ;; only continue if no error and chain has more elements
    (if (and (not (ebed:compile-has-error inbuffer msg)) (> (length ebed:compile-chain) 0))
        (progn
        ;; pop next command from list
        (setq next-command (pop ebed:compile-chain))

        (if (> (length ebed:compile-chain) 0)
          (setq compilation-finish-functions 'ebed:compile-chain-element)
          (setq compilation-finish-functions 'ebed:compile-chain-last))

        ;; call compile with saved command
        (compile next-command))

      (progn
        (setq compilation-finish-functions nil)
        (beginning-of-buffer)))))

(defun ebed:compile(commands &optional dir)
  (interactive "scommand:
sdir: ")
  (when (stringp commands) (setq commands (list commands)))
  (setq ebed:compile-chain commands)
  (let ((default-directory (if dir (expand-file-name dir) default-directory)))
    (ebed:compile-chain-element nil nil)))

(defun ebed:compile-binding(num)
  (load-dir-settings)
  (if (boundp 'ebed:compile-commands)
      (ebed:compile
       (nth num ebed:compile-commands)
       (if (boundp 'ebed:compile-dir) ebed:compile-dir nil))
    (message "ebed:compile-commands not set!")))

(provide 'ebed-compile)
;;; ebed-compile.el ends here
