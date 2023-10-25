;;; flycheck-codecheck.el --- Flycheck integration for CodeChecker      -*- lexical-binding: t; -*-

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

;; Provides flycheck support for "xp codecheck"

;;; Code:

(use-package flycheck)

(defun ebed:get-codecheck-target()
  "Gets the xp target for the given source file. Basically, this just replaces
the file's .cpp/.c/.h suffix with .cck."
  (if (or (not buffer-file-name)
          (not (string-match "\\.cpp$\\|\\.h$\\|\\.c$" buffer-file-name)))
      nil    ; invalid source file (should not happen)
     ;else: current buffer holds a cpp, h or c file
    (replace-regexp-in-string "\\.cpp$\\|\\.h$\\|\\.c$" ".cck"
                              (file-name-nondirectory buffer-file-name))
     ))

(defun ebed:codecheck-sentinel (process event flycheck-callback
                                        file-buffer
                                        process-buffer)
  "Called when xp $file.cck has completed. Output is in process-buffer."

  ;; We go through codecheck's output buffer and collect all
  ;; CHECKER_VIOLATIONS.
  ;; We add each checker violation to error-list. Finally, we call
  ;; flycheck-callback with the error-list.

  (let ((error-list '())
        (search-regex
         (concat
          "CHECKER_VIOLATION:.*"
          (file-name-nondirectory (buffer-file-name file-buffer))
          ":\\([0-9]+\\): \\(.*\\)"))
        (pclint-regex
         (concat
          "PCLINT_VIOLATION:.*"
          (file-name-nondirectory (buffer-file-name file-buffer))
          ":\\([0-9]+\\) \\((\\([0-9]+\\)) \\[\\([^]]+\\)\\] \\(.*\\)\\)"))
        (num-error 0)
        (num-recommendation 0)
        (num-suppress 0))
    (with-current-buffer process-buffer
      (beginning-of-buffer)
      (while (re-search-forward search-regex nil t)
        ;; found another CHECKER_VIOLATION line
        (let ((line (string-to-number (match-string 1)))
              (descr (match-string 2)) ; codecheck's error message
              (errorlevel 'error))
          (when (string-match "Recommendation: " descr)
            (setq errorlevel 'warning))
          (when (string-match "\\[Suppressed\\]" descr)
            (setq errorlevel nil)
            (setq num-suppress (+ num-suppress 1)))
          (when (string-match "\(\)\s+\\*\\*\\*" descr)
            (setq errorlevel nil))
          (when (eq errorlevel 'error) (setq num-error (+ num-error 1)))
          (when (eq errorlevel 'warning) (setq num-recommendation
                                               (+ num-recommendation 1)))

          (when errorlevel
            ;; push a new flycheck error to the end of the error list
            (push (flycheck-error-new-at line nil errorlevel descr
                                         :buffer file-buffer)
                  error-list))))
      (beginning-of-buffer)
      (while (re-search-forward pclint-regex nil t)
        ;; found another PCLINT_VIOLATION line
        (let* ((line (string-to-number (match-string 1)))
               (msg (match-string 2))
               (rule (string-to-number (match-string 3)))
               (type (match-string 4))
               (descr (match-string 5))
               (errorlevel 'error))
          (if (or (string= type "note") (string= type "info"))
              (progn
                (setq errorlevel 'warning)
                (setq num-recommendation (+ num-recommendation 1)))
            (progn
              (setq errorlevel 'error)
              (setq num-error (+ num-error 1))
              ))

          (when errorlevel
            ;; push a new flycheck error to the end of the error list
            (push (flycheck-error-new-at line nil errorlevel msg
                                         :buffer file-buffer)
                                         error-list))

      )))

    ;; because we always pushed to the front of error list, the list is in
    ;; reverse order. We reverse again.
    (setq error-list (nreverse error-list))

    (setq-local codecheck-last-errors error-list)  ; remember for later

    (message (format (concat "codecheck finished: %d errors, "
                             "%d recommendations, %d suppresses")
                     num-error num-recommendation num-suppress))

    ;; tell flycheck about the errors we found
    (funcall flycheck-callback 'finished error-list)))

(defun ebed:codecheck-start (checker callback)
  ;; This defun is called on file load, file save and each time the buffer is
  ;; edited.
  (if (and (buffer-modified-p) (boundp 'codecheck-last-errors))
      ;; the buffer has been modified so it does not make sense to re-run
      ;; codecheck on the file.
      ;; If we have a previous list of errors, use the old one.
      (funcall callback 'finished codecheck-last-errors)
    (progn
      (let ((codecheck-buffer (get-buffer-create "*codecheck*"))
            (file-buffer (current-buffer))

            ;; Set the directory to run xp $file.cck from:
            ;; - if ebed:flycheck-codecheck-dir is set, use this one.
            ;; - if ebed:compile-dir is set, use this one.
            ;; - if we can find a "_make" dir, use its parent directory
            ;; - otherwise, use the current default-directory (probably the
            ;;   file's dir --> xp $file.cck will not work in this case!
            (xp-dir
               (if (boundp 'ebed:flycheck-codecheck-dir)
                   ebed:flycheck-codecheck-dir
                 (if (boundp 'ebed:compile-dir)
                     ebed:compile-dir
                   (if (locate-dominating-file buffer-file-name "_make")
                       (locate-dominating-file buffer-file-name "_make")
                     default-directory))))

            (xp-command (ebed:get-codecheck-target)))

        ;; Delete old output: https://emacs.stackexchange.com/a/17517/7004
          (with-current-buffer codecheck-buffer
            (setf (buffer-string) "")
            (insert (concat "Running xp " xp-command " in "
                            xp-dir "...\n\n")))


        (let ((default-directory xp-dir))

          ;; Start xp $file.cck asynchronously.
          ;; Output will be collected in the *codecheck* buffer.
          ;; When the process is finished, ebed:codecheck-sentinel will be
          ;; called.
          (make-process
           :name "codecheck"
           :buffer codecheck-buffer
           :command (list "xp" xp-command)
           :sentinel (lambda (process event)
                       (ebed:codecheck-sentinel
                        process event callback
                        file-buffer codecheck-buffer)))
          (message "codecheck started"))))))

(flycheck-define-generic-checker 'codecheck
  "flycheck checker for c/c++ using `xp $filename.cck` to build errors.

Only runs if the file is not modified and ebed:flycheck-codecheck-enable is
    non-nil.

Asynchronously runs 'xp codecheck $filename.cck' in the directory
    ebed:compile-dir (should be set to the appropriate _make subdir) and parses
    the output."
  :start 'ebed:codecheck-start
  :predicate (lambda () (and (boundp 'ebed:flycheck-codecheck-enable)
                           ebed:flycheck-codecheck-enable))
  :modes '(c-mode c++-mode))

(flycheck-add-next-checker 'c/c++-clang '(t . codecheck))
(flycheck-add-next-checker 'lsp '(t . codecheck))

(nconc flycheck-checkers (list 'codecheck))

(defun ebed:flycheck-codecheck-enable-in-this-buffer ()
  (interactive)
  (setq-local ebed:flycheck-codecheck-enable t))

(defun ebed:flycheck-codecheck-insert-rule ()
  (interactive)
  (let (rule-number rule-descr)
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\(\\([0-9]+\\.\\)*[0-9]+[a-z]?\\)")
    (setq rule-number (match-string 1))
    (with-current-buffer "*codecheck*"
      (beginning-of-buffer)
      (re-search-forward
       (concat "(" rule-number ") +\[Suppressed]? *\\([^\n]+\\)"))
      (setq rule-descr (match-string 1))))
  (insert rule-descr)))

(provide 'flycheck-codecheck)
;;; flycheck-codecheck.el ends here
