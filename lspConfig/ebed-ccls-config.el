;;; ebed-ccls-config.el --- Scriptable ccls configuration      -*- lexical-binding: t; -*-

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

;; This allows providing ccls configuration (project root,
;; compile_commands.json) not through fixed values, but by calling an external
;; program.

;;; Code:

(defcustom ebed:ccls-config-program
  (if (eq system-type 'windows-nt)
      (concat (file-name-directory (or load-file-name buffer-file-name))
              "cclsConfig.bat")
    (concat (file-name-directory (or load-file-name buffer-file-name))
            "cclsConfig.sh"))
  "Tool to call to get ccls configuration.

Must support two modes:
- Mode 1: Input:  Source file name (e.g. *.cpp, *.h)
          Output (stdout): project root directory or empty
          Example: <program> --get-root <source file name>
- Mode 2: Input: Project root dir
          Output (stdout): compile_commands.json
          Example: <program> <project-root>")

(defun ebed:find-ccls-project-root(session)
  "Find ccls project root by calling an external program

Program is set in ``ebed:ccls-config-program'', which see.
"
  (let (program-output (openedFile (buffer-file-name)))
    (with-temp-buffer
      (call-process ebed:ccls-config-program
                    nil t nil "--get-root" openedFile)
      (setq program-output (s-trim (buffer-string))))
    
    (if (= (length program-output) 0)

        ;; external program did not provide root, fall back to ccls builtin
        ;; capabilities.
        nil

      ;; We did get a project root for ccls. Return it.
      program-output)))

(defun ebed:ccls-config-init()
  "Initialize lsp mode and set up the external command for ccls

Meant to be used as a hook on c-mode and c++-mode."

  (advice-add 'lsp--find-root-interactively :before-until
              'ebed:find-ccls-project-root)
  (require 'ccls)
  (setq ccls-initialization-options
        (plist-put ccls-initialization-options
                   :compilationDatabaseCommand ebed:ccls-config-program))
  (lsp))


(provide 'ebed-ccls-config)
;;; ebed-ccls-config.el ends here
