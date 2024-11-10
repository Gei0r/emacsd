;;; ebed-hackbrett-config.el --- Scriptable Hackbrett configuration for clangd   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adrian Ebeling

;; Author: Adrian Ebeling <adrian.ebeling@siemens.com>
;; Keywords: lisp
;; Version: 0.0.2

;;; Commentary:

;; This allows providing Hackbrett configuration (project root,
;; .clangd file) not through fixed values, but by calling an external
;; program.

;;; Code:

(defcustom ebed:hackbrett-config-program
  (if (eq system-type 'windows-nt)
      (concat (file-name-directory (or load-file-name buffer-file-name))
              "hackbrettConfig.bat")
    (concat (file-name-directory (or load-file-name buffer-file-name))
            "hackbrettConfig.sh"))
  "Tool to call to get clangd configuration.

Must support two modes:
- Mode 1: Input:  Source file name (e.g. *.cpp, *.h)
          Output (stdout): project root directory or empty
          Example: <program> --get-root <source file name>
- Mode 2: Input: Source file name (e.g. *.cpp, *.h)
          Output (file): .clangd
          Example: <program> <project-root>")

(defcustom ebed:hackbrett-clangd-enable t
  "Whether to enable hackbrett-clangd or not")

(defun ebed:find-hackbrett-project-root(session)
  "Find hackbrett project root by calling an external program

Program is set in ``ebed:hackbrett-config-program'', which see.
"
  (let (program-output (openedFile (buffer-file-name)))
    (with-temp-buffer
      (call-process ebed:hackbrett-config-program
                    nil t nil "--get-root" openedFile)
      (setq program-output (s-trim (buffer-string))))
    
    (if (= (length program-output) 0)

        ;; external program did not provide root, fall back to lsp builtin
        ;; capabilities.
        nil

      ;; We did get a project root for lsp. Return it.
      program-output)))

(defun ebed:create-hackbrett-clangd()
  "Create Hackbrett-specific .clangd file by calling external program.

Program is set in ``ebed:hackbrett-config-program'', which see."
  (interactive)
  (call-process ebed:hackbrett-config-program nil nil nil (buffer-file-name))
  )

(defun ebed:hackbrett-config-init()
  "Initialize lsp mode and set up the external command for hackbrett-config

Meant to be used as a hook on c-mode and c++-mode."

  (when ebed:hackbrett-clangd-enable
    (advice-add 'lsp--find-root-interactively :before-until
                'ebed:find-hackbrett-project-root)
    (ebed:create-hackbrett-clangd))
  (lsp))


(provide 'ebed-hackbrett-config)
;;; ebed-hackbrett-config.el ends here
