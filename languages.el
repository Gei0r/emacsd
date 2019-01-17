
(use-package org
  :bind
  ((:map org-mode-map (("C-j" . nil) ("C-k" . nil))))
  :config
  (setq org-blank-before-new-entry
        (quote ((heading . t) (plain-list-item))))
  (setq org-startup-folded 'content)
  (setq org-startup-indented t)
  (setq org-support-shift-select 'always))

(use-package web-mode
  :mode ("\\.js" "\\.jsx" "\\.xml" "\\.html")
  :config
    (setq web-mode-auto-close-style 2)
	(setq web-mode-markup-indent-offset 2))

(use-package cc-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  :config
    (setq c-default-style "bsd" c-basic-offset 4)
    (add-to-list 'c-offsets-alist '(case-label . 4)))

(use-package asm86-mode
  :quelpa (asm86-mode :fetcher github :repo "gei0r/asm86-mode")
  :mode "\\.asm"
  :bind
  ((:map asm86-mode-map
         (("RET" . (lambda ()
                     (interactive)
                     (if (looking-back ";;.*")
                         (progn (indent-new-comment-line)(insert " "))
                       (newline-and-indent)))))))
  :config
  (setq asm86-blank-base-offset 4)
  (setq asm86-code-comment-base-offset 4)
  (setq asm86-inline-comment-base-offset 4)
  (setq asm86-inst-base-offset 4)
  (setq asm86-blank-func-offset 4)
  (setq asm86-label-func-offset 4)
  (setq asm86-header-comment-func-offset 4)
  (setq asm86-code-comment-func-offset 4)
  (setq asm86-inline-comment-func-offset 4)
  (setq asm86-inst-func-offset 4)
  (setq asm86-mod-base-offset 4)
  (setq asm86-assume-base-offset 4)
  (setq asm86-extrn-base-offset 0)
  (setq asm86-scope-ident-base-offset 0)
  (setq asm86-end-func-offset 8)
  (setq asm86-tab-base-offset 4)
  (setq asm86-tab-func-offset 4)
  (setq asm86-variable-base-offset 4)
  (setq asm86-variable-func-offset 8))

(use-package typescript-mode
  :mode("\\.ts" "\\.tsx"))

(use-package tide
  :hook
  (typescript-mode .
    (lambda()
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      (company-mode +1)))
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  :bind
  ((:map tide-mode-map (("<f2>" . tide-jump-to-definition)
                        ("M-<left>" . tide-jump-back)))))

(use-package lsp-mode
  :commands lsp :config (setq lsp-prefer-flymake nil))
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(use-package bibtex
  :config
  (define-key bibtex-mode-map (kbd "C-j") nil))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp)))
  :config (setq ccls-executable "D:/Adrian/ccls/ccls/build/ccls.exe"))
