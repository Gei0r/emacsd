
(defun realign-org-tables()
  (interactive)
  (org-table-map-tables 'org-table-align))

(use-package org
  :mode (("\\.org$" . org-mode) ("\\.docbuilder$" . org-mode))
  :bind
  ((:map org-mode-map (
                       ("C-j" . nil) ("C-k" . nil)
                       ("C-a" . mark-whole-buffer)
                       ("C-e" . eshell)
                       ("C-c a" . realign-org-tables)
                       )))
  :config
  (setq org-blank-before-new-entry
        (quote ((heading . t) (plain-list-item))))
  (setq org-startup-folded 'content)
  (setq org-startup-indented t)
  (setq org-support-shift-select 'always)
  (auto-fill-mode 1)
  (column-number-mode 1))

(add-hook 'emacs-lisp-mode-hook 'company-mode)

(use-package web-mode
  :mode ("\\.jsx" "\\.xml" "\\.html" "\\.json" "\\.ms" "\\.mus")
  :config
    (setq web-mode-auto-close-style 2)
	(setq web-mode-markup-indent-offset 2)
    (setq web-mode-engines-alist
          '(("ctemplate"    . "\\.ms")
          ("ctemplate" . "\\.mus"))))

(use-package ebed-pretty-c-comments :load-path "ebed"
  :commands (ebed:activate-pretty-c-block-comments
             ebed:new-line-and-maybe-prettify-comment))

(use-package cc-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  :bind
  ((:map c-mode-base-map
         (("RET" . 'ebed:new-line-and-maybe-prettify-comment))))
  :config
    (setq c-default-style "bsd" c-basic-offset 4)
    (add-to-list 'c-offsets-alist '(case-label . 4))
    (ebed:activate-pretty-c-block-comments))

(use-package asm86-mode
  :quelpa (asm86-mode :fetcher github :repo "gei0r/asm86-mode")
  :mode "\\.asm"
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
  (setq asm86-variable-func-offset 8)
  (add-hook 'asm86-mode-hook
            '(lambda()
               (define-key asm86-mode-map (kbd "RET")
                 (lambda () (interactive)(if (looking-back ";;.*")
                                             (progn
                                               (indent-new-comment-line)
                                               (insert " "))
                                           (newline-and-indent)))))))

(use-package typescript-mode
  :mode("\\.ts" "\\.tsx" "\\.js$"))

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

(use-package bibtex
  :config
  (define-key bibtex-mode-map (kbd "C-j") nil))

(use-package lsp-mode
  :commands lsp
  :init
  (define-key global-map (kbd "C-d") nil)
  (define-key c++-mode-map (kbd "C-d") nil)
  (setq lsp-keymap-prefix "C-d l")
  :config
  (define-key lsp-mode-map (kbd "C-d l") lsp-command-map)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-file-watch-threshold 20000)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (when (fboundp idle-highlight-mode) (idle-highlight-mode -1))
  :bind
  ((:map lsp-mode-map (("<f2>" . xref-find-definitions)
                       ("M-<left>" . xref-pop-marker-stack))))
  :hook ((c-mode c++-mode) . lsp)
  )
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil)
  (advice-add 'lsp-ui-doc--extract-marked-string :around
     (lambda (original marked-string &optional language)
       "Beautify text in side window (remove ^M and set correct encoding)"
       (funcall original
                (cond ((stringp marked-string)
                       (s-trim (decode-coding-string marked-string 'latin-1)))
                      (t marked-string))
                language)))
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-include-signature t))

(use-package company-lsp :commands company-lsp)

(add-to-list 'auto-mode-alist '("\\.do\\'" . sh-mode))

(use-package tvfile :load-path "hs"
  :commands tvfile-minor-mode
  :custom-face
  (tv-channel1 ((t (:background "#aa8800"))))
  (tv-channel2 ((t (:background "#800000"))))
  (tv-channel3 ((t (:background "#002255"))))
  (tv-test-positive ((t (:background "forest green"))))
  (tv-test-start ((t (:background "yellow3" :foreground "#483737")))))

(use-package findcode :load-path "ebed"
  :commands ebed:findCodePoint)

(use-package flycheck-codecheck :load-path "ebed"
  :commands ebed:flycheck-codecheck-enable-in-this-buffer)

(use-package yaml-mode
  :mode ("\\.yml" "\\.yaml"))

(use-package cmake-mode
  :mode ("CMakeLists.txt"))

(use-package cmake-font-lock
  :hook ((cmake-mode) . cmake-font-lock-activate))
