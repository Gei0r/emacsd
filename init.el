;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(if (getenv "CLEARCASE_GROUPS") (setq isSiemens t) (setq isSiemens nil))
(when isSiemens
  (setq url-using-proxy nil)
  (setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "127.0.0.1:3128")
        ("https" . "127.0.0.1:3128")))
  (add-to-list 'package-archives
      '("hs" . "g:/user/hs/emacs/packages")))

 
; install use-package
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(use-package quelpa
  :init (setq quelpa-checkout-melpa-p nil))
(use-package quelpa-use-package)
(quelpa-use-package-activate-advice)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(case-replace nil)
 '(cua-auto-tabify-rectangles nil)
 '(cua-keep-region-after-copy t)
 '(electric-pair-mode t)
 '(fill-column 79)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (6 ((shift) . 1) ((control) . 0.5))))
 '(package-selected-packages
   (quote
    (overlay cqtbl cq compilation-mode git-gutter+ git-gutter-fringe+ lsp-mode company ccls company-lsp lsp-ui tide typescript-mode flyspell-correct-helm yasnippet flycheck ace-window ace-jump-buffer avy multiple-cursors helm-swoop helm-ls-git helm comment-dwim-2 asm86-mode undo-tree magit web-mode ws-butler bm expand-region counsel ivy idle-highlight-mode fill-column-indicator use-package)))
 '(sentence-end-double-space nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(warning-suppress-types (quote ((undo discard-info)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "dark slate blue" :foreground "ivory"))))
 '(hl-line ((t (:background "dark slate gray")))))

(if (eq system-type 'windows-nt)
    (face-spec-set 'default '((t :height 120 :family "Courier New")))
  (face-spec-set 'default '((t :height 120 :family "DejaVu Sans Mono"))))

(setq-default cursor-type 'bar)
(cua-mode)
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(global-auto-revert-mode)
(setq revert-without-query '(".*"))

; Disable the stupid bell
(defun my-bell-function ())
(setq ring-bell-function 'my-bell-function)

(define-key global-map (kbd "C-j") nil)

(use-package naquadah-theme)
(load-theme 'naquadah t)
(global-hl-line-mode)
(show-paren-mode)

(use-package ebed-progface :load-path "ebed"
  :commands ebed:progface
  :hook ((web-mode . ebed:progface)
         (c-mode-common . ebed:progface)
		 (emacs-lisp-mode . ebed:progface)
         (typescript-mode . ebed:progface)
         (python-mode . ebed:progface)))

(use-package helm
  :init
  
  :bind*
  ("M-v" . helm-show-kill-ring)
  ("M-a" . helm-M-x)
  :config
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffer-max-length nil)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-ff-skip-boring-files t)
  (setq helm-findutils-skip-boring-files t)
  (setq helm-recentf-fuzzy-match t)
  (add-to-list 'display-buffer-alist
               `("^\*\\(H\\|h\\)elm.*\*\$"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))
  :bind
  (("C-b" . helm-mini)
   ("C-o" . helm-find-files)
   ("C-x h" . helm-resume)
   (:map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         ("C-d" . helm-toggle-buffer-details))
   (:map helm--minor-mode-map
         ("C-d" . helm-toggle-buffer-details))
   (:map cua--cua-keys-keymap ("M-v" . helm-show-kill-ring))))
(helm-mode 1)

(use-package compile
  :bind ((:map compilation-mode-map (("C-o" . helm-find-files)))))

(use-package helm-ls-git :bind (("C-S-o" . helm-ls-git-ls)))

(use-package helm-swoop :bind (("C-f" . helm-swoop-without-pre-input))
  :config
  (setq helm-swoop-split-window-function 'helm-default-display-buffer))

(use-package ebed-helm-buffers-persistent-kill :load-path "ebed"
  :bind (:map helm-map ("<f12>" . ebed:helm-buffers-persistent-kill)))

(use-package ergoemacs-helm-ff-backspace :load-path "ebed"
  :bind ((:map helm-find-files-map ("DEL" . ergoemacs-helm-ff-backspace))
         (:map helm-read-file-map  ("DEL" . ergoemacs-helm-ff-backspace))))

(use-package ebed-load-dir-settings :load-path "ebed"
  :commands load-dir-settings
  :hook (find-file . load-dir-settings))

(use-package ebed-compile :load-path "ebed"
  :bind
  (("<f5>" . (lambda() (interactive) (ebed:compile-binding 0))))
  (("<f6>" . (lambda() (interactive) (ebed:compile-binding 1))))
  (("<f7>" . (lambda() (interactive) (ebed:compile-binding 2))))
  (("<f8>" . (lambda() (interactive) (ebed:compile-binding 3))))
  (("<f9>" . (lambda() (interactive) (ebed:compile-binding 4))))
  (("<f10>" . (lambda() (interactive) (ebed:compile-binding 5)))))

(use-package expand-region
  :bind (("C-S-<up>" . er/expand-region)))
  
(use-package eshell
  :bind (("C-e" . eshell))
  :config
    (setq eshell-scroll-to-bottom-on-input 'all))
(use-package ebed-eshell-here :load-path "ebed"
  :bind (("C-S-e" . ebed:eshell-here)))

(use-package bm 
  :bind (("C-j n" . bm-next) ("C-j p" . bm-previous) ("C-j s" . bm-toggle))
  :config (setq bm-highlight-style 'bm-highlight-only-fringe)
  :custom-face (bm-fringe-face ((t (:background "deep sky blue" :foreground "White")))))

(use-package ws-butler :config (ws-butler-global-mode))

(use-package ebed-helpers :load-path "ebed"
  :commands ebed:revert-buffer-without-prompt)

(use-package magit :bind (("C-x v g" . magit-status)))

(use-package undo-tree)
(global-undo-tree-mode)

(use-package comment-dwim-2 :bind* (("M-," . comment-dwim-2)))

(use-package multiple-cursors
  :init (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  ("C-l" . mc/edit-lines)
  ("M-<mouse-1>" . mc/add-cursor-on-click))

(use-package avy :bind ("C-j w" . avy-goto-word-0))

(use-package ace-window :bind ("C-j b" . ace-window))

(use-package x4-smarter-beginning-of-line :load-path "ebed"
  :bind ("<home>" . x4-smarter-beginning-of-line))

(use-package ebed-misc :load-path "ebed"
  :commands (ebed:insert-path)
  :bind
  ("<f4>" . ebed:find-other-file)
  ("<S-return>" . ebed:newline-with-semicolon))

(use-package ebed-clearcase :load-path "ebed"
  :bind ("C-w" . ebed:cc-hijack-file-in-buffer))

(use-package flycheck
  :hook ((c++-mode c-mode) . (lambda () (flycheck-mode)
                               (push 'c/c++-clang flycheck-disabled-checkers)
                               (push 'c/c++-gcc   flycheck-disabled-checkers))))

(use-package company
  :bind (("C-SPC" . company-complete))
  :hook (after-init-hook . global-company-mode)
  :config
  (setq company-idle-delay nil)
  (setq company-show-numbers t)
  (delete 'company-clang company-backends)
  (delete 'company-semantic company-backends)
  :custom-face
  (company-tooltip ((t (:background "#fce94f" :foreground "black")))))

(use-package yasnippet
  :config
  (setq yas-indent-line 'fixed)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind ((:map yas-keymap
               ("RET" . yas-next-field)
               ("<tab>" . yas-next-field))))


(use-package ebed-bib :load-path "ebed"
  :bind* ("C-x i" . ebed:getBibentryAtPoint)
  :config (setq ebed:bibDatabase "D:/User/Adrian/Literaturdatenbank"))

(use-package git-gutter+
  :demand
  :config
  (global-git-gutter+-mode)
  (set-face-foreground 'git-gutter+-added "#008000")
  (set-face-foreground 'git-gutter+-deleted "#9f0000")
  (set-face-foreground 'git-gutter+-modified "#9f9f00")
  (setq git-gutter+-lighter " GG")
  :bind ((:map git-gutter+-mode-map
               (("C-j h" . git-gutter+-next-hunk)
                ("C-j H" . git-gutter+-previous-hunk)
                ("C-x v r" . git-gutter+-revert-hunks)
                ("C-x v s" . git-gutter+-stage-hunks)))))

(load-file "~/.emacs.d/languages.el")

(load-file "~/.emacs.d/mykeys.el")
(when isSiemens (load-file "~/.emacs.d/cq/init-cq.el"))
