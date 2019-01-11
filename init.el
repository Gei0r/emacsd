;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (getenv "CLEARCASE_GROUPS") (setq isSiemens t))
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
    (ace-window ace-jump-buffer avy multiple-cursors helm-swoop helm-ls-git helm comment-dwim-2 asm86-mode undo-tree magit web-mode ws-butler bm expand-region counsel ivy idle-highlight-mode fill-column-indicator use-package)))
 '(sentence-end-double-space nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(warning-suppress-types (quote ((undo discard-info)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :height 120 :width normal :foundry "outline" :family "Courier New"))))
 '(bm-fringe-face ((t (:background "deep sky blue" :foreground "White"))))
 '(column-marker-1 ((t (:background "#782121"))))
 '(hl-line ((t (:background "dark slate gray"))))
 '(idle-highlight ((t (:background "MediumPurple4")))))

(setq-default cursor-type 'bar)
(cua-mode)
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq-default buffer-file-coding-system 'utf-8-unix)
(global-auto-revert-mode)

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
		 (emacs-lisp-mode . ebed:progface)))

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
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))
  :bind
  (("C-b" . helm-mini)
   ("C-o" . helm-find-files)
   (:map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         ("C-d" . helm-toggle-buffer-details))
   (:map helm--minor-mode-map
         ("C-d" . helm-toggle-buffer-details))
   (:map cua--cua-keys-keymap ("M-v" . helm-show-kill-ring))))
(helm-mode 1)

(use-package helm-ls-git :bind (("C-S-o" . helm-ls-git-ls)))

(use-package helm-swoop :bind (("C-f" . helm-swoop)))

(use-package ebed-helm-buffers-persistent-kill :load-path "ebed"
  :bind (:map helm-map ("<f12>" . ebed:helm-buffers-persistent-kill)))
  
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

(use-package ws-butler :config (setq ws-butler-global-mode t))

(use-package ebed-helpers :load-path "ebed"
  :commands ebed:revert-buffer-without-prompt)

(use-package magit :bind (("C-x v g" . magit-status)))

(use-package undo-tree)
(global-undo-tree-mode)

(use-package comment-dwim-2 :bind (("M-," . comment-dwim-2)))

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
  :bind
  ("<f4>" . ebed:find-other-file)
  ("<S-return>" . ebed:newline-with-semicolon))

(use-package ebed-clearcase :load-path "ebed"
  :bind ("C-w" . ebed:cc-hijack-file-in-buffer))

(load-file "~/.emacs.d/languages.el")

(load-file "~/.emacs.d/mykeys.el")

