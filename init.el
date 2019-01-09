;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Only for Siemens!!
;; (setq url-using-proxy nil)
;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("http" . "127.0.0.1:3128")
;;         ("https" . "127.0.0.1:3128")))
;; (add-to-list 'package-archives
;;       '("hs" . "g:/user/hs/emacs/packages"))
 
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
 '(mouse-wheel-scroll-amount (quote (4 ((shift) . 1) ((control)))))
 '(package-selected-packages
   (quote
    (comment-dwim-2 asm86-mode undo-tree magit web-mode ws-butler bm expand-region counsel ivy idle-highlight-mode fill-column-indicator use-package)))
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

(use-package bind-key)

(use-package ivy
  :config
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-wrap t)
    (setq ivy-height 20)
    (setq ivy-count-format "%d/%d ")
    (setq ivy-re-builders-alist '((t   . ivy--regex-ignore-order)))
  :bind
    (("C-b" . ivy-switch-buffer)
	 (:map ivy-minibuffer-map ("TAB" . ivy-alt-done))))
(ivy-mode 1)

(use-package counsel
  :commands counsel-yank-pop counsel-M-x
  :config
    (setq ivy-initial-inputs-alist
        (delq (assoc 'counsel-M-x ivy-initial-inputs-alist)
              ivy-initial-inputs-alist))
  :bind
    (("C-o" . counsel-find-file)
	 ("C-S-o" . counsel-git)
	 ("M-v" . counsel-yank-pop))
  :init
    (bind-key* "M-v" 'counsel-yank-pop)
	(define-key cua--cua-keys-keymap (kbd "M-v") 'counsel-yank-pop)
	(bind-key* "M-a" 'counsel-M-x))
(counsel-mode 1)

(use-package swiper
  :bind (("C-f" . swiper)))
  
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

(use-package ws-butler
  :config (setq ws-butler-global-mode t))

(use-package ebed-helpers :load-path "ebed"
  :commands ebed:revert-buffer-without-prompt)

(use-package magit
  :bind (("C-x v g" . magit-status)))

(use-package undo-tree)
(global-undo-tree-mode)

(use-package comment-dwim-2
  :bind (("M-," . comment-dwim-2)))

(load-file "~/.emacs.d/languages.el")

(load-file "~/.emacs.d/mykeys.el")

