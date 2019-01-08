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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 79)
 '(package-selected-packages
   (quote
    (expand-region counsel ivy idle-highlight-mode fill-column-indicator use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq-default cursor-type 'bar)
(cua-mode)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(setq mouse-wheel-scroll-amount '(4 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
; Disable the stupid bell
(defun my-bell-function ())
(setq ring-bell-function 'my-bell-function)

(use-package naquadah-theme)
(load-theme 'naquadah t)

(use-package ebed-progface :load-path "ebed")

(use-package cc-mode)
(add-hook 'c-mode-common-hook 'ebed:progface)
(add-hook 'emacs-lisp-mode-hook 'ebed:progface)

(use-package bind-key)

(use-package ivy
  :config
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-wrap t)
    (setq ivy-height 20)
  :bind
    (("C-b" . ivy-switch-buffer)))
(ivy-mode 1)

(use-package counsel
  :commands counsel-yank-pop counsel-M-x
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
  
(load-file "~/.emacs.d/mykeys.el")

