;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

; list the packages you want
(setq package-list 
    '(
	    use-package
    ))

;; Only for Siemens!!
;; (setq url-using-proxy nil)
;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("http" . "127.0.0.1:3128")
;;         ("https" . "127.0.0.1:3128")))
;; (add-to-list 'package-archives
;;       '("hs" . "g:/user/hs/emacs/packages"))
 
; fetch the list of packages available
(unless package-archive-contents (package-refresh-contents))

; install the missing packages
(dolist (package package-list) (unless (package-installed-p package) (package-install package)))

(custom-set-variables
  '(package-selected-packages (quote (use-package))))
(custom-set-faces)
