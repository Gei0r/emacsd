(define-key global-map (kbd "C-r") 'isearch-forward)
(define-key global-map (kbd "C-S-r") 'isearch-backward)

(define-key global-map (kbd "M-j") (lambda() (interactive) (insert "“")))
(define-key global-map (kbd "M-k") (lambda() (interactive) (insert "”")))
(define-key global-map (kbd "M-n") (lambda() (interactive) (insert "„")))
(define-key global-map (kbd "M-m") (lambda() (interactive) (insert "“")))

(define-key global-map (kbd "C-<down>") 'forward-sexp)
(define-key global-map (kbd "C-<up>") 'backward-sexp)

(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-a") 'mark-whole-buffer)

(define-key global-map (kbd "C-r") 'isearch-forward)
(define-key global-map (kbd "C-S-r") 'isearch-backward)

(define-key global-map (kbd "C-1") 'delete-other-windows)
(define-key global-map (kbd "C-2")
  (lambda() (interactive) (select-window (split-window-below))))
(define-key global-map (kbd "C-3")
  (lambda() (interactive) (select-window (split-window-right))))
(define-key global-map (kbd "C-0") 'delete-window)

(define-key global-map (kbd "C-j f") 'beginning-of-defun)
(define-key global-map (kbd "C-j F") 'end-of-defun)
(define-key global-map (kbd "C-j l") 'goto-line)

(define-key global-map (kbd "C-k") 'kill-whole-line)

(define-key global-map (kbd "C-n") 'call-last-kbd-macro)
