(define-key global-map (kbd "C-r") 'isearch-forward)
(define-key global-map (kbd "C-S-r") 'isearch-backward)

(define-key global-map (kbd "M-n") (lambda() (interactive) (insert "„")))
(define-key global-map (kbd "M-m") (lambda() (interactive) (insert "“")))

(define-key global-map (kbd "C-<down>") 'forward-sexp)
(define-key global-map (kbd "C-<up>") 'backward-sexp)

(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-a") 'mark-whole-buffer)
