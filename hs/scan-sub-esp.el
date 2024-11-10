
(defun ebed:scan-sub-esp ()
  (interactive)
  (let
      ((default-directory (read-directory-name "_scratch dir:"
                                               "G:/project/_mapfiles/"))
       (rg-args (list "-i"
                      "sub\\s+e?sp\\s*,\\s*([^\\r]+)|enter\\s+([^,]+),[^\\r]+"
                      "-uu" "-g" "*.lst" "-g" "*.LST")))
    (with-temp-buffer
      (apply 'call-process
             (append (list "rg" nil t nil)
                     rg-args))
    (hs/scan-sub-esp))))

(defun hs/scan-sub-esp ()
  "Scan for sub esp, sub sp and enter values, show max."
  (interactive)
  (goto-char (point-min))
  (let ((case-fold-search t)
        (max-esp 0)
        (max-sp 0)
        (max-enter 0)
        (warning ""))
    (while (re-search-forward "\\(?:sub\\s-+\\(?:\\(esp\\)\\|\\(sp\\)\\)\\)\\s-*,\\s-*\\([0-9a-zA-Z@_]+\\)\\| enter\\s-+\\([^,]+\\)\\s-*,[^\r+]" nil t)
      (cond
       ((match-string 1) ;; esp
        (let ((val (match-string 3)))
          (message "match esp %s" val)
          (cond
           ((string-match "[0-9]+" val)
            (setq max-esp (max max-esp (string-to-number val))))
           ((string-match "[0-9a-fA-F]+H" val)
            (setq max-esp (max max-esp (string-to-number val 16))))
           (t
            (setq warning (concat warning (format " %s" val)))
            (message "*** error *** unknown value %s" val)))))
       ((match-string 2) ;; sp
        (let ((val (match-string 3)))
          (message "match sp %s" val)
          (cond
           ((string-match "[0-9a-fA-F]+H" val)
            (setq max-sp (max max-sp (string-to-number val 16))))
           ((string-match "[0-9]+" val)
            (setq max-sp (max max-sp (string-to-number val))))
           (t
            (setq warning (concat warning (format " %s" val)))
            (message "*** error *** unknown value %s" val)))))
       ((match-string 4) ;; enter
        (let ((val (match-string 4)))
          (message "match enter %s" val)
          (cond
           ((string-match "[0-9a-fA-F]+H" val)
            (setq max-enter (max max-enter (string-to-number val 16))))
           ((string-match "[0-9]+" val)
            (setq max-enter (max max-enter (string-to-number val))))
           (t
            (setq warning (concat warning (format " %s" val)))
            (message "*** error *** unknown value %s" val)))))))
    (with-temp-buffer
      (insert (format "%d | %d | %d | %d | %s" max-sp max-esp max-enter (max max-sp max-esp max-enter)  warning))
      (kill-region (point-min) (point-max)))
    (message "max-sp %d max-esp %d max-enter %d %s"
             max-sp max-esp max-enter (if (> (length warning) 0) "warning" ""))))
