
(require 'ivy)

(defvar cq-find-tables--format-spec ""
  "Store the current candidates format spec.")

(defvar cq-find-tables--width nil
  "Store the amount of digits needed for the longest line nubmer.")

(defun cq-list-local-tables ()
  "Search for available tables in the current file."
  (interactive)
  (let ((tables))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*#\\+\\(tbl\\)?name:[ \t]*\\(.*\\)" nil t)
	  (let ((text (match-string 2)))
	    (set-text-properties 0 (length text)
                                 `(cq-tbl-start ,(point-at-bol)
                                                cq-tbl-buffer ,(current-buffer)
                                                  )
                                 text)
	    (setq tables (cons text tables))))))
    (nreverse tables)))

(defun cq-find-tables--candidates ()
  "Return a list of this buffer lines."
  (let* ((tables (cq-list-local-tables))
         (n-lines (length tables)))
    (unless (zerop n-lines)
      (setq cq-find-tables--width (1+ (floor (log n-lines 10))))
      (setq cq-find-tables--format-spec
            (format "%%-%dd " cq-find-tables--width))
      (let ((line-number 0)
            (advancer (if visual-line-mode
                          #'line-move
                        #'forward-line))
            candidates)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (swiper-font-lock-ensure)
            (mapc (lambda (table)
                    (let* ((tbl-start (get-text-property 0 'cq-tbl-start table))
                           (str (concat " " table)))
                      (put-text-property 0 1 'display
                                         (format cq-find-tables--format-spec
                                                 (line-number-at-pos tbl-start))
                                         str)
                      (push str candidates))
                    )
                  tables)
          (nreverse candidates)))))))

(defvar cq-find-tables--opoint 1
  "The point when `swiper' starts.")

(defun cq-find-tables ()
  "Locate all tables in current buffer."
  (interactive)
  (cq-find-tables--ivy))

(defvar cq-find-tables--anchor nil
  "A line number to which the search should be anchored.")

(defvar cq-find-tables--len 0
  "The last length of input for which an anchoring was made.")

(defun cq-find-tables--init ()
  "Perform initialization common to both completion methods."
  (setq cq-find-tables--opoint (point))
  (setq cq-find-tables--len 0)
  (setq cq-find-tables--anchor (line-number-at-pos)))

(defvar cq-find-tables-history nil)

(defun cq-find-tables--ivy ()
  "`isearch' with an overview using `ivy'.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (cq-find-tables--init)
  (let ((candidates (cq-find-tables--candidates))
        (preselect (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
        (minibuffer-allow-text-properties t)
        res)
    (unwind-protect
         (setq res (ivy-read
                    "Tables: "
                    candidates
                    :initial-input initial-input
                    :keymap swiper-map
                    :preselect preselect
                    :require-match t
                    :update-fn #'cq-find-tables--update-input-ivy
                    :unwind #'cq-find-tables--cleanup
                    :re-builder #'swiper--re-builder
                    :history 'cq-find-tables-history
                    ))
      (if (null ivy-exit)
          (goto-char cq-find-tables--opoint)
        (cq-find-tables--action res ivy-text)))))

(defun cq-find-tables--ensure-visible ()
  "Remove overlays hiding point."
  (let ((overlays (overlays-at (point)))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defvar cq-find-tables--overlays nil
  "Store overlays.")

(defun cq-find-tables--cleanup ()
  "Clean up the overlays."
  (while cq-find-tables--overlays
    (delete-overlay (pop cq-find-tables--overlays)))
  (save-excursion
    (goto-char (point-min))
    (isearch-clean-overlays)))

(defun cq-find-tables--update-input-ivy ()
  "Called when `ivy' input is updated."
  (with-ivy-window
    (cq-find-tables--cleanup)
    (when (> (length ivy--current) 0)
      (let* ((re (funcall ivy--regex-function ivy-text))
             (re (if (stringp re) re (caar re)))
             (str (get-text-property 0 'display ivy--current))
             (num (if (string-match "^[0-9]+" str)
                      (string-to-number (match-string 0 str))
                    0)))
        (goto-char (point-min))
        (when (cl-plusp num)
          (goto-char (point-min))
          (if visual-line-mode
              (line-move (1- num))
            (forward-line (1- num)))
          (if (and (equal ivy-text "")
                   (>= cq-find-tables--opoint (line-beginning-position))
                   (<= cq-find-tables--opoint (line-end-position)))
              (goto-char cq-find-tables--opoint)
            (re-search-forward re (line-end-position) t))
          (isearch-range-invisible (line-beginning-position)
                                   (line-end-position))
          (unless (and (>= (point) (window-start))
                       (<= (point) (window-end (ivy-state-window ivy-last) t)))
            (recenter)))
        (cq-find-tables--add-overlays re)))))

(defun cq-find-tables--add-overlays (re &optional beg end)
  "Add overlays for RE regexp in visible part of the current buffer.
BEG and END, when specified, are the point bounds."
  (let ((ov (if visual-line-mode
                (make-overlay
                 (save-excursion
                   (beginning-of-visual-line)
                   (point))
                 (save-excursion
                   (end-of-visual-line)
                   (point)))
              (make-overlay
               (line-beginning-position)
               (1+ (line-end-position))))))
    (overlay-put ov 'face 'swiper-line-face)
    (overlay-put ov 'window (ivy-state-window ivy-last))
    (push ov cq-find-tables--overlays)
    (let* ((wh (window-height))
           (beg (or beg (save-excursion
                          (forward-line (- wh))
                          (point))))
           (end (or end (save-excursion
                          (forward-line wh)
                          (point)))))
      (when (>= (length re) swiper-min-highlight)
        (save-excursion
          (goto-char beg)
          ;; RE can become an invalid regexp
          (while (and (ignore-errors (re-search-forward re end t))
                      (> (- (match-end 0) (match-beginning 0)) 0))
            (let ((i 0))
              (while (<= i ivy--subexps)
                (when (match-beginning i)
                  (let ((overlay (make-overlay (match-beginning i)
                                               (match-end i)))
                        (face
                         (cond ((zerop ivy--subexps)
                                (cadr swiper-faces))
                               ((zerop i)
                                (car swiper-faces))
                               (t
                                (nth (1+ (mod (+ i 2) (1- (length swiper-faces))))
                                     swiper-faces)))))
                    (push overlay cq-find-tables--overlays)
                    (overlay-put overlay 'face face)
                    (overlay-put overlay 'window (ivy-state-window ivy-last))
                    (overlay-put overlay 'priority i)))
                (cl-incf i)))))))))

(defun cq-find-tables--action (x input)
  "Goto line X and search for INPUT."
  (if (null x)
      (user-error "No candidates")
    (goto-char (point-min))
    (funcall (if visual-line-mode
                 #'line-move
               #'forward-line)
             (1- (read (get-text-property 0 'display x))))
    (re-search-forward
     (ivy--regex input) (line-end-position) t)
    (cq-find-tables--ensure-visible)
    (when (/= (point) cq-find-tables--opoint)
      (unless (and transient-mark-mode mark-active)
        (push-mark cq-find-tables--opoint t)
        (message "Mark saved where search started")))))

(provide 'cq-find-tables)
