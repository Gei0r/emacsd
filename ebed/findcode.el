
(use-package bm :commands bm-bookmark-add)
(defun ebed:findCodePoint(selector offset)
  (interactive "sSelector: \nsOffset: ")

  (setq selector (string-to-number selector 16))
  (setq offset   (string-to-number offset   16))

  ;; Find _scratch directory
  (let
      ((scratch-dir
        (concat (locate-dominating-file buffer-file-name "_scratch")
                "_scratch"))
       map-filename
       function-offset
       function-name
       all-lst-files
       proc-asm-regex
       proc-c-regex
       current-sourcefilename
       target-filename
       target-linenumber
       target-addr
       (map-file-number 0)
       temp-buffer)

    (setq map-filename (directory-files scratch-dir t "\\.map$"))


    (when (> (length map-filename) 1)
      (setq temp-buffer (get-buffer-create "*ebed:findCodePoint*"))
      (switch-to-buffer-other-window temp-buffer)
      (erase-buffer)
      (insert "Which map file?\n\n")
      (--each-indexed map-filename
        (progn
          (string-match "\\([^/]+\\)+$" it)
          (insert (number-to-string it-index) " " (match-string 1 it) "\n")))
      (setq map-file-number (string-to-number(read-string "Which map file? ")))
      (kill-buffer temp-buffer))

    (setq map-filename (nth map-file-number map-filename))

    (with-temp-buffer
      (insert-file-contents map-filename)
      (goto-char 1)
      (search-forward "SYMBOL MAP (ALL)")
      (while
          (re-search-forward
           "\\([0-9A-F]+\\):\\([0-9A-F]+\\)\s+[0-9A-F]+\s+\\([^ ]+\\)" nil t)

        (when (and (= (string-to-number (match-string 1) 16) selector)
                   (< (string-to-number (match-string 2) 16) offset))


          (setq function-offset (string-to-number (match-string 2) 16))
          (setq function-name   (match-string 3)))))

    (setq offset (- offset function-offset))
    (message "Code point is 0x%x bytes into %s" offset function-name)

    ;; Find all list files in _scratch
    (setq all-lst-files (append (directory-files scratch-dir t "\\.lst$")
                                (directory-files (concat scratch-dir "/LST")
                                                 t "\\.lst$")))

    (setq proc-asm-regex (concat "^ "
                                 "\\([0-9a-f]\\{8\\}\\|"
                                 "    [0-9a-f]\\{4\\}\\)"
                                 "[^;\n]+"
                                 function-name
                                 "\\s-+PROC \\(\\(NEAR\\)\\|\\(FAR\\)\\)"))

    (setq proc-c-regex (concat function-name
                               "    PROC \\(\\(NEAR\\)\\|\\(FAR\\)\\)"))

    ;; Find a list file that contains the requested function definition
    (--each-while all-lst-files
      (with-temp-buffer
        (insert-file-contents it)

        (if (re-search-forward proc-asm-regex nil t)
            (progn
              ;; We found the offending function in the ASM list-file `it' at
              ;; address (match-string 1).

              ;; remember file name
              (setq target-filename it)
              (setq target-addr
                    (+ offset (string-to-number (match-string 1) 16)))
              (message "search for " target-addr)
              ;; find address (offset + (match-string 1))
              (re-search-forward (format "^ %08x\\|^     %04x"
                                         target-addr target-addr))

              ;; remember line number
              (setq target-linenumber (line-number-at-pos))
              nil)  ; stop searching
          (if (re-search-forward proc-c-regex nil t)
              (progn
                ;; We found the offending function in the C++ list file `it' at
                ;; point
                (setq target-filename it)

                (re-search-forward "^   \\([0-9_a-zA-Z]+\\):\\([0-9A-F]+\\)")

                ;; function starts at address (match-string 2) in segment
                ;; (match-string 1)

                (re-search-forward (format "^   %s:%06x"
                                           (match-string 1)
                                           (+ offset
                                              (string-to-number
                                               (match-string 2) 16))))

                (setq target-linenumber (line-number-at-pos))

                nil)  ; stop searching
            t)  ; continue searching
          )))

    ;; jump to file name + line number
    (find-file target-filename)
    (goto-line target-linenumber)
    (bm-bookmark-add)))
