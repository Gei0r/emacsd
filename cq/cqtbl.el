;; -*- coding: utf-8-unix; -*-
;;; cq-table.el --- ClearQuest Table merger
;;
;; Copyright (C) 2015 Stefan-W. Hahn
;;
;; Author: Stefan-W. Hahn <stefan dot hahn at s-hahn dot de>
;; Keywords: cq table
;; Version:
;;
;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;

;; Ideas and some parts of code from:
;; https://github.com/tbanel/orgtbljoin

;; Open points:
;;; - saving of highlights made by merge
;;; - showing hints for narrowed columns
;;; - merging of tables if there are mor than one table in file,
;;;   means showup a selection of found tables in buffer


(require 'font-lock)
(require 'ediff-util)  ;; ediff-other-buffer
(require 'cq)
(require 'org-table)

(defgroup cqtbl-faces nil
  "Faces in Cqtbl."
  :tag "Cqtbl Faces"
  :group 'convenience)

(defface cqtbl-highlight
  '((t :background "Orange1" :inherit highlight))
  "Default face for highlighting the current line in Hl-Line mode."
  :version "22.1"
  :group 'cqtbl-faces)

(defun cqtbl-highlight (text hint &optional face)
  "Highlight region from BEG to END with FACE."
  ;; (unless color
  ;;   (setq color (s-trim (helm-colors))))
  (let ((len (length text)))
    (add-text-properties
     0 len
     `(font-lock-face ,face)
     text)
    (when (> len 0)
      (add-text-properties 0 len
                         `(cqtbl ,hint help-echo ,hint)
                         text)
      ))
  text
  )

(defun cqtbl--merge-append-mas-ref-row (masrow refrow refcol &optional mergecols no-fontify)
  "Concatenate master and reference rows, skipping the reference column.
MASROW is a list of cells from the master table.  REFROW is a
list of cells from the reference table.  REFCOL is the position,
numbered from zero, of the column in REFROW that should not be
appended in the result, because it is already present in MASROW.
MERGECOLS is a list cons with positions, numbered from zero. The
car of each cons is the column of REFROW, the cdr the column of
MASROW to be merged with."
  (let ((result masrow)
        (i 0))
    ;; merge rows
    (dolist (elt mergecols)
      (let* ((ref (nth (car elt) refrow))
             (col (cdr elt))
             (mas (nth col masrow)))
        (if (not (string= ref mas))
            (setf (nth col result)
                  (if no-fontify
                      ref
                    (cqtbl-highlight
                     ref
                     (concat "was value: " mas)
                     'cqtbl-highlight))))))
    (setq result (reverse result))
    ;; append new cols
    (while refrow
      (unless (or (equal i refcol)
                  (assq i mergecols))
	(setq result (cons (car refrow) result)))
      (setq refrow (cdr refrow))
      (setq i (1+ i)))
    (reverse result)))

;; (cqtbl--merge-append-mas-ref-row
;;  '("0"   "KEY" "a"   "b" "c" "d")
;;  '("res" "new" "KEY" "x" "d" "y")
;;  2
;;  '((1 . 2) (4 . 5)))

(defgroup cqtbl nil
  "Options specific for ClearQuest Table."
  :group 'cqtbl
  :tag "ClearQuest")

(defcustom cqtbl-default-key "CFXID"
  "Column header of key column for joining tables."
  :group 'cqtbl
  :type 'string)

(defvar cqtbl-column-history nil
  "History for querying columns.")

;; borrowed from: https://github.com/tbanel/orgtbljoin
(defun cqtbl--query-column (prompt table &optional key)
  "Interactively query a column.
PROMPT is displayed to the user to explain what answer is expected.
TABLE is the org mode table from which a column will be choosen
by the user.  Its header is used for column names completion.  If
TABLE has no header, completion is done on generic column names:
$1, $2..."
  ;; skip 'hline at beginning
  (while (eq 'hline (car table))
    (setq table (cdr table)))
  ;; skip format headers
  (if (memq 'hline table) ;; table has a header
      (while (not (eq (cadr table) 'hline))
        (setq table (cdr table))))
  (completing-read
   prompt
   (if (memq 'hline table) ;; table has a header
       (mapcar #'identity (car table))
     (let ((i 0))
       (setq key nil)
       (mapcar (lambda (x) (format "$%s" (setq i (1+ i))))
               (car table))))
   nil nil key cqtbl-column-history key)
  )

;; (defun xx ()
;;   (interactive)
;;   (let ((tbl (org-table-to-lisp))
;;         col)
;;     (cqtbl--query-column "Key column: " tbl cqtbl-default-key)))

;; borrowed from: https://github.com/tbanel/orgtbljoin
(defun cqtbl--colname-to-int (col table)
  "Convert the column name into an integer (first column is numbered 0)
COL may be:
- a dollar form, like $5 which is converted to 4
- a number, like 5 which is converted to 4
- an alphanumeric name which appears in the column header (if any)
When COL does not match any actual column, an error is generated.
TABLE is an Org mode table passed as a list of lists of cells.
It is used to check COL against TABLE header."
  ;; skip first hlines if any
  (while (not (listp (car table)))
    (setq table (cdr table)))
  ;; skip format headers
  (if (memq 'hline table) ;; table has a header
      (while (not (eq (cadr table) 'hline))
        (setq table (cdr table))))
  (if (symbolp col)
      (setq col (symbol-name col)))
  (cond ((numberp col)
	 t)
	((string-match "^\\$?\\([0-9]+\\)$" col)
	 (setq col (string-to-number (match-string 1 col))))
	(t
	 ;; TABLE has no header, COL does not make sense
	 (unless (memq 'hline table)
	   (user-error "No header on the table, and no such column '%s'" col))
	 ;; iterate over first line of header to find COL
	 (let ((i 0)
	       (n))
	   (mapc (lambda (c)
		   (setq i (1+ i))
		   (if (equal col c)
		       (setq n i)))
		 (car table))
	   (unless n (user-error "No such column '%s'" col))
	   (setq col n))))
  (setq col (1- col))
  (if (or (< col 0) (>= col (length (car table))))
      (user-error "Column %s outside table" col))
  col)

;; borrowed from: https://github.com/tbanel/orgtbljoin
(defun cqtbl--convert-to-hashtable (table col)
  "Convert an Org-mode TABLE into a hash table.
The purpose is to provide fast lookup to TABLE's rows.  The COL
column contains the keys for the hashtable entries.  Return a
cons, the car contains the header, the cdr contains the
hashtable."
  ;; skip heading horizontal lines if any
  (while (eq (car table) 'hline)
    (setq table (cdr table)))
  ;; split header and body
  (let ((head)
	(body (memq 'hline table))
	(hash (make-hash-table :test 'equal :size (+ 20 (length table)))))
    (if (not body)
	(setq body table)
      (setq head table)
      ;; terminate header with nil
      (let ((h head))
	(while (not (eq (cadr h) 'hline))
	  (setq h (cdr h)))
	(setcdr h nil)))
    ;; fill-in the hashtable
    (mapc (lambda (row)
	    (when (listp row)
	      (let ((key (nth col row)))
		(puthash key (nconc (gethash key hash) (list row)) hash))))
	  body)
    (cons head hash)))

;; (defun xx ()
;;   (interactive)
;;   (let ((tbl (org-table-to-lisp))
;;         col)
;;     (setq col (cqtbl--query-column "Key column: " tbl cqtbl-default-key))
;;     (setq col (cqtbl--colname-to-int col tbl))
;;     (pp (cdr (cqtbl--convert-to-hashtable tbl col)))
;;     ))

(defun cqtbl--get-title (table)
  "Find title row of table."
  (let (head)
    ;; skip heading horizontal lines if any
    (while (eq (car table) 'hline)
      (setq table (cdr table)))
    (when (memq 'hline table)
      (setq head table)
      (while (not (eq (cadr head) 'hline))
        (setq head (cdr head)))
      (setq head (car head)))
    head))

(defun cqtbl--find-head (table)
  ""
  (let (head)
    ;; skip heading horizontal lines if any
    (while (eq (car table) 'hline)
      (setq table (cdr table)))
    (when (memq 'hline table)
      (setq head table)
      (while (not (eq (cadr head) 'hline))
        (setq head (cdr head)))
      (setq head (car head)))
    head))

(defun cqtbl--split-table (table)
  "Split given TABLE into preface, title, body and return as
list (PREFACE TITLE BODY) with each as lists."
  (let ((tb table)
        preface title body head)
    ;; skip heading horizontal lines if any
    (while (eq (car table) 'hline)
      (push (car table) preface)
      (setq table (cdr table))
      )
    (when (setq body (memq 'hline table))
      (while (not (eq table body))
        (if (eq (cadr table) 'hline)
            (setq head (cons (car table) head))
          (push (car table) preface))
        (setq table (cdr table))))
    (list (nreverse preface) head body)))

(ert-deftest cqtbl/cqtbl--split-table ()
  "Test `cqatbl--split-table'."
  (should (equal
           (cqtbl--split-table '(("a" "b") hline ("v" "w") ("x" "y")))
           '(nil (("a" "b")) (hline ("v" "w") ("x" "y")))))
  (should (equal
           (cqtbl--split-table '(hline ("a" "b") hline ("v" "w") ("x" "y")))
           '((hline) (("a" "b")) (hline ("v" "w") ("x" "y")))))
  (should (equal
           (cqtbl--split-table '(hline ("a1" "b1") ("a" "b") hline ("v" "w") ("x" "y")))
           '((hline ("a1" "b1")) (("a" "b")) (hline ("v" "w") ("x" "y")))))
  )

(defun cqtbl--merge-head (mastable reftable refcol)
  ""
  (let ((mhead (cqtbl--find-head mastable))
        (rhead (cqtbl--find-head reftable))
        (i 0)
        mergecols)
    (dolist (col rhead)
      (if (and (not (string= "" col))
               (/= i refcol)
               (member col mhead))
          (setq mergecols
                (cons
                 (cons i
                       (cqtbl--colname-to-int col mastable)
                       )
                 mergecols)))
      (incf i)
      )
    (nreverse mergecols)
    ))

;; (cqtbl--merge-head '(("A" "CFXID" "State" "C" "Category") hline)
;;                    '(("A" "B" "CFXID" "State" "RequestType" "Category") hline)
;;                    2)


(defun cqtbl--add-column (table colnum &optional name val)
  "Insert new column NAME before column COLNUM in table
TABLE. Fill in value VAL."
  (let ((val (or val ""))
        (name (or name ""))
        tbl body append row)
    ;; skip any hline a the top of table
    (while (eq (car table) 'hline)
      (setq tbl (cons 'hline tbl))
      (setq table (cdr table)))
    (when (> colnum (1+ (length (car table))))
      (user-error "Wrong column %d" colnum))
    (when (= colnum (length (car table)))
      (setq append t))
    (while table
      (setq row (car table))
      (setq table (cdr table))
      (cond
       ((eq row 'hline)
        (setq body t)
        (push 'hline tbl))
       (t
        (let ((val (if body
                       val
                     (if (eq (car table) 'hline)
                         name
                       ""))))
          (if append
              (setq tbl (cons (add-to-list 'row val 'append) tbl))
            (let ((i 0) new-row)
              (dolist (elt row)
                (when (= i colnum)
                  (push val new-row))
                (push elt new-row)
                (incf i))
              (setq tbl (cons (nreverse new-row) tbl))))))))
    (nreverse tbl)))

(ert-deftest cqtbl/cqtbl--add-column ()
  "Test `cqtbl--add-column'."
  (should (equal
           (cqtbl--add-column '(("a" "b") hline ("v" "w") ("x" "y"))
                              1 "name" "new")
           '(("a" "name" "b") hline ("v" "new" "w") ("x" "new" "y"))))
  (should (equal
           (cqtbl--add-column '(("a" "b") hline ("v" "w") ("x" "y"))
                              2 "name" "new")
           '(("a" "b" "name") hline ("v" "w" "new") ("x" "y" "new"))))
  (should (equal
           (cqtbl--add-column '(hline ("a" "b") hline ("v" "w") ("x" "y"))
                              1 "name" "new")
           '(hline ("a" "name" "b") hline ("v" "new" "w") ("x" "new" "y"))))
  (should (equal
           (cqtbl--add-column '(hline ("a1" "b1") ("a" "b") hline ("v" "w") ("x" "y"))
                              0 "name" "new")
           '(hline ("" "a1" "b1") ("name" "a" "b") hline ("new" "v" "w") ("new" "x" "y"))))
  )

(defun cqtbl--get-body (table)
  "Return body of TABLE as pointer to original table."
  ;; skip any hline a the top of table
  (while (eq (car table) 'hline)
    (setq table (cdr table)))
  (catch 'found
    (while table
      (if (eq (car table) 'hline)
          (throw 'found (cdr table)))
      (setq table (cdr table)))))

(ert-deftest cqtbl/cqtbl--get-body ()
  "Test `cqtbl--get-body'."
  (should (equal
           (cqtbl--get-body '(("a" "b") hline ("v" "w") ("x" "y")))
           '(("v" "w") ("x" "y"))))
  (should (equal
           (cqtbl--get-body '(hline ("a" "b") hline ("v" "w") ("x" "y")))
           '(("v" "w") ("x" "y"))))
  (should (equal
           (cqtbl--get-body '(hline ("a1" "b1") ("a" "b") hline ("v" "w") ("x" "y")))
           '(("v" "w") ("x" "y"))))
  )

(defun cqtbl--split-header-and-body (table)
  "Results in cons with header and body."
  ;; split header and body
  (let ((head)
	(body (memq 'hline table)))
    (if (not body)
	(setq body table)
      (setq head table)
      ;; terminate header with nil
      (let ((h head))
	(while (not (eq (cadr h) 'hline))
	  (setq h (cdr h)))
	(setcdr h nil)))
    (cons head body)))

(defun cqtbl--create-merged-table (mastable mascol reftable refcol)
  "Join a master table with a reference table.
MASTABLE is the master table, as a list of lists of cells.
MASCOL is the name of the joining column in the master table.
REFTABLE is the reference table.
REFCOL is the name of the joining column in the reference table.
Returns MASTABLE enriched with material from REFTABLE."
  (let ((result)  ;; result built in reverse order
        (refform) ;; format line of reftable
	(refhead)
	(refhash)
        (masform)
        (masbody)
        (mascols)
        (mergecols)
        (seenkeys))
    (setq mastable (cqtbl--add-column mastable 0 "" ""))
    (setq reftable (cqtbl--add-column reftable 0 "" "x"))
    ;; skip any hline a the top of both tables
    (while (eq (car mastable) 'hline)
      (setq result (cons 'hline result))
      (setq mastable (cdr mastable)))
    (while (eq (car reftable) 'hline)
      (setq reftable (cdr reftable)))
    ;; convert column-names to numbers
    (setq mascol (cqtbl--colname-to-int mascol mastable))
    (setq refcol (cqtbl--colname-to-int refcol reftable))
    (setq mergecols (cons (cons 0  0) ;; added col0
                          (cqtbl--merge-head mastable reftable refcol)))
    ;; convert reference table into fast-lookup hashtable
    (let ((tbl (cqtbl--convert-to-hashtable reftable refcol)))
      (setq refhead (car tbl)
            refhash (cdr tbl)))

    (let ((refcount (length refhead))
          (tbl (cqtbl--split-header-and-body mastable))
          mashead
          mascount)
      (setq refhead (last refhead))
      (setq mashead (car tbl)
            mascount (length mashead)
            mastable (cdr tbl))
      ;; iterate over master table header if any
      ;; and join it with reference table header if any
      (when mashead
        (while (> mascount 1)
          (setq result (cons (car mashead) result))
          (decf mascount)
          (setq mashead (cdr mashead)))
        (setq result
              (cons (cqtbl--merge-append-mas-ref-row
                     (car mashead)
                     (and refhead (car refhead))
                     refcol
                     mergecols)
                    result))
        ))

    ;; create the merged table
    (mapc (lambda (masline)
	    (if (not (listp masline))
		(setq result (cons masline result))
              (unless mascols
                (setq mascols (length masline)))
	      (let ((result0 result))
                (push (nth mascol masline) seenkeys)

                ;; if several ref-lines match, all of them are considered
		(mapc (lambda (refline)
			(setq result
			      (cons
			       (cqtbl--merge-append-mas-ref-row
				masline
				refline
				refcol
                                mergecols)
			       result)))
		      (gethash (nth mascol masline) refhash))
		;; if no ref-line matches, add the non-matching master-line anyway
		(if (eq result result0)
		    (setq result (cons masline result))))))
	  mastable)
    ;; now all rows from reftable which where not used
    (setq result (cons 'hline result))
    (maphash (lambda (key value)
               (unless (member key seenkeys)
                 (let ((mline (make-list mascols "")) (i 0))
                   (setf (nth mascol mline) (or key ""))
                   (mapc (lambda (refline)
                           (setq result
                                 (cons
                                  (cqtbl--merge-append-mas-ref-row
                                   mline
                                   refline
                                   refcol
                                   mergecols
                                   'no-fontify)
                                  result)))
                         (gethash key refhash))
                   )))
             refhash)
    (nreverse result)))

(defconst cqtbl-border-regexp "^[ \t]*|[ \t]"
  "Searching a table (any type) this finds the first line of the table.")

(defconst cqtbl-border-or-name-regexp "^[ \t]*\\(|[ \t]\\|#\\+\\(tbl\\)?name:[ \t]*\\(.*\\)\\)"
  "Searching a table (any type) this finds the first line of the table.")

(defconst cqtbl--org-table-font-lock-regexp "^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)")

;; (defun xx ()
;;   (interactive)
;;   (set (make-local-variable 'org-font-lock-keywords)
;;        (copy-tree org-font-lock-keywords))
;;   (let ((elt (assoc "^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)" org-font-lock-keywords)))
;;     (if elt
;;         (setcdr elt '((1 'org-table prepend))))))

;; (defun cqtbl--replace-table-fontifying ()
;;   "To fontify table entries in a special manner it is neccessary
;; to replace the font-lock-keyword entry for tables, because they
;; overwrite existing fontification."
;;   (when (assoc cqtbl--org-table-font-lock-regexp
;;                org-font-lock-keywords)
;;     (set (make-local-variable 'org-font-lock-keywords)
;;          (copy-tree org-font-lock-keywords))
;;     (let ((elt (assoc cqtbl--org-table-font-lock-regexp
;;                       org-font-lock-keywords)))
;;       (when elt
;;         (setcdr elt '((1 'org-table prepend)))
;;         (setq-local 'font-lock-defaults
;;                        '(org-font-lock-keywords t nil nil backward-paragraph))
;;         ))))

;; *TODO* change fontifing just for current buffer
(defun cqtbl--replace-table-fontifying ()
  "To fontify table entries in a special manner it is neccessary
to replace the font-lock-keyword entry for tables, because they
overwrite existing fontification."
  (when (assoc cqtbl--org-table-font-lock-regexp
               org-font-lock-keywords)
    ;; (set (make-local-variable 'org-font-lock-keywords)
    ;;      (copy-tree org-font-lock-keywords))
    (let ((elt (assoc cqtbl--org-table-font-lock-regexp
                      org-font-lock-keywords)))
      (when elt
        (setcdr elt '((1 'org-table prepend)))
        ;; (setq-local 'font-lock-defaults
        ;;                '(org-font-lock-keywords t nil nil backward-paragraph))
        ))))

;; borrowed from: https://github.com/tbanel/orgtbljoin
(defun cqtbl-insert-elisp-table (table &optional no-align)
  "Insert TABLE in current buffer at point.
TABLE is a list of lists of cells.  The list may contain the
special symbol 'hline to mean an horizontal line."
  (cqtbl--replace-table-fontifying)
  (while table
    (let ((row (car table)))
      (setq table (cdr table))
      (cond ((consp row)
             (insert "|")
             (insert (mapconcat #'identity row "|")))
            ((eq row 'hline)
             (insert "|-"))
            (t (error "Bad row in elisp table")))
      (insert "\n")))
  (when (not no-align)
    ;;(delete-char -1)
    (skip-chars-backward "^|") ;; go back into table
    (org-table-align))
  )

;;; Macros
(defsubst cqtbl-buffer-live-p (buf)
  (and buf (get-buffer buf) (buffer-name (get-buffer buf))))

;; Make a readable representation of the invocation sequence for FUNC-DEF.
;; It would either be a key or M-x something.
(defun cqtbl-format-bindings-of (func-def)
  (let ((desc (car (where-is-internal func-def
				      overriding-local-map
				      nil nil))))
    (if desc
	(key-description desc)
      (format "M-x %s" func-def))))

(defvar cqtbl-reftable-titles-history nil
  "History of ref-table column names.")

(defun cqtbl--memqcar (object list)
  "Return OBJECT if contained in LIST. Compare car of elements."
  (catch 'found
    (mapc (lambda (v)
            (if (cond
                 ((consp v)
                  (equal (car v) object))
                 (t
                  (equal v object)))
                (throw 'found v)))
          list)
    nil))

(ert-deftest cqtbl/cqtbl--memqcar/string ()
    (should (equal (cqtbl--memqcar "B" '(("A" . 1)  "B"))
                   "B")))
(ert-deftest cqtbl/cqtbl--memqcar/cons ()
    (should (equal (cqtbl--memqcar "A" '(("A" . 1)  "B"))
                   (cons "A" 1))))
(ert-deftest cqtbl/cqtbl--memqcar/nil ()
    (should (equal (cqtbl--memqcar "x" '(("A" . 1)  "B"))
                   nil)))

(defun cqtbl--reduce-table (table)
  "Rename or remove columns from TABLE. Return modified table."
  (let* ((split-table (cqtbl--split-table table))
         (preface (nth 0 split-table))
         (ref-title (car (nth 1 split-table)))
         (body (nth 2 split-table))
         refstr nref nconf idx
         columns-to-delete)

    ;; Build string to read, concat each column name. If column has no
    ;; name, write '$<column number>'.
    (setq idx 0)
    ;; TODO [:pos] Angabe der Position möglich
    (setq nref (read-string "Reftable title (old[>new]): "
                            (mapconcat (lambda (v)
                                         (prog1
                                             (if (string= v "") (format "$%d" idx) v)
                                           (incf idx)))
                                       ref-title ",")
                            refstr
                            cqtbl-reftable-titles-history))
    (setq nref (split-string nref ","))

    ;; All elements of reftitle, not contained in nref get cleared,
    ;; all "old>new" get converted to new.
    (mapc (lambda (v)
            (let ((idx (string-match ">" v)))
              (if idx
                  (push (cons (substring-no-properties v nil idx)
                              (substring-no-properties v (1+ idx) nil))
                        nconf)
                (push (cons v v) nconf))
              ))
          nref)

    ;; Replace each column name with new name or 'del if to be deleted.
    (setq idx 0)
    (mapc (lambda (v)
            (let* ((search (if (string= v "") (format "$%d" idx) v))
                   (res (assoc-string search nconf)))
              (if res
                  ;; set new column name
                  (setf (nth idx ref-title) (cdr res))
                ;; mark column to be deleted
                (push idx columns-to-delete)))
            (incf idx))
          ref-title)

    (let ((reduce-row (lambda (r)
                        (if (equal r 'hline)
                            'hline
                          (let (nr (idx 0))
                            (mapc (lambda (e)
                                    (unless (member idx columns-to-delete)
                                      (push e nr))
                                    (incf idx))
                                  r)
                            (nreverse nr)))))
          res-table)
      (mapc (lambda (v)
              (push (funcall reduce-row v) res-table))
            preface)
      ;; ref-title is just one row
      (push (funcall reduce-row ref-title) res-table)
      (mapc (lambda (v)
              (push (funcall reduce-row v) res-table))
            body)
      (nreverse res-table))))

;; (defun xx ()
;;   (interactive)
;;   (let* ((ref-table (org-table-to-lisp)))
;;     (cqtbl--reduce-table ref-table)))

;;;###autoload
(defun cqtbl-merge-tables ()
  "Join given two tables. Master table comes from master-buffer,
 reference table comes from ref-buffer. Key colum is named column
 in both tables.  Result will overwrite master table. For both
 buffers the table where point is will be used. If point is not
 inside the first table found from beginning of buffer will be
 used."
  (interactive)
  (let* ((master-buffer (read-buffer "Buffer A (master) result: "
                                     (ediff-other-buffer "") t))
         column
         ref-column
         ref-buffer
         beg-master end-master
         master-table ref-table
         joined-table
         )

    (if (not (cqtbl-buffer-live-p master-buffer))
      (error "Buffer %S doesn't exist" master-buffer))

    (with-current-buffer master-buffer
      (save-restriction
        ;;(widen)
        (save-excursion
          (if (org-at-table-p)
              (goto-char (org-table-begin))
            (goto-char (point-min)))
          (if (re-search-forward cqtbl-border-regexp nil t)
              (progn
                (goto-char (org-table-begin))
                (setq beg-master (point)
                      end-master (progn (save-excursion (org-table-end))))
                (setq master-table (org-table-to-lisp)))
            (error "No org-table found in buffer %s" (buffer-name master-buffer))
            ))))

    (setq column (cqtbl--query-column "Key column: " master-table cqtbl-default-key))
    (setq ref-column column)

    (setq ref-buffer (read-buffer "Buffer B (ref) for merge in: "
                                  (progn
                                    ;; realign buffers so that two visible bufs will be
                                    ;; at the top
                                    (save-window-excursion (other-window 1))
                                    (ediff-other-buffer master-buffer))
                                  t))

    (if (not (cqtbl-buffer-live-p ref-buffer))
      (error "Buffer %S doesn't exist" ref-buffer))

    (with-current-buffer ref-buffer
      (save-restriction
        ;;(widen)
        (save-excursion
          (if (equal master-buffer ref-buffer)
              (save-restriction
                (widen)
                (message "Goto table. When done, type %s, use %s to abort"
                         (cqtbl-format-bindings-of 'exit-recursive-edit)
                         (cqtbl-format-bindings-of 'abort-recursive-edit))
                (condition-case nil
                    (recursive-edit)
                  (quit
                   (error "Aborted")))
                (if (org-at-table-p)
                    (goto-char (org-table-begin))
                  (error "Not in table"))
                )
            (if (org-at-table-p)
                (goto-char (org-table-begin))
              (goto-char (point-min))))
          (if (re-search-forward cqtbl-border-regexp nil t)
              (progn
                (goto-char (org-table-begin))
                (let ((beg (point))
                      (end (progn (save-excursion (org-table-end)))))
                  (setq ref-table (org-table-to-lisp))))
            (error "No org-table found in buffer %s" ref-buffer)
            ))))


    ;;(setq joined-table (orgtbl--create-table-joined master-table "CFXID" ref-table "CFXID"))

    ;; Konvertierung der Spaltennamen einlesen
    ;; Die Konvertierung dient dazu, dass beim Merge bestimmte Spalten vom Eintrag in die
    ;; neue Tabelle ausgeschlossen werden können oder durch Umbenennung zwar eingetragen, aber
    ;; nicht mit den Spalten der Mastertabelle gemerged werden.
    (setq ref-table (cqtbl--reduce-table ref-table))

    (let* ((split-table (cqtbl--split-table ref-table))
           (ref-title (car (nth 1 split-table))))
      (unless (member column ref-title)
        (setq ref-column (cqtbl--query-column "Key column: " ref-table))
        )
      )

    (setq joined-table (cqtbl--create-merged-table master-table column ref-table ref-column))

    (with-current-buffer master-buffer
      (save-excursion
        ;;(delete-region beg-master end-master)
        ;; Insert new table before old table
        (goto-char beg-master)
        (open-line 2)
        (goto-char beg-master)
        (cqtbl-insert-elisp-table joined-table)
        (org-table-end)
        (forward-line)
        (goto-char beg-master)

        ;; (goto-char end-master)
        ;; (forward-line 1)
        ;; (insert "\n\n")
        ;; (cqtbl-insert-elisp-table joined-table)
        )
      (goto-char beg-master)
      )
    (switch-to-buffer master-buffer)
    ))

(defun cqtbl-split-cu-column ()
  "Split column which is CU structure column."
  (interactive)
  (org-table-check-inside-data-field)
  (let ((col (1- (org-table-current-column)))
        )
    (save-excursion
      (goto-char (org-table-begin))
      (let ((beg (point))
            (end (progn (save-excursion (org-table-end))))
            table new-tbl)
        (setq table (org-table-to-lisp))

        (mapc (lambda (row)

                (if (not (listp row))
                    (setq new-tbl (cons row new-tbl))
                  (let (new-row
                        (i 0))
                    (dolist (elem row)
                      (if (= col i)
                          (let ((entry (split-string elem " "))
                                (cols 0))
                            (dolist (subelem entry)
                              (setq new-row (cons subelem new-row))
                              (incf cols)
                              )
                            (while (< cols 3)
                              (setq new-row (cons "" new-row))
                              (incf cols))
                            )
                        (setq new-row (cons elem new-row))
                        )
                      (incf i)
                      )
                    (setq new-tbl (cons (nreverse new-row)
                                        new-tbl))
                    )
                  )
                )
              table)

        (delete-region beg end)
        (goto-char beg)
        (cqtbl-insert-elisp-table (nreverse new-tbl))
        )
      )
    )
  )

(defun cqtbl-add-x-column ()
  "Add a first column with all lines after headline having a 'x'."
  (interactive)
  (let (beg-master end-master table)
    (save-restriction
      (widen)
      (save-excursion
        (if (org-at-table-p)
            (progn
              (goto-char (org-table-begin))
              (if (re-search-forward cqtbl-border-regexp nil t)
                  (progn
                    (goto-char (org-table-begin))
                    (setq beg-master (point)
                          end-master (progn (save-excursion (org-table-end))))
                    (setq table (org-table-to-lisp)))
                (error "No org-table found")))
              (error "Point not at org-table"))))
    (setq table (cqtbl--add-column table 0 "" "x"))
    (save-restriction
      (save-excursion
        (widen)
        (delete-region beg-master end-master)
        (goto-char beg-master)
        (cqtbl-insert-elisp-table table)
        )
      (goto-char beg-master)
      )))

(defvar cqtbl-add-cr-to-column-columns nil)

;;;###autoload
(defun cqtbl-add-cr-to-column ()
  "Read current table and add string \"(CR)\" to column for all
  reports having \"Change Request\" or \"Development Reuest\" in
  column \"RequestType\"."
  (interactive)
  (let* ((master-buffer (read-buffer "Buffer result: "
                                     (ediff-other-buffer "") t))
         column
         beg-master end-master
         (type-column "RequestType")
         dest-column
         new-table
         master-table)

    (if (not (cqtbl-buffer-live-p master-buffer))
      (error "Buffer %S doesn't exist" master-buffer))

    (with-current-buffer master-buffer
      (save-restriction
        ;;(widen)
        (save-excursion
          (if (org-at-table-p)
              (goto-char (org-table-begin))
            (goto-char (point-min)))
          (if (re-search-forward cqtbl-border-regexp nil t)
              (progn
                (goto-char (org-table-begin))
                (setq beg-master (point)
                      end-master (progn (save-excursion (org-table-end))))
                (setq master-table (org-table-to-lisp)))
            (error "No org-table found in buffer %s" (buffer-name master-buffer))
            ))))

    (let* ((title (cqtbl--get-title master-table)))
      (setq dest-column (completing-read
                         "Destination column: "
                         title
                         nil nil "Sum" cqtbl-add-cr-to-column-columns))

      (unless (member type-column title)
        (setq type-column (cqtbl--query-column "RequestType column: " master-table))
        )
      (when (not (member type-column title))
        (user-error "Column %s not available" type-column))
      (unless (member dest-column title)
        (setq dest-column (cqtbl--query-column "Destination column: " master-table))
        )
      (when (not (member dest-column title))
        ;; Add new column
        (setq master-table (cqtbl--add-column master-table 0 dest-column)))

      ;; Spaltenname in Spaltennummer konvertieren
      (setq type-column (cqtbl--colname-to-int type-column master-table))
      (setq dest-column (cqtbl--colname-to-int dest-column master-table))

      ;; Konvertierung
      (let ((body (cqtbl--get-body master-table))
            (table master-table)
            )
        (while (not (eq table body))
          (push (car table) new-table)
          (setq table (cdr table)))
        (dolist (elt body)
          (cond
           ((eq elt 'hline)
            (push elt new-table))
           (t
            (let ((content (nth type-column elt))
                  (res (nth dest-column elt)))
              (when (or (string= content "Change Request, internal")
                        (string= content "Change Request, external")
                        (string= content "Development Request"))
                (unless (string-match-p "(CR)" res)
                  (setf (nth dest-column elt) (concat res "(CR)"))))
              (push elt new-table)))))
        (setq new-table (nreverse new-table)))

      (with-current-buffer master-buffer
        (save-excursion
          ;;(delete-region beg-master end-master)
          ;; Insert new table before old table
          (goto-char beg-master)
          (open-line 2)
          (goto-char beg-master)
          (cqtbl-insert-elisp-table new-table)
          (org-table-end)
          (forward-line)
          (goto-char beg-master)

          ;; (goto-char end-master)
          ;; (forward-line 1)
          ;; (insert "\n\n")
          ;; (cqtbl-insert-elisp-table joined-table)
          )
        (goto-char beg-master)
        ))
    (switch-to-buffer master-buffer)
    ))

;; ================================================================================

;; from org-table.el
(eval-when-compile
  (require 'org-table))
(defun my-org-table-move-column (&optional left)
  "Move the current column to the right.  With arg LEFT, move to
the left. Let text-properties as is."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field)
  (let* ((col (org-table-current-column))
	 (col1 (if left (1- col) col))
	 (colpos (if left (1- col) (1+ col)))
	 (beg (org-table-begin))
	 (end (copy-marker (org-table-end))))
    (when (and left (= col 1))
      (user-error "Cannot move column further left"))
    (when (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
      (user-error "Cannot move column further right"))
    (org-table-save-field
     (goto-char beg)
     (while (< (point) end)
       (unless (org-at-table-hline-p)
	 (org-table-goto-column col1 t)
	 (when (looking-at "|\\([^|\n]+\\)|\\([^|\n]+\\)|")
           (transpose-regions
            (match-beginning 1) (match-end 1)
            (match-beginning 2) (match-end 2))
           ;;(replace-match "|\\2|\\1|")
           ))
       (forward-line)))
    (set-marker end nil)
    (org-table-goto-column colpos)
    (org-table-align)
    (when (or (not org-table-fix-formulas-confirm)
	      (funcall org-table-fix-formulas-confirm "Fix formulas? "))
      (org-table-fix-formulas
       "$" (list (cons (number-to-string col) (number-to-string colpos))
		 (cons (number-to-string colpos) (number-to-string col))))
      (org-table-fix-formulas
       "$LR" (list (cons (number-to-string col) (number-to-string colpos))
		   (cons (number-to-string colpos) (number-to-string col)))))))

(advice-add 'org-table-move-column :override #'my-org-table-move-column)

;; ================================================================================

(defun cq-scan-table-in-region ()
  "Search for available tables in the current file."
  (interactive)
  (let (beg end table)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward cq-table-border-regexp nil t)
          (progn
            (goto-char (org-table-begin))
            (setq table (org-table-to-lisp)))
        (error "No org-table found in buffer %s" (buffer-name))
        )
      table)))

(defun cq-scan-not-closed-tables ()
  (let (all-tables)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (while (re-search-forward "Bekannte nicht geschlossene Problemreporte" nil t)
          (let ((beg (point))
                (end)
                table)
            (if (re-search-forward "Tabelle " nil t)
                (progn
                  (setq end (point))
                  (save-excursion
                    (save-restriction
                      (narrow-to-region beg end)
                      (setq table (cq-scan-table-in-region))
                      (if table
                          (push table all-tables)))))
              )))))
    (nreverse all-tables)))

(defun cq-scan-pr-tables ()
  "Scan current buffer for all CFX Numbers in tables following the headline

 \"Bekannte nicht geschlossene Problemreporte\"

Build a tabel of the result in a new buffer.
"
  (interactive)
  (let ((all-tables (cq-scan-not-closed-tables)))
    (with-current-buffer (get-buffer-create "*cq-prs*")
      (erase-buffer)
      (let ((cfx-col 0)
            (type-col 1)
            (headline-col 2))
        (insert "|| <10> |\n"
                "|CFXID|RequestType|Headline\n"
                "|-\n")
        (dolist (table all-tables)
          (let* ((tb (cqtbl--split-table table))
                 (preface (car (nth 0 tb)))
                 (title (nth 1 tb))
                 (body (nth 2 tb))
                 tb-cfx-col tb-type-col tb-headline-col)
            (if (and title body)
                (progn
                  ;; hier preface nicht title, da hinter der headline noch eine Zeile steht
                  (setq tb-cfx-col 0
                        tb-type-col 1
                        tb-headline-col 2)
                  (dolist (row body)
                    (cond
                     ((eq row 'hline))
                     (t
                      (if (and (> (length (nth tb-cfx-col row)) 0)
                               (not (string-match "-" (nth tb-cfx-col row))))
                          (let ((cfx (nth tb-cfx-col row)))
                            (if (string-match "\\(SP\\\\_P[0-9]+\\)" cfx)
                                (setq cfx (match-string 1 cfx)))
                            (setq cfx (replace-regexp-in-string "," "" cfx))
                            (setq cfx (replace-regexp-in-string "\\\\_" "_" cfx))
                            (insert (format "| %s|%s|%s\n"
                                            cfx
                                            (nth tb-type-col row)
                                            (nth tb-headline-col row)))
                            )
                        )
                      ))
                    )
                  )))))
      (goto-char (point-min))
      (org-mode)
      (org-table-align)
      (switch-to-buffer "*cq-prs*"))
    all-tables))

;; ================================================================================

(provide 'cqtbl)

