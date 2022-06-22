;; -*- mode: lisp-interaction; coding: utf-8-unix -*-
;;; tvfile.el --- Stefan's tvfile Minor Mode

;; Faltender tvfile-minor-mode
;; Anleihen aus org-mode entnommen.

;; Copyright (C) 2005-2014 Stefan-W. Hahn
;; Author: Stefan-W. Hahn <stefan.hahn@s-hahn.de>

;; Requires:
;; - use-package

;;; Code:
(eval-when-compile
  (require 'use-package))
(use-package outline)
(use-package imenu)

;; 1436504237.455500 7 [emu_ACC_20]Unilink> 01 01 ff a1 86 01 00 03 00 00 00 00 00 00 00 53 74 61 72 74 75 70 66 75 6e 6b 74 69 6f 6e 20 25 75 20 66 72 6f 6d 20 42 41 42 20 23 25 64 20 27 4e 6f 52 61 6d 70 72 66 2e 2d 6e 6f 6e 65 2d 27 20 2e 2e 2e 00 

(defconst tvfile-tft-re (concat
			 "\\(?:"
                         "\\(?:\\(?:[123]\\.[0-9]+<[0-9a-fx]+> \\)\\|" ;; Televist tv header
                         "\\(?:[0-9]+ [123]\\.[0-9]+:[0-9]+<[0-9a-fx]+> \\)\\|" ;; Uldisplay header
                         "\\(?:[0-9]+\\.[0-9]+ [0-7] \\[.*?\\]<[0-9a-fx]+> \\)\\)" ;; TOPOLOGIE header
                         "\\(?:"
			 "\\(?:<TFT> Ausfuehren von '\\(.*\\)['@\\]\\)\\|" ;; match 1: Testfallname, Teststart
			 "\\(?:<TFT> \\.\\.\\. \\(" ;; match 2: Testabschluss
			 "\\(<TFT> Test war erfolgreich\\)\\|" ;; match 3: positiver Testabschluss
			 "\\(<TFT> Test ist fehlgeschlagen\\)\\|" ;; match 4: fehlgeschlagener Test
			 "\\(Review der Testausgaben notwendig\\)\\|" ;; match 5: Testergebnis unklar
			 "\\(Test in dieser Umgebung nicht relevant\\)\\|" ;; match 6: nicht relevanter Test
			 "\\(manuelle Eingabe des Testresultats erforderlich\\)" ;; match 7: manuelle Eingabe
                         "\\)\\)\\|" ;; end-of "<TFT> \\."
			 "\\(.*?" ;; match 8 Teiltes
                         "\\(Teiltest war erfolgreich\\)\\|" ;; match 9: Teiltest erfolgreich
			 "\\(?:.*?\\(Teiltest ist fehlgeschlagen\\)\\)\\)" ;; match 10 Teiltest fehlgeschlagen
                         "\\)\\)"
                         )
  "Testframe Start- und Endemarken einer Testausführung

  match 1: Testfallname Teststart
  match 2: Testabschluss
  match 3: erfolgreicher Testabschluss
  match 4: fehlgeschlagener Test
  match 5: Testergebnis unklar
  match 6: nicht relevanter Test
  match 7: manuelle Ergebniseingabe erforderlich
  match 8: Teiltest
  match 9: Teiltest erfolgreich
  match 10: Teiltest fehlgeschlagen")

(defun tvfile-scan ()
  "Ergebniss von Testläufen einer tv._tv Datei analysieren. Liefert
eine Liste der Ergebnisse. Jedes Element hat folgenden Aufbau:

  (TESTNAME TESTRESULT TESTRESULTSTRING START END TEILTESTS)

  TESTNAME Testfallstring
  TESTRESULT
        0 Test nicht ausgeführt
        1 Test nicht erfolgreich
        2 Test erfolgreich
        3 Test irrelevant
        4 Testergebnis manuell
        5 Testergebnis unklar
        6 Teiltest erfolgreich
        7 Teiltest fehlgeschlagen
  TESTRESULTSTRING Ergebnisstring
  START END Position in buffer
  TEILTESTS Liste mit Positionen der Teiltestergebnisse"
  (interactive)
  (let (testname wait-for-test-end rc result teiltests start end)
    (save-excursion
      (goto-char (point-min))
      (widen)
      (while (re-search-forward tvfile-tft-re nil t)
	(cond
	 ((match-end 1)
	  (when wait-for-test-end
	    (message "Test '%s' nicht abgeschlossen" testname))
	  (setq testname (match-string 1)
		start (point-at-bol)
		teiltests nil
		wait-for-test-end t))
	 ((match-end 8)
	  (if (not wait-for-test-end)
	      (message "Teiltest ohne Testanfang")
	    (message "Teiltest von '%s': Ergebnis: '%s'" testname (match-string 8))
	    (push (point-at-bol) teiltests)
	    ))
	 (t
	  (if (not wait-for-test-end)
	      (progn
		(message "Testabschluss ohne Testanfang")
		(push (list testname 0 (match-string 2) (point-at-bol) (point-at-bol)) result))
	    (message "Testabschluss von '%s': Ergebnis: '%s'" testname (match-string 2))
	    (setq rc (cond
		      ((match-string 3) 2)
		      ((match-string 4) 1)
		      ((match-string 5) 5)
		      ((match-string 6) 3)
		      ((match-string 7) 4)
		      ((match-string 7) 4)
		      (t 0)))
	    (push (list testname rc (match-string 2) start (point-at-eol) (nreverse teiltests)) result))))))
    (setq result (nreverse result))
    ;;(setq result (sort result (lambda (e1 e2)
    ;;				(string< (car e1) (car e2)))))
    result))

(defmacro tvfile-point-at-bol (pos)
  `(save-excursion
     (goto-char ,pos)
     (point-at-bol)))

(defmacro tvfile-point-at-eol (pos)
  `(save-excursion
     (goto-char ,pos)
     (point-at-eol)))

(defun xx ()
  (interactive)
  (let* ((result (tvfile-scan))
	 (s (point-min))
	 (elem (car result))
	 teiltests teil e p)
    (when result
      (remove-text-properties (point-min) (point-max) '(:tv-heading nil))
      (while (setq elem (pop result))
	(setq e (1- (tvfile-point-at-bol (nth 3 elem))))
	(when (< s e)
	  (setq p (tvfile-point-at-bol s))
	  (put-text-property p s :tv-heading (cons s e))
	  (outline-flag-region s e t))

	(setq s (tvfile-point-at-eol (nth 3 elem)))
	;; Alle Teiltests sichtbar
	(setq teiltests (nth 5 elem))
	(while (setq teil (pop teiltests))
	  (setq e (1- (tvfile-point-at-bol teil)))
	  (setq p (tvfile-point-at-bol s))
	  (put-text-property p s :tv-heading (cons s e))
	  (outline-flag-region s e t)
	  (setq s (tvfile-point-at-eol teil)))

	(setq e (1- (tvfile-point-at-bol (nth 4 elem))))
	(setq p (tvfile-point-at-bol s))
	(put-text-property p s :tv-heading (cons s e))
	(outline-flag-region s e t)
	(setq s (tvfile-point-at-eol (nth 4 elem))))
      (when (< s (point-max))
	(setq p (tvfile-point-at-bol s))
	(put-text-property p s :tv-heading (cons s (point-max)))
	(outline-flag-region s (point-max) t))
      )))

(defun xtvfile-back-to-heading ()
  (interactive)
  (let ((found nil))
    (while (and (not found) (not (bobp)))
      (let ((plist (text-properties-at (point)))
	    (next-change
	     (or (previous-property-change (point) (current-buffer))
		 (point-min))))
	(if (not (get-text-property (point) :tv-heading))
	    (goto-char next-change)
	  (message "found textproperty at %d" (point))
	  (setq found t)
	  (beginning-of-line))))))

(defun tvfile-at-heading-p ()
  (interactive)
  (let ((p (get-text-property (point) :tv-heading))
	)
    (and p (= (point-at-bol) p))))

(defvar tvfile-cycle-global-status nil
  "Global state of tvfile-cycle-global,  will be buffer-local if set in anyway.")
(make-variable-buffer-local 'tvfile-cycle-global-status)
(defvar tvfile-cycle-subtree-status nil)
(make-variable-buffer-local 'tvfile-cycle-subtree-status)

(defun tvfile-cycle-local ()
  "Do the local cycling action with `tvfile-outline-regexp'."
  (interactive)
  (let* ((pos (point)))
    (when (save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      (tvfile-cycle-internal-local))))

(defvar tvfile-cycle-skip-children-state-if-no-children t
  "Non-nil means skip CHILDREN state in entries that don't have any."
)

(defun tvfile-cycle-internal-local ()
  "Do the local cycling action."
  ;(org-back-to-heading)
  (let ((goal-column 0) eoh eol eos level has-children children-skipped)
    ;; First, some boundaries
    (save-excursion
      ;(org-back-to-heading)
      (setq level (funcall outline-level))
      (save-excursion
	(beginning-of-line 2)
	(while (and (not (eobp)) ;; this is like `next-line'
		    (get-char-property (1- (point)) 'invisible))
	  (goto-char (next-single-char-property-change (point) 'invisible))
	  (and (eolp) (beginning-of-line 2)))
	(setq eol (point)))
      (outline-end-of-heading)   (setq eoh (point))
      (save-excursion
	(outline-next-heading)
	(setq has-children (and (outline-on-heading-p t)
				(> (funcall outline-level) level))))
      (tvfile-end-of-subtree t)
      (unless (eobp)
	(skip-chars-forward " \t\n")
	(beginning-of-line 1) ; in case this is an item
	)
      (setq eos (if (eobp) (point) (1- (point)))))
    ;; Find out what to do next and set `this-command'
    (cond
     ((= eos eoh)
      ;; Nothing is hidden behind this heading
      ;(run-hook-with-args 'org-pre-cycle-hook 'empty)
      (message "EMPTY ENTRY")
      (setq tvfile-cycle-subtree-status nil)
      (save-excursion
	(goto-char eos)
	(outline-next-heading)
	;(if (org-invisible-p) (org-flag-heading nil))
	))
     ((and (or (>= eol eos)
	       (not (string-match "\\S-" (buffer-substring eol eos))))
	   (or has-children
	       (not (setq children-skipped
			  tvfile-cycle-skip-children-state-if-no-children))))
      ;; Entire subtree is hidden in one line: children view
      ;(run-hook-with-args 'org-pre-cycle-hook 'children)
      (org-show-entry)
      (show-children)
      (message "CHILDREN")
      (save-excursion
	(goto-char eos)
	(outline-next-heading)
	;(if (org-invisible-p) (org-flag-heading nil))
	)
      (setq tvfile-cycle-subtree-status 'children)
      ;(run-hook-with-args 'org-cycle-hook 'children)
      )
     ((or children-skipped
	  (and (eq last-command this-command)
	       (eq tvfile-cycle-subtree-status 'children)))
      ;; We just showed the children, or no children are there,
      ;; now show everything.
      (run-hook-with-args 'org-pre-cycle-hook 'subtree)
      (org-show-subtree)
      (message (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
      (setq tvfile-cycle-subtree-status 'subtree)
;      (run-hook-with-args 'org-cycle-hook 'subtree)
      )
     (t
      ;; Default action: hide the subtree.
      (run-hook-with-args 'org-pre-cycle-hook 'folded)
      (hide-subtree)
      (message "FOLDED")
      (setq tvfile-cycle-subtree-status 'folded)
      ;(run-hook-with-args 'org-cycle-hook 'folded)
      ))))

(defun tvfile-cycle-global ()
  "Do the global cycling action."
  (interactive)
  (cond
   ((eq tvfile-cycle-global-status 'overview)
    ;; We just showed the table of contents - now show everything
    ;(run-hook-with-args 'org-pre-cycle-hook 'all)
    (message "Toggle visibility...")
    (show-all)
    (message "SHOW ALL")
    (setq tvfile-cycle-global-status 'all)
    ;(run-hook-with-args 'org-cycle-hook 'all)
)

   (t
    ;; Default action: go to overview
    ;(run-hook-with-args 'org-pre-cycle-hook 'overview)
    (message "Toggle visibility...")
    (tvfile-overview)
    (message "OVERVIEW")
    (setq tvfile-cycle-global-status 'overview)
    ;(run-hook-with-args 'org-cycle-hook 'overview)
)))

(defun tvfile-cycle (&optional arg)
  "TAB-action and visibility cycling for tvfile-mode.

This is the command invoked in tvfile-mode by the TAB key.  Its main purpose
is outline visibility cycling, but it also invokes other actions
in special contexts.

- When this function is called with a prefix argument, rotate the entire
  buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.
  When called with two `C-u C-u' prefixes, switch to the startup visibility,
  determined by the variable `org-startup-folded', and by any VISIBILITY
  properties in the buffer.
  When called with three `C-u C-u C-u' prefixed, show the entire buffer,
  including any drawers.

- When inside a table, re-align the table and move to the next field.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
  If there is no subtree, switch directly from CHILDREN to FOLDED.

- When point is at the beginning of an empty headline and the variable
  `org-cycle-level-after-item/entry-creation' is set, cycle the level
  of the headline by demoting and promoting it to likely levels.  This
  speeds up creation document structure by pressing TAB once or several
  times right after creating a new headline.

- When there is a numeric prefix, go up to a heading with level ARG, do
  a `show-subtree' and return to the previous cursor position.  If ARG
  is negative, go up that many levels.

- When point is not at the beginning of a headline, execute the global
  binding for TAB, which is re-indenting the line.  See the option
  `org-cycle-emulate-tab' for details.

- Special case: if point is at the beginning of the buffer and there is
  no headline in line 1, this function will act as if called with prefix arg
  (C-u TAB, same as S-TAB) also when called without prefix arg.
  But only if also the variable `org-cycle-global-at-bob' is t."
  (interactive "P")
  (let* ((pos (point)))

    (if (equal arg '(4))
	;; special case:  use global cycling
	(setq arg t))

    (cond

     ;; Global cycling: delegate to `org-cycle-internal-global'.
     ((eq arg t) (tvfile-cycle-global))

     ;; At an item/headline: delegate to `org-cycle-internal-local'.
     ((save-excursion (beginning-of-line 1)
		      (looking-at outline-regexp))
      (tvfile-cycle-internal-local))

     ;; From there: TAB emulation and template completion.
     (buffer-read-only (outline-back-to-heading))

     ((or (not (bolp))
	  (not (looking-at outline-regexp)))
      (call-interactively (global-key-binding "\t")))

     (t (save-excursion
	  (outline-back-to-heading)
	  (tvfile-cycle))))))

(defun tvfile-overview ()
  "Switch to overview mode, showing only top-level headlines.
Really, this shows all headlines with level equal or greater than the level
of the first headline in the buffer.  This is important, because if the
first headline is not level one, then (hide-sublevels 1) gives confusing
results."
  (interactive)
  (save-excursion
    (let* (outline-view-change-hook
           (beg (progn
                  (goto-char (point-min))
                  ;; Skip the prelude, if any.
                  (unless (outline-on-heading-p t) (outline-next-heading))
                  (point)))
           (end (progn
                  (goto-char (point-max))
                  ;; Keep empty last line, if available.
                  (if (bolp) (1- (point)) (point)))))
      (if (< end beg)
	  (setq beg (prog1 end (setq end beg))))
      ;; First hide everything.
      (outline-flag-region beg end t)
      ;; Then unhide the top level headers.
      (outline-map-region
       (lambda ()
	 (outline-show-heading))
       beg end)))
  (run-hooks 'outline-view-change-hook))

  ;; (let ((level (save-excursion
  ;; 		 (goto-char (point-min))
  ;; 		 (if (re-search-forward (concat "^" outline-regexp) nil t)
  ;; 		     (progn
  ;; 		       (goto-char (match-beginning 0))
  ;; 		       2))))) ;(funcall outline-level))))))
  ;;   (and level (hide-sublevels level))))


(defun tvfile-end-of-subtree (&optional invisible-OK to-heading)
  ;; This contains an exact copy of the original function, but it uses
  ;; `tvfile-back-to-heading', to make it work also in invisible
  ;; trees.  And is uses an invisible-OK argument.
  ;; Under Emacs this is not needed, but the old outline.el needs this fix.
  ;; Furthermore, when used inside Org, finding the end of a large subtree
  ;; with many children and grandchildren etc, this can be much faster
  ;; than the outline version.
  (tvfile-back-to-heading invisible-OK)
  (outline-next-heading)
  (unless to-heading
    (if (memq (preceding-char) '(?\n ?\^M))
	(progn
	  ;; Go to end of line before heading
	  (forward-char -1)
	  (if (memq (preceding-char) '(?\n ?\^M))
	      ;; leave blank line before heading
	      (forward-char -1)))))
  (point))

(defun tvfile-back-to-heading (&optional invisible-ok)
  "Call `outline-back-to-heading', but provide a better error message."
  (condition-case nil
      (outline-back-to-heading invisible-ok)
    (error (error "Before first headline at position %d in buffer %s"
		  (point) (current-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup tvfile-faces nil
  "Faces in tvfile-minor-mode."
  :tag "tvfile faces"
  :group 'tvfile)

(defface tv-channel1
  '((t (:background "yellow")))
  "Face for televist channel1."
  :group 'tvfile)

(defface tv-channel2
  '((t (:background "lightblue")))
  "Face for televist channel2."
  :group 'tvfile)

(defface tv-channel3
  '((t (:background "green")))
  "Face for televist channel3."
  :group 'tvfile)

(defface tv-test-negative
  '((((class color) (min-colors 8)) (:background "red"))
    (t (:inverse-video t :weight bold)))
  "Font Lock mode face used to highlight warnings."
  :group 'tvfile)

(defface tv-test-positive
  '((((class color) (min-colors 8)) (:background "green"))
    (t (:weight bold)))
  "Font Lock mode face used to highlight positive."
  :group 'tvfile)

(defface tv-test-undecided
  '((((class color) (min-colors 8)) (:background "orange"))
    (t (:weight bold)))
  "Font Lock mode face used to highlight undecided."
  :group 'tvfile)

(defface tv-test-start
  '((t (:background "yellow")))
  "Face for start of test."
  :group 'tvfile)

(defface tv-test-pausey
  '((t (:foreground "red")))
  "Face for pausey of test."
  :group 'tvfile)

(defface tv-rpc-color
  '((t (:foreground "grey70")))
  "Face for RPC commands."
  :group 'tvfile)

(defface tv-other-orbiter
  '((t (:foreground "grey50")))
  "Face for not local ORBITER/PATHFINDER."
  :group 'tvfile)

(defface tv-testlink-orbiter
  '((t (:foreground "gold")))
  "Face for Testlink debugdata."
  :group 'tvfile)

(defconst tvfile-sum-positive-re "\\.\\.\\. Test war erfolgreich")
(defconst tvfile-sub-positive-re "Teiltest war erfolgreich")
(defconst tvfile-positive-re (concat "\\(" tvfile-sum-positive-re "\\|" tvfile-sub-positive-re "\\)"))

(defconst tvfile-sum-negative-re "\\.\\.\\. Test ist fehlgeschlagen")
(defconst tvfile-sub-negative-re "Teiltest ist fehlgeschlagen")
(defconst tvfile-negative-re (concat "\\(" tvfile-sum-negative-re "\\|" tvfile-sub-negative-re "\\)"))

(defconst tvfile-testresult-re (concat "\\(" tvfile-positive-re "\\|" tvfile-negative-re "\\)"))

(defconst tvfile-undecided-re (concat
			 "\\.\\.\\. \\("
			 "\\(Review der Testausgaben notwendig!\\)\\|"
			 "\\(Test in dieser Umgebung nicht relevant\\.\\)\\|"
			 "\\(manuelle Eingabe des Testresultats erforderlich\\.\\)\\)"))

(defconst tvfile-font-lock-alist
  `(
    ;; Televist Header
    ("\\(^1\\.[^<]+<[0-9a-fx]+> \\)" 1 'tv-channel1)
    ("\\(^2\\.[^<]+<[0-9a-fx]+> \\)" 1 'tv-channel2)
    ("\\(^3\\.[^<]+<[0-9a-fx]+> \\)" 1 'tv-channel3)

    ;; Topologie
    ("\\(^[0-9]+\\.[0-9]+ [0-9]+ \\[.*?\\]\\(<[0-9a-fx]+>\\|Unilink>\\) \\)" 1 'tv-channel1)

    ;; Uldisplay
    ("\\(^[0-9]+ 0\\.[0-9]+:[0-9]+<[0-9a-fx]+> \\)" 1 'tv-channel1)
    ("\\(^[0-9]+ 1\\.[0-9]+:[0-9]+<[0-9a-fx]+> \\)" 1 'tv-channel2)
    ("\\(^[0-9]+ 2\\.[0-9]+:[0-9]+<[0-9a-fx]+> \\)" 1 'tv-channel3)

    (,tvfile-positive-re 1 'tv-test-positive)
    (,tvfile-negative-re 1 'tv-test-negative)
    (,tvfile-undecided-re 1 'tv-test-undecided)

    ("\\(\\(<TFT> Ausfuehren von\\|\\[BEGIN Test\\).*$\\)" 1 'tv-test-start)
    ("\\(ilTest> pauseY>\\|pauseY timeout\\|\\*\\*\\* error \\*\\*\\*.*$\\)" 1 'tv-test-pausey)

    ("\\(\\(RPC.?-\\(snd\\|rcv\\)\\|rpc_\\).*$\\)" 1 'tv-rpc-color)
    ("\\(ilOrbiter[1-9]>\\|ILORBITER[1-9]-\\|pathfinder[1-9]>.*$\\)" 1 'tv-other-orbiter)
    ("\\(ilTestLink>.*\\)" 1 'tv-testlink-orbiter)

    )
  "Font lock alist.")

(defun tvfile-next-negative-result ()
  "Search next negative testresult."
  (interactive)
  (when (search-forward-regexp tvfile-negative-re nil t)
      (isearch-filter-visible (match-beginning 0) (match-end 0))
    ))

(defun tvfile-prev-negative-result ()
  "Search previous negative testresult."
  (interactive)
  (when (search-backward-regexp tvfile-negative-re nil t)
      (isearch-filter-visible (match-beginning 0) (match-end 0))
    ))

(defun tvfile-next-undecided-result ()
  "Search next undecided testresult."
  (interactive)
  (when (search-forward-regexp tvfile-undecided-re nil t)
      (isearch-filter-visible (match-beginning 0) (match-end 0))
    ))

(defun tvfile-next-result ()
  "Search next testresult."
  (interactive)
  (when (search-forward-regexp tvfile-testresult-re nil t)
      (isearch-filter-visible (match-beginning 0) (match-end 0))
    ))

(defun tvfile-prev-result ()
  "Search previous testresult."
  (interactive)
  (when (search-backward-regexp tvfile-testresult-re nil t)
      (isearch-filter-visible (match-beginning 0) (match-end 0))
    ))

(defun tvfile-indirect (channel)
  (let ((buffer (current-buffer))
	bname)
    (when (buffer-live-p buffer)
      (setq bname (concat "*" (buffer-name buffer) "- channel " (number-to-string channel) "*"))
      (condition-case nil
	  (make-indirect-buffer buffer bname 'clone)))))

(defvar tvfile-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" " " st)
    st)
  "Syntax table used while in tvfile mode.")

;; (defconst tvfile-imenu-generic-expression
;;   `((nil ,(concat "^\\s-*" nasm-label-regexp) 1)
;;     (nil ,(concat (nasm--opt '("%define" "%macro"))
;;                   "\\s-+\\([a-zA-Z0-9_$#@~.?]+\\)") 2))
;;   "Expressions for `imenu-generic-expression'.")

;;(nil "TCT * LDT=\\(0x84c8\\) * stack at lin addr \\(0x00269f6c\\) with limit=\\(0x1fff\\) 

;; (defconst tvfile-task-regexp
;;   "====> Taskframe starten (COSPAS PID=\\([0-9]+\\)(dez), Task-LDT=\\(0x[0-9a-fA-F]+\\)) \\[Taskname\\]:\w*[^\000]+?[0-9.<x>a-fA-F]+ \\(.+\\)? ")
;; (defun tvfile-find-task ()
;;   (if (re-search-backward tvfile-task-regexp nil t)
;;       (let ((res (format-string
;;                   "%s PID=%d LDT=0x%04x"
;;                   (match-string 3) (match-string 1) (match-string 2)))
;;             )
;;         )
;;     )
;;   )

(defconst tvfile-imenu-generic-expression
  '(("Task starten"
     "====> Taskframe starten (COSPAS PID=\\([0-9]+\\)(dez), Task-LDT=\\(0x[0-9a-fA-F]+\\)) \\[Taskname\\]:\w*[^\000]+?[0-9.<x>a-fA-F]+ \\(.+\\)? "
     3)
   ("Test gefunden" "<TFT> Test gefunden: '\\(.*\\)'"
    1)
   ("Ausführen" "<TFT> Ausfuehren von '\\(.*\\)'"
    1))
  "Expressions for `imenu-generic-expression'.")

;; (use-package helm-imenu)
;; (defun tvfile-imenu-transformer (candidates)
;;   (cl-loop for (k . v) in candidates
;;         for types = (or (helm-imenu--get-prop k)
;;                         (list "Function" k))
;;         for bufname = (buffer-name (marker-buffer v))
;;         for disp1 = (mapconcat
;;                      (lambda (x)
;;                        (propertize
;;                         x 'face (cond ((string= x "Task starten")
;;                                        'font-lock-variable-name-face)
;;                                       ((string= x "Test gefunden")
;;                                        'font-lock-function-name-face)
;;                                       ((string= x "Ausführen")
;;                                        'font-lock-type-face))))
;;                      types helm-imenu-delimiter)
;;         for disp = (propertize disp1 'help-echo bufname)
;;         collect
;;         (cons disp (cons k v))))
;; (defclass tvfile-imenu-source (helm-source-sync)
;;   ((candidates :initform 'helm-imenu-candidates)
;;    (candidate-transformer :initform 'tvfile-imenu-transformer)
;;    (persistent-action :initform 'helm-imenu-persistent-action)
;;    (persistent-help :initform "Show this entry")
;;    (keymap :initform helm-imenu-map)
;;    (mode-line :initform helm-imenu-mode-line)
;;    (action :initform 'helm-imenu-action)))
;; (defun tvfile-helm-imenu ()
;;   "Preconfigured `helm' for `imenu' in tvfile."
;;   (interactive)
;;   (unless helm-source-imenu
;;     (setq helm-source-imenu
;;           (helm-make-source "Imenu" 'tvfile-imenu-source
;;             :fuzzy-match helm-imenu-fuzzy-match)))
;;   (let ((imenu-auto-rescan t)
;;         (str (thing-at-point 'symbol))
;;         (helm-execute-action-at-once-if-one
;;          helm-imenu-execute-action-at-once-if-one))
;;     (helm :sources 'helm-source-imenu
;;           :default (list (concat "\\_<" str "\\_>") str)
;;           :buffer "*helm imenu*")))

(defvar tvfile-minor-mode-hook nil)
(define-minor-mode tvfile-minor-mode
  "Toggle tvfile-minor-mode for televist recordings (*._tv).
With no argument, this command toggles the mode.
Non-null prefix turns on the mode.
Null  prefix argument turns off the mode.

M-n search next negative testresult
M-p search previous negative testresult
M-u search next undecided testresult

C-M-n search next testresult
C-M-p search previous testresult

C-. toggles global visibility
C-, toggles local  visibility."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " tv"
  ;; The minor mode bindings.
  '(
    ("\M-n" . tvfile-next-negative-result)
    ("\M-p" . tvfile-prev-negative-result)
    ("\M-u" . tvfile-next-undecided-result)
    ("\C-\M-n" . tvfile-next-result)
    ("\C-\M-p" . tvfile-prev-result)
;;    ("\C-c m" . tvfile-helm-imenu)
    ("\C-i" . tvfile-cycle)
    ([tab] . tvfile-cycle)
    ([S-iso-lefttab] . tvfile-cycle-global)
    ([S-tab] . tvfile-cycle-global)
    )
  :group 'private
  (if tvfile-minor-mode
      ;; turn on tvfile-minormode
      (progn
	(toggle-truncate-lines 1)
	(auto-save-mode 0)
	(buffer-disable-undo)
	(make-local-variable 'outline-regexp)
	(setq outline-regexp tvfile-tft-re)
	(add-to-invisibility-spec (cons 'outline t))

        (font-lock-add-keywords nil tvfile-font-lock-alist)
	;;(font-lock-ensure (window-start) (window-end))
        ;;(font-lock-fontify-region (window-start) (window-end))
        (font-lock-fontify-buffer)

        (set (make-local-variable 'imenu-case-fold-search) nil)
        (setq imenu-generic-expression tvfile-imenu-generic-expression)

        (make-variable-buffer-local 'comment-start)
        (make-variable-buffer-local 'comment-end)
        (setq comment-start ";"
              comment-end "")

        ;; move " to syntax class of whitespace. (Otherwise " will be in class of comment open, end)
        (set-syntax-table (make-syntax-table tvfile-mode-syntax-table))
	)
    ;; turn off tvfile-minormode
    (hi-lock-mode 0)
    (remove-from-invisibility-spec (cons 'outline  t)))
  )

;;(add-to-list 'auto-mode-alist '(".*\\._tv$" . (lambda() (tvfile-minor-mode t))))
;;(add-to-list 'auto-mode-alist '(".*\\._tvf$" . (lambda() (tvfile-minor-mode t))))

;;;; ================================================================================

(require 'compile)

(defface tv-compile
  '((((class color) (background light))
     (:foreground "blue" :background "yellow"))
    (((class color) (background dark)) (:foreground "red" :background "white")))
  "Face for annotation text."
  :group 'tvfile)

(defface tv-compile-wait
  '((((class color) (background light))
     (:inherit tv-compile :foreground "red1"))
    (((class color) (background dark)) (:foreground "red" :background "white")))
  "Face for annotation text."
  :group 'tvfile)

(defface tv-compile-highlight
  '((((class color) (background light))
     (:inherit tv-compile :weight demibold))
    (((class color) (background dark)) (:foreground "red" :background "white")))
  "Face for annotation text."
  :group 'tvfile)

(defface tv-compile-negative
  '((t (:inherit tv-compile :inherit tv-test-negative)))
  "Font Lock mode face used to highlight warnings."
  :group 'tvfile)

(defface tv-compile-positive
  '((t (:inherit tv-compile :inherit tv-test-positive)))
  "Font Lock mode face used to highlight positive."
  :group 'tvfile)

(defface tv-compile-undecided
  '((t (:inherit tv-compile :inherit tv-test-undecided)))
  "Font Lock mode face used to highlight undecided."
  :group 'tvfile)

(defvar tvfile-found-tvoutput nil)
(defvar tvfile-compile-overlay nil)
(defvar tvfile-compile-result nil)
(defvar tvfile-compile-start nil)
(defvar tvfile-compile-wait nil)

(defconst tvfile-compile-re (concat
                             "\\(=== Taskerkontrolltask einrichten und starten ===\\)"
			     "\\|<TFT> Test gefunden: '\\(.*\\)'"
			     "\\|<TFT> Ausfuehren von '\\(.*\\)'"
			     "\\|\\(" tvfile-sum-positive-re "\\)"
			     "\\|\\(" tvfile-sub-positive-re "\\)"
			     "\\|\\(" tvfile-sum-negative-re "\\)"
			     "\\|\\(" tvfile-sub-negative-re "\\)"
			     "\\|\\(\\.\\.\\. Review der Testausgaben notwendig"
			     "\\|\\.\\.\\. Test in dieser Umgebung nicht relevant"
			     "\\|\\.\\.\\. manuelle Eingabe des Testresultats erforderlich\\)"
			     "\\|\\(\\.\\.\\.wait\\)"
			     "\\|\\(\\.\\.\\.\\(?:end of wait\\|resume after\\)\\)"
			     ))

(defun tvfile-compile-scan-old (pos-max &optional last-result)
  "Scan from point till end of buffer. Return a list

   (NUMBER-OF-STARTUPS NUMBER-FOUND-TESTS RUNNING-TEST PREVIOUS-TEST POSITIVE-COUNT NEGATIVE-COUNT UNDECIDED-COUNT)

   NUMBER-OF-STARTUPS  Number of startups: \"=== Taskerkontrolltask einrichten und starten ===\"
   NUMBER-FOUND-TESTS  Number of found tests: \"<TFT> Test gefunden: '.*'\"
   RUNNING-TEST        Currently running test. A list with
                           (NAME POSITIVE SUB-POSITIVE SUB-NEGATIVE)
                           NAME  Name of currently running test: \"<TFT> Ausfuehren von '.*'\"
                           POSITIVE `t' if test was positive, `nil' otherwise.
                           SUB-POSITIVE Number of positive sub-tests.
                           SUB-NEGATIVE Number of negative sub-tests.
   PREVIOUS-TEST       Prevoius Test. A list like RUNNING-TEST
"
  (interactive)
  (let* ((result (or last-result (list 0 0 nil nil 0 0 0)))
	 (reboots (nth 0 result))
	 (found (nth 1 result))
	 (running-test (or (nth 2 result) (list "" nil 0 0)))
	 (positive (nth 4 result))
	 (negative (nth 5 result))
	 (undecided (nth 6 result))
	 test
	 previous)
    (while (re-search-forward tvfile-compile-re pos-max t)
      (setq tvfile-found-tvoutput t)
      (cond
       ((match-end 1)
	;; Neuer Hochlauf
	(setq reboots (1+ reboots)
	      found 0
	      test nil))
       ((match-end 2)
	;; Test gefunden
	(setq found (1+ found)))
       ((match-end 3)
	;; Ausfuehren von
	(setq previous running-test)
	(setq running-test (list (match-string 3) nil 0 0))
	(setf (nth 1 running-test) t))
       ((match-end 4)
	;; Test erfolgreich
	(setq positive (1+ positive))
	(setf (nth 1 running-test) nil))
       ((match-end 5)
	;; Subtest erfolgreich
	(setf (nth 2 running-test) (1+ (nth 2 running-test))))
       ((match-end 6)
	;; Test fehlgeschlagen
	(setq negative (1+ negative))
	(setf (nth 1 running-test) nil))
       ((match-end 7)
	;; Subtest ist fehlgeschlagen
	(setf (nth 3 running-test) (1+ (nth 3 running-test))))
       ((match-end 8)
	;; Ergebnis ohne Aussage
	(setq undecided (1+ undecided)))
       ((match-end 9)
	(save-excursion
	  (goto-char (match-beginning 9))
	  (setq tvfile-compile-wait
		(buffer-substring (point) (point-at-eol)))))
       ((match-end 10)
	(setq tvfile-compile-wait nil))
       ))
    (setq result (list reboots found running-test previous positive negative undecided))
    result))

(defun tvfile-compile-scan (pos-max &optional last-result)
  "Scan from point till end of buffer. Return a property list

   (:startups NUMBER-OF-STARTUPS
    :tests-found NUMBER-FOUND-TESTS
    :current RUNNING-TEST
    :previous PREVIOUS-TEST
    :positive POSITIVE-COUNT
    :negative NEGATIVE-COUNT
    :undecided UNDECIDED-COUNT)

   NUMBER-OF-STARTUPS  Number of startups: \"=== Taskerkontrolltask einrichten und starten ===\"
   NUMBER-FOUND-TESTS  Number of found tests: \"<TFT> Test gefunden: '.*'\"
   RUNNING-TEST        Currently running test. A list with
                           (NAME POSITIVE SUB-POSITIVE SUB-NEGATIVE)
                           NAME  Name of currently running test: \"<TFT> Ausfuehren von '.*'\"
                           POSITIVE `t' if test was positive, `nil' otherwise.
                           SUB-POSITIVE Number of positive sub-tests.
                           SUB-NEGATIVE Number of negative sub-tests.
   PREVIOUS-TEST       Prevoius Test. A list like RUNNING-TEST
"
  (interactive)
  (let* ((result (or last-result (list :startups 0
                                       :tests-found 0
                                       :positive 0
                                       :negative 0
                                       :undecided 0)))
	 (reboots (plist-get result :startups))
	 (found (plist-get result :tests-found))
	 (running-test (or (plist-get result :current) (list "" nil 0 0)))
	 (positive (plist-get result :positive))
	 (negative (plist-get result :negative))
	 (undecided (plist-get result :undecided))
	 test
	 previous)
    (while (re-search-forward tvfile-compile-re pos-max t)
      (setq tvfile-found-tvoutput t)
      (cond
       ((match-end 1)
	;; Neuer Hochlauf
	(setq reboots (1+ reboots)
	      found 0
	      test nil))
       ((match-end 2)
	;; Test gefunden
	(setq found (1+ found)))
       ((match-end 3)
	;; Ausfuehren von
	(setq previous running-test)
	(setq running-test (list (match-string 3) nil 0 0))
	(setf (nth 1 running-test) t))
       ((match-end 4)
	;; Test erfolgreich
	(setq positive (1+ positive))
	(setf (nth 1 running-test) nil))
       ((match-end 5)
	;; Subtest erfolgreich
	(setf (nth 2 running-test) (1+ (nth 2 running-test))))
       ((match-end 6)
	;; Test fehlgeschlagen
	(setq negative (1+ negative))
	(setf (nth 1 running-test) nil))
       ((match-end 7)
	;; Subtest ist fehlgeschlagen
	(setf (nth 3 running-test) (1+ (nth 3 running-test))))
       ((match-end 8)
	;; Ergebnis ohne Aussage
	(setq undecided (1+ undecided)))
       ((match-end 9)
	(save-excursion
	  (goto-char (match-beginning 9))
	  (setq tvfile-compile-wait
		(buffer-substring (point) (point-at-eol)))))
       ((match-end 10)
	(setq tvfile-compile-wait nil))
       ))
    (setq result (list :startups reboots
                       :tests-found found
                       :current running-test
                       :previous previous
                       :positive positive
                       :negative negative
                       :undecided undecided))
    result))

(defun tvfile-compilation-start (proc)
  "Prepare for counting the running tests."
  (make-local-variable 'tvfile-found-tvoutput)
  (make-local-variable 'tvfile-compile-overlay)
  (make-local-variable 'tvfile-compile-result)
  (make-local-variable 'tvfile-compile-start)
  (make-local-variable 'tvfile-compile-wait)
  (make-local-variable 'header-line-format)
  (setq tvfile-compile-start (current-time))
  (setq tvfile-compile-result nil)
  (setq tvfile-compile-wait nil)
  (tvfile-minor-mode 1)
  (setq tvfile-found-tvoutput nil)
  (remove-overlays (point-min) (point-max) 'tvfile t)
  (setq tvfile-compile-overlay (make-overlay (point) (point-max) nil 'front-advance))
  (overlay-put tvfile-compile-overlay 'tvfile t))

;;(add-hook 'compilation-start-hook 'tvfile-compilation-start)

(defun tvfile-runtime-to-HH:MM:SS (runtime)
  "Convert RUNTIME in seconds to string HH:MM:SS."
  (let* ((h (floor runtime 3600))
	 (m (floor (mod runtime 3600) 60))
	 (s (- runtime (+ (* h 3600) (* m 60)))))
    (format "%02d:%02d:%02d =%05d[sec]" h m s runtime)))

(defun tvfile-add-props (string plist &rest props)
  "Add text properties to entire string, from beginning to end.
PLIST may be a list of properties, PROPS are individual properties and values
that will be added to PLIST.  Returns the string that was modified."
  (add-text-properties
   0 (length string) (if props (append plist props) plist) string)
  string)
(put 'tvfile-add-props 'lisp-indent-function 2)

(defun tvfile-format-test-text (current)
  "Format text line of CURRENT test. CURRENT is a list.

    (NAME POSITIVE SUB-POSITIVE SUB-NEGATIVE)

          NAME  Name of currently running test: \"<TFT> Ausfuehren von '.*'\"
          POSITIVE `t' if test was positive, `nil' otherwise.
          SUB-POSITIVE Number of positive sub-tests.
          SUB-NEGATIVE Number of negative sub-tests."
  (if (or (null current) (string= (car current) ""))
      (tvfile-propertize (format "No test currently running\n") 'face 'tv-compile)
    (tvfile-propertize
     (format "Running test '%s'\n     with %d subtests (%d positive, %d negative)\n"
	     (car current)
	     (+ (nth 2 current) (nth 3 current))
	     (nth 2 current)
	     (nth 3 current))
     'face 'tv-compile-highlight)))

    ;; (concat
    ;;  (propertize
    ;;   "Running "
    ;;   'face 'tv-compile)
    ;;  (if (nth 1 current)
    ;; 	 (propertize "POSITIVE" 'face 'tv-test-positive)
    ;;    (propertize "NEGATIVE" 'face 'tv-test-negative))
    ;;  (propertize
    ;;   (format " test '%s'\n     with %d subtests ("
    ;; 	      (car current)
    ;; 	      (+ (nth 2 current) (nth 3 current)))
    ;;   'face 'tv-compile)
    ;;  (propertize
    ;;   (format "%d positive,"
    ;; 	      (nth 2 current))
    ;;   'face 'tv-test-positive)
    ;;  (propertize
    ;;   (format " %d negative)\n"
    ;; 	      (nth 3 current))
    ;;   'face 'tv-test-negative))))

(defsubst tvfile-propertize (text &rest properties)
  (apply 'propertize text properties))

(add-to-list 'tvfile-minor-mode-hook 'tvfile-scan-results)
(defun tvfile-scan-results ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((tvfile-compile-result (tvfile-compile-scan (point-max)))
           (subtest (plist-get tvfile-compile-result :current))
           (positive (plist-get tvfile-compile-result :positive))
           (negative (plist-get tvfile-compile-result :negative))
           (undecided (plist-get tvfile-compile-result :undecided))
           (tvfile-compile-start (current-time))
           (runtime 0)
           (sum (+ positive negative undecided))
           start)

      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^[123].\\([0-9]+\\)<0xff01> " nil t)
          (setq start (match-string-no-properties 1))
          (goto-char (point-max))
          (when (re-search-backward "^[123].\\([0-9]+\\)<0xff01> " nil t)
            (setq end (match-string-no-properties 1))
            (setq runtime (- (string-to-number end)
                             (string-to-number start))))))

      (setq header-line-format
	  (concat
	   (format "Since %s (%s)"
		   (format-time-string "%H:%M:%S" tvfile-compile-start)
		   (tvfile-runtime-to-HH:MM:SS runtime)
		   )

	   (if tvfile-found-tvoutput
	       (concat
		 (format ", done (%04d/%04d/r%02d) ("
                         sum
                         (plist-get tvfile-compile-result :tests-found)
                         (plist-get tvfile-compile-result :startups)
			 )

		 (tvfile-add-props
		     (format "positive(%d/S%d)"
			     positive
			     (nth 2 subtest))
		     nil
		   'face 'tv-test-positive)
		 " "
		 (propertize
		  (format "negative(%d/S%d)"
                          negative
			  (nth 3 subtest))
		  'face 'tv-test-negative)
		 " "
		 (propertize
		  (format "undecided(%d)"
                          undecided)
		  'face 'tv-test-undecided)
		 ")")
	     )
	   ))
          (force-mode-line-update)
          )))

(defconst tvfile-export-variable "TESTNAME"
  "Siehe export-tester.el")

(defun tvfile-compilation-filter ()
  "Update the test run statistic and show it at end of
compilation buffer as overlay."
  (let ((end (point))
	(runtime (- (float-time) (float-time tvfile-compile-start)))
	text result subtest
	(pos-max (point))
        positive negative undecided sum)
    (save-excursion
      (goto-char compilation-filter-start)
      (setq tvfile-compile-result (tvfile-compile-scan pos-max tvfile-compile-result)))
    ;; (setq result tvfile-compile-result)
    ;; (with-temp-buffer
    ;;   (insert (propertize
    ;; 	       (format "\nRunning since %s (%s)\n" (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
    ;; 		       (tvfile-runtime-to-HH:MM:SS runtime))
    ;; 	       'face 'tv-compile))
    ;;   (insert (propertize
    ;; 	       (format "Found %d reboots. " (nth 0 result))
    ;; 	       'face 'tv-compile))
    ;;   (insert (propertize
    ;; 	       (format "Already done %d test (%d positive, %d negative).\n"
    ;; 		       (+ (nth 4 result) (nth 5 result))
    ;; 		   (nth 4 result) (nth 5 result))
    ;; 	       'face 'tv-compile))
    ;;   (insert (tvfile-format-test-text (nth 2 result)))
    ;;   (insert "\n")
    ;;   (setq text (buffer-substring (point-min) (point-max))))

    (setq subtest (plist-get tvfile-compile-result :current)
          positive (plist-get tvfile-compile-result :positive)
          negative (plist-get tvfile-compile-result :negative)
          undecided (plist-get tvfile-compile-result :undecided)
          sum (+ positive negative undecided))

    (setq text
	  (concat
	   "\n"
    	   (tvfile-propertize
	    (concat (format "Running since %s (%s)\n"
			    (format-time-string "%Y-%m-%d %H:%M:%S" tvfile-compile-start)
			    (tvfile-runtime-to-HH:MM:SS runtime)))
	    'face 'tv-compile)

           (let ((expv (getenv tvfile-export-variable)))
             (if expv
                 (tvfile-propertize
                  (format "Found \"%s=%s\"\n"
                          tvfile-export-variable expv)
                  'face 'tv-compile)))

	   (if tvfile-found-tvoutput
	       (concat
		 (tvfile-propertize
		  (concat
		   (format "Found %d reboots. " (plist-get tvfile-compile-result :startups))
		   (format "Found %d tests at startup after last reboot.\n" (plist-get tvfile-compile-result :tests-found))
		   (format "Already done %d test (%d positive, %d negative, %d undecided).\n"
			   sum positive negative undecided))
		  'face 'tv-compile)
		 (tvfile-format-test-text subtest)
		 (if tvfile-compile-wait
		     (tvfile-propertize (concat "Waiting: '" tvfile-compile-wait "'\n")  'face 'tv-compile-wait))))

	;"Previous: " (tvfile-format-test-text (nth 3 tvfile-compile-result))
    	   ))
    (overlay-put tvfile-compile-overlay 'after-string text)
    (setq header-line-format
	  (concat
	   (format "Since %s (%s)"
		   (format-time-string "%H:%M:%S" tvfile-compile-start)
		   (tvfile-runtime-to-HH:MM:SS runtime)
		   )
	   (if tvfile-found-tvoutput
	       (concat
		 (format ", done (%04d/%04d/r%02d) ("
                         sum
                         (plist-get tvfile-compile-result :tests-found)
                         (plist-get tvfile-compile-result :startups)
			 )

		 (tvfile-add-props
		     (format "positive(%d/S%d)"
			     positive
			     (nth 2 subtest))
		     nil
		   'face 'tv-test-positive)
		 " "
		 (propertize
		  (format "negative(%d/S%d)"
                          negative
			  (nth 3 subtest))
		  'face 'tv-test-negative)
		 " "
		 (propertize
		  (format "undecided(%d)"
                          undecided)
		  'face 'tv-test-undecided)
		 ")")
	     )
	   ))
    (move-overlay tvfile-compile-overlay (point-at-bol) (point-at-eol))
  ))

;;(add-hook 'compilation-filter-hook 'tvfile-compilation-filter)

;; ================================================================================
;; Testframe for tvfile-compilation-filter

(defun tvfile-test-filter-hook ()
  "Simulate `tvfile-compilation-filter' by reading a file and
calling the hooks for the output buffer."
  (interactive)
  (let ((buffer (call-interactively 'find-file))
	min max
	pos epos test-buffer text)
    (when buffer
      (if (get-buffer "*compile-test*") (kill-buffer "*compile-test*"))
      (setq test-buffer (get-buffer-create "*compile-test*"))
      (switch-to-buffer test-buffer)
      (with-current-buffer test-buffer
	(compilation-mode "Compilation")
	(setq buffer-read-only nil)
	(setq min (point-min-marker))
	(setq max (point-max-marker))
	(run-hook-with-args 'compilation-start-hook t))
      (with-current-buffer buffer
	(setq pos (point-min))
	(while (< pos (point-max))
	  (setq epos (+ pos (random 10000)))
	  (if (> epos (point-max))
	      (setq epos (point-max)))
	  (setq text (buffer-substring-no-properties pos epos))
	  (with-current-buffer test-buffer
	    ;; If we are inserting at the end of the accessible part
	    ;; of the buffer, keep the inserted text visible.
	    (set-marker-insertion-type max t)
	    (widen)
	    ;; We used to use `insert-before-markers', so that windows with
	    ;; point at `process-mark' scroll along with the output, but we
	    ;; now use window-point-insertion-type instead.
	    (insert text)
	    (let ((compilation-filter-start pos))
	      (run-hooks 'compilation-filter-hook)))
	  (sit-for 0 300)
	  (setq pos epos)))
      (set-marker min nil)
      (set-marker max nil))))

;; --------------------------------------------------------------------------------

(defconst tvfile-line-regexp-alist
  '(
    ;; televist
    ("^\\(?:\\*CHECKSUM ERROR\\*> \\)?\\([123]\\)\\.[0-9]+\\(<[0-9a-fx]+>\\) \\(.*?\\)\\( \\\\\\)?$"
     1 2 3 4)
    ;; uldisplay
    ("^[0-9]+ \\([012]\\)\\.[0-9]+:[0-9]+\\(<[0-9a-fx]+>\\) \\(.*\\)"
     1 2 3)
    )
    "Regexp for finding a line output. Each entry is a list containing

   (regexp
      match-for-link-channel
      match-for-unilink-channel
      match-for-data
      opt-match-for-continuation)
"
    )

(defvar tvfile-line-regexp nil
  "Used regexp for scanning the file.")
(defvar tvfile-line-regexp-link-channel nil
  "Matched regexp for link-channel.")
(defvar tvfile-line-regexp-uni-channel nil
  "Matched regexp for unilink-channel.")
(defvar tvfile-line-regexp-data nil
  "Matched regexp for data.")
(defvar tvfile-line-regexp-continuation nil
  "Matched regexp for optional coninuation.")

(defun tvfile-identify-tv ()
  (catch 'found
    (dolist (elem tvfile-line-regexp-alist)
      (let ((regexp (car elem))
            (channel (nth 1 elem)))
        (save-excursion
          (goto-char (point-min))
          (if (and (search-forward-regexp regexp nil t)
                   (match-beginning channel))
              (progn
                (setq tvfile-line-regexp regexp
                      tvfile-line-regexp-link-channel channel
                      tvfile-line-regexp-uni-channel (nth 2 elem)
                      tvfile-line-regexp-data (nth 3 elem)
                      tvfile-line-regexp-continuation (nth 4 elem)
                      )
                (throw 'found t)))
          nil))
      )))

(defun tvfile-filter-tv ()
  "Desplit tv lines in current buffer."
  (interactive)
  (let (pos end str)
    (save-excursion
      (goto-char (point-min))
      ;;(while (search-forward-regexp "^[0-9.]+<0xff00> \\(.*?\\)\\( \\\\\\)?$" nil t)
      (while (search-forward-regexp tvfile-line-regexp nil t)
        (when (and (match-beginning tvfile-line-regexp-uni-channel)
                   (string= (match-string tvfile-line-regexp-uni-channel)
                            "<0xff00>"))
          (setq pos (and tvfile-line-regexp-continuation
                         (match-beginning tvfile-line-regexp-continuation)))
          (when pos
            (setq end (match-end tvfile-line-regexp-continuation))
            (beginning-of-line)
            (forward-line)
            (when (looking-at "^[0-9.]+<0xff00> \\\\>   \\(.*\\)")
              (setq str (match-string 1))
              (delete-region (point-at-bol) (save-excursion
                                              (beginning-of-line)
                                              (forward-line)
                                              (point)))
              (delete-region pos end)
              (save-excursion
                (goto-char pos)
                (insert-before-markers str))
              )
            )
          )
        )
      )
    )
  )

(defun tvfile-split-tvfile ()
  "Split current tvfile buffer and put each unilink channel in
seperate buffer. Lines not associated to a special channel will
be copied in all buffers."
  (interactive)
  (let ((text "")
        (data "")
        (buff-name (or (buffer-file-name) (buffer-name)))
        buffers
        buffer
        text-copied
        preface preface-copied
        start beg end found)
    (save-excursion
      (unless (tvfile-identify-tv)
        (error "Buffer does not contain tvfile content."))

      (setq preface
            (format "content copied from file: %s\ndate: %s\n\n"
                    buff-name
                    (format-time-string "%a, %d %b %Y %T %z")))

      (setq start (point-min))
      (goto-char start)
      (while (and (< (point) (point-max))
                  (search-forward-regexp tvfile-line-regexp nil t))
        (setq channel (string-to-number (match-string tvfile-line-regexp-link-channel))
              beg (point-at-bol))
        (if (> beg start)
            (setq text (buffer-substring start beg)
                  text-copied nil
                  start beg))

        ;; search change in channel or lines without channel
        (while (and (setq found (search-forward-regexp tvfile-line-regexp nil t))
                    (= channel (string-to-number (match-string
                                                  tvfile-line-regexp-link-channel)))))
        (if (and (not found)
                 (>= (save-excursion (beginning-of-line)(forward-line)(point))
                     (point-max))
                 (= (1- (point-max)) (point-at-eol)))
            (setq end (point-max))
          (setq end (point-at-bol)))
        (setq data (buffer-substring beg end))

        ;; copy text to appropriate buffer
        (setq buffer (assoc channel buffers))
        (unless buffer
          (let ((fn (file-name-sans-extension buff-name))
                (ext (file-name-extension buff-name)))
            (setq buffer (cons channel
                               (get-buffer-create
                                (concat fn "_channel-" (format "%s" channel)
                                        (if ext (concat "." ext))))))
            (with-current-buffer (cdr buffer)
              (unless buffer-file-name
                (set-visited-file-name (buffer-name))))
            (add-to-list 'buffers buffer)))
        (with-current-buffer (cdr buffer)
          (goto-char (point-max))
          (unless (member channel preface-copied)
            (insert preface)
            (push channel preface-copied))
          (unless (member channel text-copied)
            (insert text)
            (push channel text-copied))
          (insert data))

        (setq beg end
              start beg
              data "")
        (goto-char beg)

        ;; text or channel info?
        (unless (match-beginning tvfile-line-regexp-link-channel)
          (setq start (point)))
        )
      )
    (mapc (lambda (buf)
            (switch-to-buffer (cdr buf))
            (buffer-enable-undo)
            (tvfile-filter-tv)
            (font-lock-fontify-buffer)
            (tvfile-scan-results)
            (when (featurep 'org-link-minor-mode)
              (org-link-minor-mode))
            (goto-char (point-min))
            )
          buffers)
    )
  )

(provide 'tvfile)
;;; tvfile.el ends here
