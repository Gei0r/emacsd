;; -*- coding: utf-8-unix; -*-
;;; cq.el --- ClearQuest
;;
;; Copyright (C) 2012, 2013, 2014, 2015 Stefan-W. Hahn
;;
;; Author: Stefan-W. Hahn <stefan dot hahn at siemens dot com>
;; Keywords: cq
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

;; If files from ClearQuest should be automatically converted if
;; they contain query results, insert following in your init.el

;; (add-hook 'find-file-hook 'cq-find-file-hook)

;; Open points:
;; - On lines with org-mode text properties these get lost if applying cq properties

(require 'url)
(require 'url-auth)
(require 'url-http)
(require 'org)         ;; org-defkey
(require 'org-table)   ;; org-table-convert-region, org-table-to-lisp
(require 'font-lock)
(require 'browse-url)
(require 'ediff-util)  ;; ediff-other-buffer
(require 'uuid nil t)
(require 'orgtbl-join)
(require 'cq-find-tables)

(declare-function ido-completing-read "ido" (prompt choices &optional predicate require-match
                                                    initial-input hist def inherit-input-method))

(defgroup cq nil
  "Options specific for ClearQuest."
  :group 'cq
  :tag "ClearQuest")

(defcustom cq-template-query-pr-by-cfx-txt
  "https://champweb.siemens.net/cqweb/restapi/01_CHAMP/CFX/RECORD/%s?format=TEXT&loginId={loginid}&password={password}&recordType=CFXRequest&noframes=true"
   "Template for accessing ClearQuest via http. Parameter order is `pr'."
  :group 'cq
  :type 'regexp)

(defcustom cq-template-query-pr-by-cfx
  "https://champweb.siemens.net/cqweb/restapi/01_CHAMP/CFX/RECORD/%s?format=HTML&loginId={loginid}&password={password}&recordType=CFXRequest&noframes=true"
   "Template for accessing ClearQuest via http. Parameter order is `pr'."
  :group 'cq
  :type 'regexp)

(defcustom cq-template-query-pr-by-cfxid
  "https://champweb.siemens.net/cqweb/oslc/repo/01_CHAMP/db/CFX/record/?oslc_cm.query=CFXID=\"%s\"&loginId={loginid}&password={password}&rcm.type=CFXRequest"
   "Template for accessing ClearQuest via http. Parameter order is `pr'."
  :group 'cq
  :type 'regexp)

(defcustom cq-template-query-pr-by-gnats
  "https://champweb.siemens.net/cqweb/oslc/repo/01_CHAMP/db/CFX/record/?oslc_cm.query=MigratedID=\"%s\"&loginId={loginid}&password={password}&rcm.type=CFXRequest"
   "Template for accessing ClearQuest via http. Parameter order is `pr'."
  :group 'cq
  :type 'regexp)

(defcustom cq-pr-id-re
  "\\(\\(GNATS\\|SP_P\\|SisW\\|CFX\\|SPLC\\)\\(?:[ #]\\| #\\)?\\([0-9]\\{1,8\\}\\)\\)"
  "#1 true if string is problem id.
#2 prefix
#3 id. CQ ID"
  :group 'cq
  :type 'regexp)

(defcustom cq-browser
  "d:/bin/FirefoxPortable/FirefoxPortable.exe"
  "Browser to use for calling a problem report."
  :group 'cq
  :type '(choice
          (file :tag "Browser" :must-match t)
          (const :tag "Default browser" nil)))

(defconst cq-realm "01_CHAMP")
(defvar cq-auth-user nil)
(defvar cq-auth-pass nil)

(if (symbolp 'advice-add)
    (progn
      ;; Nutzung von advice-add erst ab emacs 24.4

      (defun cq-read-string-function (orig-fun &rest args)
        (setq cq-auth-user (apply orig-fun args))
        ;; Advice entfernen, damit read-passwd den gelesenen Usernamen nicht
        ;; überschreibt
        (advice-remove 'read-string #'cq-read-string-function)
        (if (string-match
             "^\\(WW.*\\|AD.*\\)\\\\" cq-auth-user)
            cq-auth-user
          (setq cq-auth-user (concat "AD001\\" cq-auth-user)))
        )

      (defun cq-read-passwd-function (orig-fun &rest args)
        (setq cq-auth-pass (apply orig-fun args))
        cq-auth-pass)

      (defun cq-read-authentication (url)
        "Read username and password for using clearquest."
        (interactive)

        ;; Advice functions to get username and password
        (advice-add 'read-string :around #'cq-read-string-function)
        (advice-add 'read-passwd :around #'cq-read-passwd-function)

        ;;(url-get-authentication url "01_CHAMP" "basic" t)
        ;; *TODO* Bei Aufruf von url-get-authentication wird jedes Mal das Passwort abgefragt.
        (url-basic-auth url t nil cq-realm)

        (advice-remove 'read-string #'cq-read-string-function)
        (advice-remove 'read-passwd #'cq-read-passwd-function)

        (and cq-auth-user cq-auth-pass)))

  ;; usage of old defadvice

  (defadvice read-string (around cq-read-string-function)
    (setq cq-auth-user ad-do-it)
    ;; Advice entfernen, damit read-passwd den gelesenen Usernamen nicht
    ;; überschreibt
    (ad-deactivate 'read-string)
    (if (string-match
         "^\\(WW.*\\|AD.*\\)\\\\" cq-auth-user)
        cq-auth-user
      (setq cq-auth-user (concat "AD001\\" cq-auth-user)))
    )

  (defadvice read-passwd (around cq-read-passwd-function)
    (setq cq-auth-pass ad-do-it)
    cq-auth-pass)

  (defun cq-read-authentication (url)
    "Read username and password for using clearquest."
    (interactive)

    ;; Advice functions to get username and password
    (ad-activate 'read-string)
    (ad-activate 'read-passwd)

    ;;(url-get-authentication url "01_CHAMP" "basic" t)
    ;; *TODO* Bei Aufruf von url-get-authentication wird jedes Mal das Passwort abgefragt.
    (url-basic-auth url t nil cq-realm)

    (ad-deactivate 'read-string)
    (ad-deactivate 'read-passwd)

    (and cq-auth-user cq-auth-pass))
  )

(defun cq-clear-authentication ()
  "Clear authentication or call of clearquest."
  (interactive)
  (dolist (elem '(url-http-proxy-basic-auth-storage
                  url-http-real-basic-auth-storage))
    (unless (null (symbol-value elem))
      (set elem
            (mapcar #'(lambda (elt)
                        (if (string= (car (cadr elt)) cq-realm)
                            nil
                          elt))
                    (symbol-value elem)))))
  (setq cq-auth-user nil)
  (setq cq-auth-pass nil)
  )

(defun cq-set-authentication (url)
  (when (cq-read-authentication url)
    (setq url (replace-regexp-in-string "{loginid}" (url-hexify-string cq-auth-user) url))
    (setq url (replace-regexp-in-string "{password}" (url-hexify-string cq-auth-pass) url))
    )
  url)

(defun cq-open-url (url)
  "Start program via `url'."
  (w32-shell-execute "open" cq-browser (cq-set-authentication url))
)

;; (defun xx ()
;;   (interactive)
;;   (let ((url (cq-set-authentication
;;               (format cq-template-query-pr-by-cfx "CFX00199831")))
;;         (url-using-proxy nil)
;;         (url-proxy-services nil)
;;         (url-cookie-trusted-urls '("^https?://"))
;;         (url-request-method "GET")
;;         (url-request-extra-headers
;;          `(("Content-Type" . "application/json; charset=UTF-8")
;;            ("Authorization" . ,(concat "Basic "
;;                                        (base64-encode-string
;;                                         (concat cq-auth-user ":" cq-auth-pass))))
;;            )
;;          )
;;         )
;;     (url-retrieve url
;;                   (lambda (status)
;;                     (switch-to-buffer (current-buffer))))
;;     )
;;   )


;; https://champweb.siemens.net/cqweb/oslc/?rcm.contentType=application/json
;; https://champweb.siemens.net/cqweb/oslc/01_CHAMP/discovery

;; (defun xx ()
;;   (interactive)
;;   (cq-search-and-open-url "CFX00199831" (format cq-template-query-pr-by-cfx-txt "CFX00199831")))
;; https://champweb.siemens.net/cqweb/oslc/repo/01_CHAMP/db/CFX/record/?oslc_cm.query=CFXID="SP_P00005083"&loginId=AD001%5Cbw1hahs0&password=Astoria%2304&rcm.type=CFXRequest
;; https://champweb.siemens.net/cqweb/oslc/repo/01_CHAMP/db/CFX/record/?oslc_cm.query=CFXID="SP_P00005083"&rcm.type=CFXRequest

;; (let ((url "https://champweb.siemens.net/cqweb/oslc/repo/01_CHAMP/db/CFX/record/?oslc_cm.query=CFXID=\"SP_P00005083\"&rcm.type=CFXRequest")
;;       (url-using-proxy nil)
;;       (url-proxy-services nil)
;;       (url-debug t))
;;   (url-retrieve-synchronously (cq-set-authentication url) 'no-silent))

(defun xx ()
  (interactive)
  (let* ((url "https://champweb.siemens.net/cqweb/restapi/01_CHAMP/CFX/RECORD/CFX00186135?format=XML&loginId=AD001%5Cbw1hahs0&password=Astoria%2308&noframes=true")
        (url-using-proxy nil)
        (url-proxy-services nil)
        (url-debug t)
        (buf (url-retrieve-synchronously (cq-set-authentication url) 'no-silent)))
    (switch-to-buffer buf)
    (when (not (null buf))
      (with-current-buffer buf
        ;;(switch-to-buffer buf)
        (re-search-forward "^$" nil 'move)
        (setq xml (xml-parse-region (point) (point-max)))
        (switch-to-buffer (get-buffer-create "*result*"))
        (xml-print xml)))))

(defun cq-search-real-id (pr url)
  "If SP_P or GNATS search PR and open via search result."
  (let* ((url-using-proxy nil)
         (url-proxy-services nil)
         (url-debug t)
         (buf (url-retrieve-synchronously (cq-set-authentication url) 'no-silent))
	 count url-list id)
    (when (not (null buf))

      (switch-to-buffer buf)

      (with-current-buffer buf
	(goto-char (point-min))
;; TODO
;;	(if (re-search-forward "The document hast moved.*<address>.* at \\([a-zA-Z:.]+\\) Port \\([0-9]+\\)</address>" nil t)

	(when (re-search-forward "<oslc_cm:totalCount.*?>\\([0-9]+\\)</oslc_cm:totalCount>" nil t)
	    (setq count (string-to-number (match-string 1)))

	    (cond
	     ((= count 1)

	      (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
		  (setq id (match-string 1)))
	      (if (re-search-forward "<link rel=\"alternate\" href=\"\\(.*\\)\"/>" nil t)
		  (progn
		    (message "open %s -> %s" pr id)
                    id))
	      )
	     ((> count 1)

	      (while (> count 0)
		(let (id url)
		  (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
		      (setq id (match-string 1)))
		  (if (re-search-forward "<link rel=\"alternate\" href=\"\\(.*\\)\"/>" nil t)
		      (setq url (match-string 1)))

		  (push (cons id (list id url)) url-list))
		(setq count (1- count)))

	      (let* ((all-ids (mapcar (lambda (x)
					(car x))
                                      url-list))
		     (id (ido-completing-read "PR: " all-ids))
		     )
                id)
	      )
	     (t
	      (message "No PR %s found" pr)
	      ;;
	      ))))

      (kill-buffer buf)
      )
    id))

(defun cq-search-id-by-prefix (prefix num)
  (let ((prefix (upcase prefix))
        id)
    (cond
     ((string= prefix "SP_P")
      (setq id (format "%s%08d" prefix num))
      (cq-search-real-id id (format cq-template-query-pr-by-cfxid id)))
     ((string= prefix "SISW")
      (setq id (format "%s%08d" prefix num))
      (cq-search-real-id id (format cq-template-query-pr-by-cfxid id)))
     ((string= prefix "GNATS")
      (setq id (format "%s%04d" prefix num))
      (cq-search-real-id id (format cq-template-query-pr-by-gnats id)))
     ((string= prefix "SPLC")
      (setq id (format "%s%08d" prefix num))
      (cq-search-real-id id (format cq-template-query-pr-by-cfxid id)))
     (t
      (format "%s%08d" prefix num)))))

(defun cq-search-and-open-url (pr url)
  "If SP_P or GNATS search PR and open via search result."
  (let ((url-using-proxy nil)
        (url-proxy-services nil)
        (buf (url-retrieve-synchronously (cq-set-authentication url)))
	count url-list id)
    (when (not (null buf))

      (switch-to-buffer buf)

      (with-current-buffer buf
	(goto-char (point-min))
;; TODO
;;	(if (re-search-forward "The document hast moved.*<address>.* at \\([a-zA-Z:.]+\\) Port \\([0-9]+\\)</address>" nil t)

	(when (re-search-forward "<oslc_cm:totalCount.*?>\\([0-9]+\\)</oslc_cm:totalCount>" nil t)
	    (setq count (string-to-number (match-string 1)))

	    (cond
	     ((= count 1)

	      (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
		  (setq id (match-string 1)))
	      (if (re-search-forward "<link rel=\"alternate\" href=\"\\(.*\\)\"/>" nil t)
		  (progn
		    (message "open %s -> %s" pr id)
		    (cq-open-url (format cq-template-query-pr-by-cfx id))))
	      )
	     ((> count 1)

	      (while (> count 0)
		(let (id url)
		  (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
		      (setq id (match-string 1)))
		  (if (re-search-forward "<link rel=\"alternate\" href=\"\\(.*\\)\"/>" nil t)
		      (setq url (match-string 1)))

		  (push (cons id (list id url)) url-list))
		(setq count (1- count)))

	      (let* ((all-ids (mapcar (function
				       (lambda (x)
					(car x))) url-list))
		     (id (ido-completing-read "PR: " all-ids))
		     url)
		(setq url (cdr (assoc id all-ids)))
		(if url
		    (message "open %s -> %s" pr id)
		    (cq-open-url (format cq-template-query-pr-by-cfx id))))
	      )
	     (t
	      (message "no PR found")
	      ;;
	      ))))

      (kill-buffer buf)
      )))

(defun cq-open-pr (id)
  "Open PR by ID"
  (interactive
   (list
    (if (use-region-p)
	(buffer-substring (region-beginning) (region-end))
      (read-string "Search for PR: " (thing-at-point 'word)))))
  (if (string-match cq-pr-id-re id)
      (cq-open-pr-by-id (list (match-string 2 id)
                              (string-to-number (match-string 3 id))))
    (message "no problem id.")))

(defun cq-get-pr-id-at-point (&optional pos)
  "Get id of PR at point."
  (get-text-property (or pos (point)) 'cq-id))

(defun cq-open-pr-by-id (&optional prid pos)
  "Try to read a pr via if point is on an PR-ID. PRID ist a list with `PREFIX' and `NUMBER'.
cq-id = '(PREFIX NUM). CFX00225427 SP_P00007104."
  (interactive)
  (let* ((url-using-proxy nil)
         ;;(url-proxy-services nil)
         (id (or prid
                 (cq-get-pr-id-at-point pos)))
	 realid)
    (when id
      (setq realid (cq-search-id-by-prefix (car id) (nth 1 id)))
      (if realid (cq-open-url (format cq-template-query-pr-by-cfx realid)))
      realid)))

;; (defun cq-open-pr-by-id (&optional prid pos)
;;   "Try to read a pr via if point is on an PR-ID. PRID ist a list with `PREFIX' and `NUMBER'.
;; cq-id = '(PREFIX NUM). CFX00225427 SP_P00007104."
;;   (interactive "sCFX: \n")
;;   (let* ((url-using-proxy nil)
;;          ;;(url-proxy-services nil)
;;          id
;; 	 realid)
;;     (if prid
;;         (setq id (with-temp-buffer
;;                    (insert prid)
;;                    (cq-get-pr-id-from-point (point-min))))
;;       (setq id (cq-get-pr-id-at-point pos)))
;;     (when id
;;       (setq realid (cq-search-id-by-prefix (car id) (nth 1 id)))
;;       (if realid (cq-open-url (format cq-template-query-pr-by-cfx realid)))
;;       realid)))

(defvar cq nil
  "Result of interpretet XML answer for query of one PR:
  '((cqresponse
    ((xmlns . \"http://ibm.com/rational/clearquest/web/v7.1\"))
    (displayname nil \"CFX00042441\")
    (fields nil
            (field nil
                   (fieldname nil \"Tasks\")
                   (datatype nil \"RESOURCE_LIST\")
                   (values nil))
    ...
    )))")
;;(make-variable-buffer-local 'cq)

(defun cq-get-value (pr field)
  (let ((fields (cdr (cdr (nth 2 (cdr (car pr))))))
        fieldname datatype value values)
    (if (catch 'found
          (dolist (elt fields)
            (setq fieldname (nth 2 (assoc 'fieldname elt)))
            (setq datatype (nth 2 (assoc 'datatype elt)))
            (setq values (nth 2 (assoc 'values elt)))
            (setq value (nth 2 (assoc 'value elt)))
            (if (string= fieldname field)
                (throw 'found t))))
        (let ((coding-system 'utf-8))
          (decode-coding-string value coding-system)
          )
      )
    )
  )

(defun cq-read-cq (&optional prid pos)
  "Read PR under cursor into If SP_P or GNATS search PR and open via search result."
  (interactive)
  (let* ((id (or prid (get-text-property (or pos (point)) 'cq-id)))
        (url-using-proxy nil)
        (url-proxy-services nil)
	 realid url buf xml)
    (when id
      (setq realid (cq-search-id-by-prefix (car id) (nth 1 id)))
      (setq url (cq-set-authentication
                 (format
                  "https://champweb.siemens.net/cqweb/restapi/01_CHAMP/CFX/RECORD/%s?recordType=CFXRequest&loginId={loginid}&password={password}&noframes=true"
                  realid)))
      (setq buf (url-retrieve-synchronously url))
      (when (not (null buf))
        (with-current-buffer buf
          ;;(switch-to-buffer buf)
          (re-search-forward "^$" nil 'move)
          (setq xml (xml-parse-region (point) (point-max)))
          (setq cq xml))
        (kill-buffer buf)
        (message "PR %s (%s) [%s] %s"
                 (format "%s%08d" (car id) (nth 1 id))
                 realid
                 (cq-get-value cq "State")
                 (cq-get-value cq "Headline")
                 )
        )
     xml)))

(defun xx ()
  "Test parse xml."
  (interactive)
  (let* (xml url
         (buf (current-buffer)))
    (with-current-buffer buf
      (switch-to-buffer buf)
      ;;(re-search-forward "^$" nil 'move)
      (goto-char (point-min))
      (setq xml (xml-parse-region (point) (point-max)))
      (pp-display-expression xml "*xml*")
      xml)))

(defun cq-insert-title ()
  (interactive)
  (unless cq
    (error "No cq data available."))
  (insert (cq-get-value cq "Headline")))

(defun cq-insert-state ()
  (interactive)
  (unless cq
    (error "No cq data available."))
  (insert (cq-get-value cq "State")))

(defun cq-mouse-2 (event)
  "Move point to the position clicked on with the mouse and query
 the pr at that point.  This should be bound to a mouse click
 event type."
  (interactive "e")
  (let ((position (event-end event)))
    (when (windowp (posn-window position))
      (with-selected-window (posn-window position)
        (goto-char (posn-point (event-start event)))
        (cq-open-pr-by-id nil (posn-point (event-start event)))
        ))))

(defun cq-open-at-point ()
  "Open PR at point. If in org-table and no PR is at point,
serarch forward in current line."
  (interactive)
  (if (cq-get-pr-id-at-point)
      (cq-open-pr-by-id)
    (when (and (derived-mode-p 'org-mode)
               (org-at-table-p))
      (catch 'found
        (save-excursion
          (let ((limit (point-at-eol)))
            (while (and (< (point) limit) (not (eobp)))
              (let ((plist (text-properties-at (point)))
                    (next-change
                     (or (next-property-change (point) (current-buffer))
                         (point-at-eol))))
                (when (cq-get-pr-id-at-point)
                  (cq-open-pr-by-id)
                  (throw 'found t))
                (goto-char next-change)))))))))


(defvar cq-mouse-map (make-sparse-keymap))
(org-defkey cq-mouse-map [mouse-2] 'cq-mouse-2)
;;(org-defkey org-mouse-map [mouse-3] 'org-find-file-at-mouse)
(org-defkey cq-mouse-map [follow-link] 'mouse-face)
;;(org-defkey cq-mouse-map [(tab)] 'cq-open-pr-by-id)
(org-defkey cq-mouse-map "\C-c\C-o" #'cq-open-pr-by-id)
;;(org-defkey oab-mouse-map "\C-i" 'cq-open-pr-by-id)
(add-to-list 'org-open-at-point-functions #'cq-open-at-point)
;;(setq org-open-at-point-functions (delq 'cq-open-pr-by-id org-open-at-point-functions))

(defface cq-pr-face
  '((t (:box (:line-width 1 :color grey40))))
  "The special face for cq ids."
  :group 'cq)

(defvar cq-pr-face 'cq-pr-face
  "Face name to use for cq id's.")

(defun cq-get-pr-id-from-point (beg)
  "Try to read CQ-ID from point. Returns `nil' if there is
no PR-ID at point, otherwise returns the PR as
list (prefix num)."
  (save-match-data
    (save-excursion
      (goto-char beg)
      (let ((case-fold-search t)
	    prefix num id)
	(when (looking-at cq-pr-id-re)
	  (setq prefix (match-string-no-properties 2))
	  (setq num (string-to-number (match-string-no-properties 3)))
	  (setq id (list prefix num)))
	id))))

(defun cq-fontify-pr-ids (limit)
  (condition-case nil
      (cq-fontify-pr-ids-1 limit)
    (error (message "cq fontification error"))))

(defun cq-fontify-pr-ids-1 (limit)
  "Fontify and add text properties for mouse keymap."
  (let ((case-fold-search t))
    (if (re-search-forward cq-pr-id-re limit t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (cq-id (cq-get-pr-id-from-point beg)))
          (org-remove-flyspell-overlays-in beg end)
          (add-text-properties beg end
                               (list 'mouse-face 'highlight
                                     'face 'org-link ;;cq-pr-face
                                     'keymap cq-mouse-map
                                     'help-echo (format "LINK ClearQuest:%s#%s" (car cq-id) (cadr cq-id))
                                     'cq-id cq-id
                                     'rear-nonsticky '(mouse-face keymap help-echo cq-id)))
          (org-rear-nonsticky-at end)
          t))))

(defun cq-font-lock ()
  "Add font locking search function."
  (font-lock-add-keywords
   nil
   (list
    ;; function for applying text-properties
    '(cq-fontify-pr-ids)
    )
   t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Liste der eingescannten Daten
;;
;; Elemente:
;; (action type name release parents-type parents-name parents-release componenet-owner
;;         [ dtype dname drelease dparents-type dparents-name dparents-release dcomponenet-owner ]
;;
;;   action:
;;      'noaction
;;      'add     Füge neuen Datensatz hinzu.
;;               Prüfe, ob der Parent vorhanden ist.
;;      'modify  Liste mit Zielwerten ist angegeben.
;;               type,name,release geändert: Merke die Umbenamung für alle folgenden Aktionen.
;;               parent geändert: Entferne Datensatz von altem Parent und füge neuem Parent hinzu.
;;      'delete  Lösche Daten aus dem Parent, sofern einer vorhanden ist.

(defvar cq-table-actions nil)

(defun cq-combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
	p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
	(setq p (pop ls) v (pop ls))
	(setq rtn (plist-put rtn p v))))
    rtn))

(defun cq-read-table ()
  "Scanne im aktuellen Buffer eine Tabelle mit folgendem Format:

| A | Type | Name | Release | Parents.CompleteName | ComponentOwner | ...
|---+------+------+---------+----------------------+----------------+ ...
|   | S002_Subsystem | SB_BROM | 003.009.000 | S001_System SB_ECC 003.009.000-ECC3.4.0 |  | ...

Wobei Spalte A die auszuführende Aktion beschreibt:
  - ? Datensatz fehlerhaft, Korrektur klären
  - x Neue Configurationunit
  - d Zu löschende Configurationunit
  - m move, Folgezeile in A = .
      Configurationunit die umbenannt wird bzw. einem anderen Parent zugeordnet wird.

Liefere eine Liste mit Property-List Elementen:

 :action          action
 :type            type
 :name            name
 :release         release
 :parents-type    parents-type
 :parents-name    parents-name
 :parents-release parents-release
 :component-owner component-owner
 :lineno          line-nr
optionale Anteile:
 :dtype           dtype
 :dname           dname
 :drelease        drelease
 :dparents-type   dparents-type
 :dparents-name   dparents-name
 :dparents-release dparents-release
 :dcomponent-owner dcomponent-owner


 (action type name release parents-type parents-name parents-release component-owner
         [ dtype dname drelease dparents-type dparents-name dparents-release dcomponent-owner ]

   action:
      'noaction
      'add     Füge neuen Datensatz hinzu.
               Prüfe, ob der Parent vorhanden ist.
      'modify  Liste mit Zielwerten ist angegeben.
               type,name,release geändert: Merke die Umbenamung für alle folgenden Aktionen.
               parent geändert: Entferne Datensatz von altem Parent und füge neuem Parent hinzu.
      'delete  Lösche Daten aus dem Parent, sofern einer vorhanden ist.
"
  (interactive)
  (let ((buf (get-buffer-create "*cqtmp*"))
	actions-list
	source)
    (with-current-buffer buf (erase-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^|[^-].*" nil t)
	(catch 'skip
	  (let ((items (split-string (match-string-no-properties 0) " *?| *"))
		linenum
		action type name release parents componentowner
		parents-type parents-name parents-release
		rename-alist plist)

	    (setq action (nth 1 items)
		  type (nth 2 items)
		  name (nth 3 items)
		  release (nth 4 items)
		  parents (nth 5 items)
		  componentowner (nth 6 items))

	    (if (or (and (string= "" action)
			 (string= "" type))
		    (and (string= "A" action)
			 (string= "Type" type)))
		(throw 'skip nil))

	    (setq parents-type ""
		  parents-name ""
		  parents-release "")

	    (when (string-match "\\([0-9a-zA-Z_().-]+\\) \\([0-9a-zA-Z_().-]+\\) \\([ 0-9a-zA-Z_()<>.-]+\\) *" parents)
	      (if (match-beginning 1)
		  (setq parents-type (match-string-no-properties 1 parents)))
	      (if (match-beginning 2)
		  (setq parents-name (match-string-no-properties 2 parents)))
	      (if (match-beginning 3)
		  (setq parents-release (match-string-no-properties 3 parents))))

	    ;; Spalten: '' Action Type Name Release Parents.Completename ComponentOwner
	    ;; Parents.Completename besteht aus 'Type Name Release'

	    ;;(message "items: %s" (mapcar (lambda (x) (concat "'" x "'")) items))
	    (setq linenum (line-number-at-pos (point)))
	    (with-current-buffer buf
	      (insert (format "# line#%d item: %s\n"
			      linenum (mapconcat (lambda (x) (concat "'" x "'")) items " ")))
	      ;;(insert (format "#         p: '%s' '%s' '%s'\n" parents-type parents-name parents-release))

	      (if (or (and (string= action ".") (null source))
		      (and source (not (string= action "."))))
		  (error "found wrong action in line %d" linenum))

	      (cond
	       ((string= action "d")
		(push (list :lineno linenum
			    :action 'delete :type type :name name :release release
			    :parents-type parents-type :parents-name parents-name :parents-release parents-release :component-owner componentowner)
		      actions-list)
		)
	       ((string= action "x")
		(cq-push-cu type name release)
		(push (list :lineno linenum
			    :action 'add :type type :name name :release release
			    :parents-type parents-type :parents-name parents-name :parents-release parents-release :component-owner componentowner)
		      actions-list)
		)
	       ((string= action "m")
		(cq-push-cu type name release)
		(setq source (list :lineno linenum
			    :action 'modify :type type :name name :release release
			    :parents-type parents-type :parents-name parents-name :parents-release parents-release :component-owner componentowner))
		)
	       ((string= action ".")
		(push (cq-combine-plists source
					 (list :dtype type :dname name :drelease release
					       :dparents-type parents-type :dparents-name parents-name :dparents-release parents-release
					       :dcomponent-owner componentowner))
		      actions-list)
		(setq source nil)
		)
	       ((string= action "")
		(cq-push-cu type name release)
		(push (list :lineno linenum
			    :action 'noaction :type type :name name :release release
			    :parents-type parents-type :parents-name parents-name :parents-release parents-release :component-owner componentowner)
		      actions-list)
		)
	       (t
		(insert (format "# *** warning *** action '%s' unknown\n" action))
		(cq-push-cu type name release)
		;; do nothing
		))
	      )
	    )))


	(with-current-buffer buf
	  (insert (format "\n %s\n" actions-list)))
	)

    (reverse actions-list)))

(defvar cq-all-alist nil
  "Liste aller Configunits.")
(defun cq-push-cu (type name release)
  "Aufnahme der configunit, sofern sie nich bereits in der Liste enthalten ist."
  (let ((cu (format "%s %s %s" type name release))
	)
    (if (not (gethash cu cq-all-alist))
	(puthash cu t cq-all-alist))
  ))

(defun cq-available (type name release)
  "Prüft, ob die CU vorhanden ist."
  (let ((cu (format "%s %s %s" type name release))
	)
    (gethash cu cq-all-alist)))

(defvar cq-rename-alist nil)
(defun cq-push-rename (type name release ntype nname nrelease)
  (let ((cu (format "%s %s %s" type name release))
	)
    (when (not (assoc cu cq-rename-alist))
      (push (cons cu (list ntype nname nrelease)) cq-rename-alist))
      (message "new alist %s" cq-rename-alist)))

(defun cq-get-name (type name release)
  (let* ((cu (format "%s %s %s" type name release))
	 (elem (assoc cu cq-rename-alist)))
    (if elem
	(cdr elem)
      (list type name release))))

(defun cq-convert-table ()
  (interactive)

  (if (null cq-all-alist)
      (setq cq-all-alist (make-hash-table :test 'equal))
    (clrhash cq-all-alist))

  (let ((actions-list (cq-read-table))
	(buf (get-buffer-create "*cqtmp2*"))
	items)

    (with-current-buffer buf

      (setq cq-rename-alist nil)
      (erase-buffer)

      (while (setq items (pop actions-list))
	(let* ((action (plist-get items :action))
	       (cu (cq-get-name (plist-get items :type) (plist-get items :name) (plist-get items :release)))
	       (type (nth 0 cu))
	       (name (nth 1 cu))
	       (release (nth 2 cu))
	       (parents (cq-get-name (plist-get items :parents-type) (plist-get items :parents-name) (plist-get items :parents-release)))
	       (parents-type (nth 0 parents))
	       (parents-name (nth 1 parents))
	       (parents-release (nth 2 parents))
	       (componentowner (plist-get items :component-owner))
	       (lineno (plist-get items :lineno))
	       p)

	  (cond
	   ((eq action 'add)

	    (insert (format "# #%d add '%s %s %s' '%s' '%s %s %s'\n"
			    lineno
			    type name release componentowner
			    parents-type parents-name parents-release))

	    (insert (format "set component-owner \"%s\"\n" componentowner))
	    (insert (format "create-cu \"%s\" \"%s\" \"%s\"\n" ;; description
			    type name release))

	    ;; *FIX* check if parent exist
	    (if (or (not (string= parents-type ""))
		    (not (string= parents-name ""))
		    (not (string= parents-release "")))
		(insert (format "add-child \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\"\n"
				parents-type parents-name parents-release
				type name release)))

	    (insert "\n\n")
	    )
	   ((eq action 'delete)

	    (insert (format "# #%d delete '%s %s %s' '%s' '%s %s %s'\n"
			    lineno
			    type name release componentowner
			    parents-type parents-name parents-release))

	    ;; *FIX* check if parent exist
	    (if (not (and (string= parents-type "")
			  (string= parents-name "")
			  (string= parents-release "")))
		(insert (format "remove-child \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\"\n"
				parents-type parents-name parents-release
				type name release)))

	    (insert "\n\n")
	    )
	   ((eq action 'modify)
	    (let* ((dcu (cq-get-name (plist-get items :dtype) (plist-get items :dname) (plist-get items :drelease)))
		   (dtype (nth 0 dcu))
		   (dname (nth 1 dcu))
		   (drelease (nth 2 dcu))
		   (dparents (cq-get-name (plist-get items :dparents-type) (plist-get items :dparents-name) (plist-get items :dparents-release)))
		   (dparents-type (nth 0 dparents))
		   (dparents-name (nth 1 dparents))
		   (dparents-release (nth 2 dparents))
		   (dcomponentowner (plist-get items :dcomponent-owner))

		   (new-parent (not (and (string= parents-type dparents-type)
					 (string= parents-name dparents-name)
					 (string= parents-release dparents-release))))

		   (rename (not (and (string= type dtype)
			      (string= name dname)
			      (string= release drelease)
			      (string= componentowner dcomponentowner))))
		   )

	      ;; erst child neuem parent zuordnen, dann child umbenennen
	      (when new-parent
		;; attach configunit to new parent
		(insert (format "# #%d change parent '%s %s %s'\n#               '%s %s %s' -> '%s %s %s'\n"
				lineno
				type name release
				parents-type parents-name parents-release
				dparents-type dparents-name dparents-release))

		(insert (format "remove-child \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\"\n"
				parents-type parents-name parents-release
				type name release))
		(if (not rename)
		    (insert (format "add-child \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\"\n"
				    dparents-type dparents-name dparents-release
				    type name release)))
		)

	      (when rename
		;; rename configunit
		(insert (format "# #%d rename '%s %s %s %s' -> '%s %s %s %s'\n"
				lineno
				type name release componentowner
				dtype dname drelease dcomponentowner))
		(if (cq-available dtype dname drelease)
		    (insert "#   destination already exists.\n")
		  (insert (format "change-cu \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\"\n"
				  type name release dtype dname drelease dcomponentowner))
		  (cq-push-rename type name release dtype dname drelease))

		;; Den neuen Configunit-Namen dem Parent zuordnen.
		(insert (format "add-child \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\"\n"
				dparents-type dparents-name dparents-release
				dtype dname drelease)))

	      (insert "\n")
	      ))
	 (t
	  ;; no action
	  ))


	)



      ))

    ))

;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup clearcase nil
  "Options specific for ClearCase."
  :group 'clearcase
  :tag "ClearCase")

;;    Pfad = "ClearCase:Hackbrett/_section/cu-boards/ecc/_manuals/caruso_sw_interface.doc",
(defcustom clearcase-link-re
  "\\(ClearCase:\\([a-zA-Z0-9-äüöÄÖÜ_/\\\\.][a-zA-Z0-9-äöüÄÖÜ_ /\\\\.]+\\)\\)"
  "Pfadangabe für Dateien im ClearCase"
  :group 'clearcase
  :type 'regexp)

(defcustom clearcase-views '()
  "List of dirs of ClearCase views."
  :group 'clearcase
  :type '(choice
	  (repeat :tag "List of dirs of clearcase views." directory)
	  (file :tag "Store list of dirs a file\n" :value "~/.clearcase_views")))

(require 'font-lock)

(defface clearcase-file-face
  '((t (:box (:line-width 1 :color grey40))))
  "The special face for clearcase links."
  :group 'clearcase)

(defvar clearcase-mouse-map (make-sparse-keymap))
(org-defkey clearcase-mouse-map [mouse-2] #'clearcase-follow-link)
(org-defkey clearcase-mouse-map [follow-link] 'mouse-face)
(org-defkey clearcase-mouse-map [(tab)] #'clearcase-open-link-at-point)

(defvar clearcase-file-face 'clearcase-file-face
  "Face name to use for links to clearcase elements.")

(defun clearcase-get-filename-from-point (beg end)
  "Try to read filename from point. Returns `nil' if there is
no filename at point, otherwise returns the filename."
  (save-match-data
    (save-excursion
      (goto-char beg)
      (let ((case-fold-search nil)
	    name)
	(when (looking-at clearcase-link-re)
	  (setq name (match-string-no-properties 2)))
	name))))

(defun clearcase-fontify-links (limit)
  (condition-case nil
      (clearcase-fontify-links-1 limit)
    (error (message "clearcase fontification error"))))

(defun clearcase-fontify-links-1 (limit)
  "Fontify and add text properties for mouse keymap."
  (if (re-search-forward clearcase-link-re limit t)
      (let* ((beg (match-beginning 0))
	     (end (match-end 0))
	     (name (clearcase-get-filename-from-point beg end)))
	(org-remove-flyspell-overlays-in beg end)
	(add-text-properties beg end
			     (list 'mouse-face 'highlight
				   'face 'org-link ;;cq-pr-face
				   'help-echo (format "LINK ClearCase:%s" name)
				   'keymap clearcase-mouse-map
				   'clearcase-fname name
                                   'rear-nonsticky '(mouse-face keymap help-echo clearcase-fname)))
	(org-rear-nonsticky-at end)
	t)))

(defun clearcase-font-lock ()
  "Add font locking search function."
  (font-lock-add-keywords
   nil
   (list
    ;; function for applying text-properties
    '(clearcase-fontify-links)
    )
   t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Follow "ClearCase:" file links.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clearcase-split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.
No empty strings are returned if there are matches at the beginning
and end of string."
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
	  (and (eq (match-beginning 0) (match-end 0))
	       (eq (match-beginning 0) start))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (or (eq start (length string))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun clearcase-read-dir-list (views &optional pair-with-expansion)
  "Read the list of files from a file.
If PAIR-WITH-EXPANSION is t return pairs with un-expanded
filenames, used by `org-store-new-agenda-file-list' to write back
un-expanded file names."
  (when (file-directory-p views)
    (error "`clearcase-views' cannot be a single directory"))
  (when (stringp views)
    (with-temp-buffer
      (insert-file-contents views)
      (mapcar
       (lambda (f)
	 (let ((e (expand-file-name (substitute-in-file-name f))))
	   (if pair-with-expansion
	       (cons e f)
	     e)))
       (clearcase-split-string (buffer-string) "[ \t\r\n]*?[\r\n][ \t\r\n]*")))))

(defvar clearcase-window-config nil)
(make-local-variable 'clearcase-window-config)

(defun clearcase-quit-buffer (&optional button)
  (let ((cur (current-buffer)))
    (if button
        (message (format "Button [%s]" (button-label button))))
    (if clearcase-window-config
	(set-window-configuration clearcase-window-config))
    (kill-buffer cur))
)

(define-button-type 'clearcase-quit-button
  'action 'clearcase-quit-buffer
  'follow-link t
  'face 'custom-button
  'help-echo "Close window")

(defun clearcase-open-link-via-buffer ()
  "Try to read a link via if point is on an ClearCase file link."
  (interactive)
  (let* ((fname (get-text-property (point) 'clearcase-fname))
	 (orig-buf (current-buffer))
	 (orig-pos (point))
	 (cc-views (cond
		    ((stringp clearcase-views) (clearcase-read-dir-list clearcase-views))
		    ((listp clearcase-views) clearcase-views)
		    (t (error "`clearcase-views' does not contain a valid list of views")))
		   )
	out win)
    (if (get-buffer "*clearcase-follow*") (kill-buffer "*clearcase-follow*"))
    (setq out (get-buffer-create "*clearcase-follow*"))
    (setq win (get-buffer-window out))
    (with-current-buffer out
      (let ((inhibit-read-only t)
	    ;; Don't generate undo entries for creation of the initial contents.
	    (buffer-undo-list t)
            (map (make-sparse-keymap))
	    view)

        (define-key map "q" 'clearcase-quit-buffer)
        (use-local-map map)

	(erase-buffer)

	(setq clearcase-window-config (current-window-configuration))
	(insert-text-button " QUIT " :type 'clearcase-quit-button)
	(insert (format "  List of file %s in different ClearCase views:\n\n" fname))

	(while (setq view (pop cc-views))
	  (insert (format "View [[file:%s][%s]]\n" view view))
	  (cond
	   ((file-directory-p view)
	    (if (file-exists-p (format "%s/%s" view fname))
		(insert (format "  [[file:%s/%s][%s]]\n" view fname fname))
	      (insert (format "  %s does not exist\n" fname))))
	   (t
	    (insert (format "  does not exist\n"))))
	  (insert "\n")))

      (set-buffer-modified-p nil)
      (setq buffer-read-only t))

    (delete-other-windows win)
    (split-window)
    (switch-to-buffer out)
    (org-mode)

    ;; ;; Change keymaps
    ;; (goto-char (point-min))
    ;; (while (not (eobp))
    ;;   (let ((plist (text-properties-at (point)))
    ;; 	    (next-change
    ;; 	     (or (next-property-change (point) (current-buffer))
    ;; 		 (point-max))))
    ;; 	(add-text-properties (point) next-change
    ;; 			     (list 'keymap nil
    ;; 				   ))
    ;; 	(goto-char next-change)))

    ;; Make the window the right size
    (goto-char (point-min))
    (org-fit-window-to-buffer)))

(defun clearcase-open-link-candidates (fname)
  "Return a list of found pathes where FNAME ist found. FNAME is
the file name with given path starting from ClearView root."
  (interactive)
  (let ((cc-views (cond
		   ((stringp clearcase-views) (clearcase-read-dir-list clearcase-views))
		   ((listp clearcase-views) clearcase-views)
		   (t (error "`clearcase-views' does not contain a valid list of views")))
		  )
	list-of-views)
    (while (setq view (pop cc-views))
      (if (and (file-directory-p view)
	       (file-exists-p (format "%s/%s" view fname)))
          (push view list-of-views)))
    list-of-views))


(if (eq system-type 'windows-nt)
    (progn
      (defun w32-browser (file)
        "Run default Windows application associated with FILE.
If no associated application, then `find-file' FILE."
        (interactive "fFile: ")
        (or (condition-case nil
                (w32-shell-execute nil file) ; Use Windows file association
              (error nil))
            (find-file file)))              ; E.g. no Windows file association
      )
  (defun w32-browser (file)
    "Run default Windows application associated with FILE.
If no associated application, then `find-file' FILE."
    (interactive "fFile: ")
    (find-file file))
  )


(defun clearcase-open-link--find-file (candidate)
  "Open given file externally."
  (with-ivy-window
    (w32-browser (subst-char-in-string ?/ ?\\ candidate))))

(defun clearcase-open-link (fname)
  "Open ClearCase files from different views."
  (let ((candidates (sort
                     (clearcase-open-link-candidates fname)
                     #'string<)))
    (if candidates
        (ivy-read (format "Open File '%s' in view: " fname) candidates
                  :action (lambda (d)
                            (clearcase-open-link--find-file
                             (concat (file-name-as-directory d)
                                     fname)))
                  :caller 'clearcase-open-link-ivy)
      (message "No view with '%s' found." fname))))

(defun clearcase-open-link-at-point ()
  "Open ClearCase file at point."
  (interactive)
  (if (get-text-property (point) 'clearcase-fname)
      (clearcase-open-link (get-text-property (point) 'clearcase-fname))))

(defun clearcase-follow-link (event)
  "Move point to the position clicked on with the mouse and query
 the pr at that point.  This should be bound to a mouse click
 event type."
  (interactive "e")
  (let ((position (event-end event)))
    (when (windowp (posn-window position))
      (with-selected-window (posn-window position)
        (goto-char (posn-point (event-start event)))
        (clearcase-open-link-at-point))))
        )

(add-to-list 'org-open-at-point-functions #'clearcase-open-link-at-point)

;; --------------------------------------------------------------------------------

;; Queryergebnisse die als Textdatei aus ClearQuest Weboberfläsche exportiert werden
;; sind in UTF-8 codiert.
(modify-coding-system-alist 'file "QueryResult.*\\.txt\\'" 'utf-8-dos)

;; --------------------------------------------------------------------------------

;;items: ('' '' '' '<10>' '' '' '')
;;items: ('' 'CFXID' 'State' 'RequestType' 'Category' 'Headline' '')
;;items: ('' 'CFX00014322' 'Postponed' 'Change Request, internal' 'Software' 'K32+: Fehlender outbyte() in der Postmortemschleife für Emulator' '')

(defun cq-table-get-columns (buffer)
  "Extract columns of ClearQuest result table in buffer. Search
  for a line which contains \"CFXID\"."
  (let (columns cfx-column-found)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (and (not cfx-column-found)
                    (< (point) (point-max)))
          (re-search-forward ".*?|[ ]*?\\(CFXID\\)[ ]*?|.*" nil t)
          (setq cfx-column-found (string= (match-string 1) "CFXID")))
        (if (not cfx-column-found)
          (message "No CFXID headline in table '%s' found"
                   (buffer-name buffer)))
        (let ((items (split-string (match-string-no-properties 0) " *?| *"))
              (column 0)
              col)
          (while (setq col (pop items))
            (when (not (string= col ""))
              (push (cons column col) columns))
            (setq column (1+ column))))
        ))
    columns))

;; http://www.emacswiki.org/emacs/ElispCookbook
(defun cq-table-scan-buffer (buffer hash-buffer &optional prs)
  "Scan BUFFER as table with CQ entries. Return a list of all found PR-Ids and COLUMNS."
  (let ((all-prs prs)
        (columns (nreverse (cq-table-get-columns buffer)))
        col-cfxid)
    (setq col-cfxid (car (rassoc "CFXID" columns)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^|[^-].*" nil t)
          (catch 'skip
            (let* ((items (split-string (match-string-no-properties 0) " *?| *"))
                   (cfx (nth col-cfxid items))
                   (column 0)
                  col)
              (when (or (string= cfx "") (string= cfx "CFXID"))
                (throw 'skip nil))

              (when (not (member cfx all-prs))
                (push cfx all-prs))

              (let ((cols columns)
                    plist)
                (while cols
                  (let* ((item (car cols))
                         (colname (cdr item))
                         (colnum (car item)))
                    (setq plist (plist-put plist
                                           (intern (concat ":" colname))
                                           (nth colnum items))))
                  (setq cols (cdr cols)))
                (puthash cfx plist hash-buffer))
              )
            )
          )
        )
      )
    (list all-prs columns)
    )
  )


(defvar cq-table-params '(("Headline" (:downcase t :shorten t :column-width "<80>"))
                          ("RequestType" (:column-width "<10>"))
                          ("Category" (:column-width "<10>"))
                          ("SafetyRelevant" (:column-width "<5>"))
                          ("Safety?" (:column-width "<5>"))
                          ("Rejected?" (:column-width "<5>")))
  "List of table column names with properties. The following
  properties will be handled:

 `:downcase'    if t the compare between the entries in this column will done in same case
 `:shorten'     if t the compare will only be done on minimal length of both entries
 `:column-with' string will be put at table top, e.g. \"<10>\" to
 restrict the width of the column.")

(defun cq-table-diff (buffer-A buffer-B &optional table-params)
  "Make diff of two files which contain the query result of
ClearQuest formatted as org-table. Generate diff file in new
temporary buffer. If `table-params' not given uses
`cq-table-params'."
  (interactive
   (let (bf)
     (list (setq bf (read-buffer "Buffer A to compare: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Buffer B to compare: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (let ((buf (get-buffer-create "*cqtmp*"))
        (hash-buffer-A (make-hash-table :test 'equal))
        (hash-buffer-B (make-hash-table :test 'equal))
        columns-A columns-B columns
        all-prs
        pr
        ipos
        rc)
    (unless table-params
      (setq table-params cq-table-params))
    (setq rc (cq-table-scan-buffer buffer-A hash-buffer-A))
    (setq all-prs (nth 0 rc)
          columns-A (nth 1 rc))
    (setq rc (cq-table-scan-buffer buffer-B hash-buffer-B all-prs))
    (setq all-prs (nth 0 rc)
          columns-B (nth 1 rc))

    (setq all-prs (sort all-prs 'string<))
    (with-current-buffer buf
      (widen)
      (goto-char (point-max))
      (if (not (eolp)) (insert-before-markers "\n"))
      (org-mode)
      (insert-before-markers
       "-*- mode: org; coding: utf-8-unix; -*-\n"
       (format-time-string "Added: [%Y-%m-%d %a]\n\n")
       (format "+++ %s\n" (buffer-file-name (get-buffer buffer-A)))
       (format "--- %s\n" (buffer-file-name (get-buffer buffer-B)))
       "\n"
       "Legende:\n"
       "di: diff result marker\n"
       "  \" \" PR is contained in both files\n"
       "  \"+\" PR is contained only in file \"+++\"\n"
       "  \"-\" PR is contained only in file \"---\"\n"
       "  If PR is changed between the two files, the content of \"+++\" is given \n"
       "    with \"+\" marker and the changed fields are shown with a second line with \"-\" marker.\n"
       "\n"
       )

      (setq ipos (point))
      ;; build merged columns list
      (let ((col columns-A)
            colname)
        (while col
          (setq colname (cdar col))
          (push colname columns)
          (setq col (cdr col)))
        (setq col columns-B)
        (while col
          (setq colname (cdar col))
          (when (not (rassoc colname columns-A))
            (push colname columns))
          (setq col (cdr col))))
      (setq columns (reverse columns))

      ;; insert special table-property line
      (insert-before-markers "|")
      (mapc #'(lambda (colname)
                (let ((params (assoc colname table-params))
                      (value ""))
                  (if params
                      (setq value (or (plist-get (cadr params) :column-width) "")))
                  (insert-before-markers (format "|%s" value))
                ))
            columns)
      (insert-before-markers "|\n")

      ;; insert columns headers
      (insert-before-markers "| di ")
      (mapc #'(lambda (colname)
                (insert-before-markers (format "|%s" colname))
                )
            columns)
      (insert-before-markers "|\n|-\n")

      ;;(message "all prs %s" all-prs)
      (while (setq pr (pop all-prs))
        (let* ((plist-A (gethash pr hash-buffer-A))
               (plist-B (gethash pr hash-buffer-B))
               (prs-differ-in-content nil)
               plist
               prefix
               )

          (if (and plist-A plist-B)
              ;; PR ist in beiden Tabellen enthalten
              (progn
                ;; Prüfe, ob unterschiedliche Feldinhalte existieren
                (setq prefix " "
                      plist plist-A)
                (let ((cols columns)
                      current params
                      value-A value-B vv-A vv-B)
                  (while (and cols (not prs-differ-in-content))
                    (setq current (car cols)
                          value-A (plist-get plist-A (intern (concat ":" current)))
                          value-B (plist-get plist-B (intern (concat ":" current))))
                    (when (and value-A value-B)
                      (setq vv-A value-A
                            vv-B value-B
                            params (assoc current table-params))

                      (when (and params (plist-get (cadr params) :downcase))
                        (setq vv-A (downcase vv-A)
                              vv-B (downcase vv-B)))

                      (when (and params (plist-get (cadr params) :shorten))
                        (let ((len (min (length vv-A) (length vv-B))))
                          (setq vv-A (substring vv-A 0 len)
                                vv-B (substring vv-B 0 len))))

                      (when (not (string= vv-A vv-B))
                        (setq prs-differ-in-content t))
                      )
                    (setq cols (cdr cols)))))
            (if plist-A
                (setq prefix " +"
                      plist plist-A)
              (setq prefix " -"
                    plist plist-B)))

          (if prs-differ-in-content
              (progn
                ;; Ausgabe:
                ;; + CFXID Werte aus plist-A
                ;; -       Werte aus plist-B, wenn verschieden zu plist-A
                (insert-before-markers (format "| +"))
                (let ((cols columns)
                      colname value-A value-B)
                  (while cols
                    (setq colname (car cols)
                          value-A (plist-get plist-A (intern (concat ":" colname))))
                    (insert-before-markers (format "|%s" (or value-A "")))
                    (setq cols (cdr cols)))
                  (insert-before-markers "|\n")
                  (insert-before-markers (format "| -"))
                  (setq cols columns)
                  (while cols
                    (setq colname (car cols)
                          value-A (plist-get plist-A (intern (concat ":" colname)))
                          value-B (plist-get plist-B (intern (concat ":" colname))))
                    (if (and (not (string= colname "CFXID"))
                             (not (string= value-A value-B)))
                        (insert-before-markers (format "|%s" (or value-B "")))
                      (insert-before-markers "|"))
                    (setq cols (cdr cols)))
                  (insert-before-markers "|\n")
                  )
                )
            ;; Ausgabe:
            ;; PREFIX CFXID Werte aus der entsprechenden plist
            (insert-before-markers (format "|%s" prefix))
            (let ((cols columns)
                  colname value)
              (while cols
                (setq colname (car cols)
                      value (plist-get plist (intern (concat ":" colname))))
                (insert-before-markers (format "|%s" (or value "")))
                (setq cols (cdr cols))))
            (insert-before-markers "|\n"))
          )
        )
      (insert-before-markers "|-\n")
      (save-excursion
        (backward-delete-char 1) ;; letzte '\n' löschen
        (goto-char ipos)
        (skip-chars-forward "^|") ;; Tabelleanfang suchen
        (org-table-align))
      (org-mode)
      )
    (switch-to-buffer buf)
    (goto-char (point-min))
    )
  )

(defun cq-convert-table-xxx (buffer-A &optional table-params)
  "Make diff of two files which contain the query result of
ClearQuest formatted as org-table. Generate diff file in new
temporary buffer. If `table-params' not given uses
`cq-table-params'."
  (interactive
   (let (bf)
     (list (setq bf (read-buffer "Buffer A to compare: "
				 (other-buffer "") t))
	   )))
  (let ((buf (get-buffer-create "*cqtmp*"))
        (hash-buffer-A (make-hash-table :test 'equal))
        columns-A columns-B columns
        all-prs
        pr
        ipos
        rc)
    (unless table-params
      (setq table-params cq-table-params))
    (setq rc (cq-table-scan-buffer buffer-A hash-buffer-A))
    (setq all-prs (nth 0 rc)
          columns-A (nth 1 rc))

    (setq all-prs (sort all-prs 'string<))
    (with-current-buffer buf
      (widen)
      (goto-char (point-max))
      (if (not (eolp)) (insert-before-markers "\n"))
      (org-mode)

      (setq ipos (point))

      ;; build columns list
      (let ((col columns-A)
            colname)
        (while col
          (setq colname (cdar col))
          (push colname columns)
          (setq col (cdr col))))
      (setq columns (reverse columns))

      ;; insert special table-property line
      (insert-before-markers "|")
      (mapc #'(lambda (colname)
                (let ((params (assoc colname table-params))
                      (value ""))
                  (if params
                      (setq value (or (plist-get (cadr params) :column-width) "")))
                  (insert-before-markers (format "|%s" value))
                ))
            columns-A)
      (insert-before-markers "|\n")

      (insert-before-markers "|")
      (mapc #'(lambda (colname)
                (insert-before-markers (format "|%s" colname))
                )
            columns-A)
      (insert-before-markers "|\n|-\n")

      ;;(message "all prs %s" all-prs)
      (while (setq pr (pop all-prs))
        (let* ((plist (gethash pr hash-buffer-A))
               (prs-differ-in-content nil)
               (prefix "")
               )

            ;; Ausgabe:
            ;; PREFIX CFXID Werte aus der entsprechenden plist
            (insert-before-markers (format "|%s" prefix))
            (let ((cols columns)
                  colname value sp_p cfxid)
              (while cols
                (setq sp_p (plist-get plist (intern (concat ":" "SP_P")))
                      cfxid nil)
                (if (and sp_p (not (string= sp_p "")))
                    (setq cfxid (format "SP_P%08d" (string-to-number sp_p))))
                (setq colname (car cols)
                      value (plist-get plist (intern (concat ":" colname))))
                (if (and (string= colname "CFXID")
                         cfxid)
                    (setq value cfxid))
                (insert-before-markers (format "|%s" (or value "")))
                (setq cols (cdr cols))))
            (insert-before-markers "|\n"))
          )

      (insert-before-markers "|-\n")
      (save-excursion
        (backward-delete-char 1) ;; letzte '\n' löschen
        (goto-char ipos)
        (skip-chars-forward "^|") ;; Tabelleanfang suchen
        (org-table-align))
      )
    (switch-to-buffer buf)
    (goto-char (point-min))
    )
  )

(defun cq-orgtbl-to-orgtbl (table &rest params)
  "Speed up orgtbl-to-orgtbl by calling it only if params are
given, otherwise do convert without interpretation of cell content."
  (if params
      (orgtbl-to-orgtbl table params)
    (org-trim
     ;; Return TABLE as Org syntax.  Tolerate non-string cells.
     (with-output-to-string
       (dolist (e table)
         (cond ((eq e 'hline) (princ "|--\n"))
               ((consp e)
                (princ "| ") (dolist (c e) (princ c) (princ " |"))
                (princ "\n")))))
     )
    )
  )

(defun cq-format-result (&optional table-params)
  "Format a ClearQuest result got from ClearQuest web
frontend. If `table-params' not given uses `cq-table-params'."
  (interactive)
  (let* ((with-region (region-active-p))
         (start (if with-region (region-beginning) (point-min)))
         (end (if with-region (region-end) (point-max)))
         (cfx-column 0)
         (org-table-convert-region-max-lines 10000))
    (unless with-region (set-buffer-file-coding-system 'utf-8-unix))
    (org-table-convert-region start end "|")
    (goto-char start)
    (unless with-region (insert "-*- mode: org; coding: utf-8-unix; -*-\n"))
    (insert (format-time-string "Abfrage: [%Y-%m-%d %a]\n\n"))
    (org-table-begin)
    (let ((beg (point))
          (end (progn (save-excursion (org-table-end))))
          (table (org-table-to-lisp))
          new-table top-line)
      (unless table-params
        (setq table-params cq-table-params))
      (dolist (entry table)
        (cond
         ((eq entry 'hline)
          (push entry new-table))
         (t
          (if (member "CFXID" entry)
              (progn

                ;; build table line with properties
                (setq top-line
                      (mapcar #'(lambda (colname)
                                  (let ((params (assoc colname table-params))
                                        (value ""))
                                    (if (string= "CFXID" colname) (setq cfx-column (incf cfx-column)))
                                    (if params
                                        (setq value (or (plist-get (cadr params) :column-width) "")))
                                    value)
                                  )
                              entry))

                (setq top-line (append '("") top-line))
                ;;(push (append '("") top-line) new-table)

                ;; zusätzliche Spalte einfügen
                (push (append '("res") entry) new-table)
                (push 'hline new-table))
            ;; zusätzliche Spalte einfügen
            (push (append '("x") entry) new-table))
          )))
      (setq table (nreverse new-table))
      (delete-region beg end)

      (insert "#+TBLNAME: ")
      (insert (format-time-string "%Y%m%d%H%M%S"))
      (when (fboundp 'uuid-string)
        (insert "-" (uuid-string)))
      (insert "\n")

      ;; *TODO* Export mittels orgtbl-to-orgtbl geschieht mittels backend, welches
      ;; die <???> Spalten/Zeilen herausfiltert. Deshlab die topline hier per Hand eintragen.
      (mapc #'(lambda (col)
                (insert (format "| %s " col)))
             top-line)
      (insert
       "\n"
       (cq-orgtbl-to-orgtbl table)
       "\n")
      )
    (when cfx-column
      (goto-char (org-table-end))
      (org-table-previous-field)
      ;; cfx-column um eins vergrößern, da oben eine zusätzliche Spalte eingebaut wurde
      (org-table-goto-column (1+ cfx-column))
      (if with-region (deactivate-mark))
      (org-table-sort-lines nil ?a)
      )
    (goto-char start)
    (org-mode))
  )

(require 'xml)
(defun cq-search-and-open-url-xml (pr url)
  "If SP_P or GNATS search PR and open via search result."
  (let ((url-using-proxy nil)
        (url-proxy-services nil)
        (buf (url-retrieve-synchronously (cq-set-authentication url)))
	count url-list xml id)
    (when (not (null buf))

      (switch-to-buffer buf)

      (with-current-buffer buf
	(goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (setq xml (xml-parse-region (point) (point-max)))
        (xml-print xml)

        
;; TODO
;;	(if (re-search-forward "The document hast moved.*<address>.* at \\([a-zA-Z:.]+\\) Port \\([0-9]+\\)</address>" nil t)

	(when (re-search-forward "<oslc_cm:totalCount.*?>\\([0-9]+\\)</oslc_cm:totalCount>" nil t)
	    (setq count (string-to-number (match-string 1)))

	    (cond
	     ((= count 1)

	      (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
		  (setq id (match-string 1)))
	      (if (re-search-forward "<link rel=\"alternate\" href=\"\\(.*\\)\"/>" nil t)
		  (progn
		    (message "open %s -> %s" pr id)
		    (cq-open-url (format cq-template-query-pr-by-cfx id))))
	      )
	     ((> count 1)

	      (while (> count 0)
		(let (id url)
		  (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
		      (setq id (match-string 1)))
		  (if (re-search-forward "<link rel=\"alternate\" href=\"\\(.*\\)\"/>" nil t)
		      (setq url (match-string 1)))

		  (push (cons id (list id url)) url-list))
		(setq count (1- count)))

	      (let* ((all-ids (mapcar (function
				       (lambda (x)
					(car x))) url-list))
		     (id (ido-completing-read "PR: " all-ids))
		     url)
		(setq url (cdr (assoc id all-ids)))
		(if url
		    (message "open %s -> %s" pr id)
		    (cq-open-url (format cq-template-query-pr-by-cfx id))))
	      )
	     (t
	      (message "no PR found")
	      ;;
	      ))))

      (kill-buffer buf)
      )))

(defun cq-deescape-string (string)
  "Convert STRING into a string containing valid XML character data.
Replace occurrences of &<>'\" in STRING with their default XML
entity references (e.g. replace each & with &amp;).

XML character data must not contain & or < characters, nor the >
character under some circumstances.  The XML spec does not impose
restriction on \" or ', but we just substitute for these too
\(as is permitted by the spec)."
  (with-temp-buffer
    (insert string)
    (dolist (substitution '(("&amp;" . "&")
			    ("&lt;" . "<")
			    ("&gt;". ">")
			    ("&apos;". "'")
			    ("&quot;" . "\"")))
      (goto-char (point-min))
      (while (search-forward (car substitution) nil t)
	(replace-match (cdr substitution) t t nil)))
    (buffer-string)))

(defun cq-get-columns (buffer)
  "Extract columns of ClearQuest result table in buffer."
  (let (columns)
    (with-current-buffer buffer
      (goto-char (point-min))
      (when (re-search-forward "^.*|" nil t)
        (beginning-of-line)
        (let ((items (split-string (match-string-no-properties 0) " *?| *"))
              (column 0)
              col)
          (while (setq col (pop items))
            (when (not (string= col ""))
              (push (cons column col) columns))
            (setq column (1+ column))))
        ))
  (nreverse columns)))

(defun cq-read-columns (&optional buffer)
  "Extract columns of ClearQuest result table in buffer. Column
line will be searched beginning at point."
  (let ((buffer (or buffer (current-buffer)))
        columns)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^.*?|.*" nil t)
            (progn
              (beginning-of-line)
              (let ((items (split-string (match-string-no-properties 0) " *?| *"))
                    (column 0)
                    col)
                (while (setq col (pop items))
                  (when (not (string= col ""))
                    (push (cons column col) columns))
                  (setq column (1+ column))))
              )
          (error "No headline found"))))
    (nreverse columns)))

(defun cq-scan-buffer (buffer)
  "Scan BUFFER as table with CQ entries. Return a list of all found PR-Ids and COLUMNS."
  (let (entries columns)
    (with-current-buffer buffer
      (save-excursion
        (setq columns (cq-get-columns buffer))
        (forward-line)
        (while (re-search-forward "^[^|][^-].*" nil t)
          (catch 'skip
            (let* ((items (split-string (match-string-no-properties 0) " *?| *"))
                   (column 0)
                  col)

              (let ((cols columns)
                    plist)
                (while cols
                  (let* ((item (car cols))
                         (colname (cdr item))
                         (colnum (car item)))
                    (setq plist (plist-put plist
                                           (intern (concat ":" colname))
                                           (nth colnum items))))
                  (setq cols (cdr cols)))
                (push plist entries))
              )
            )
          )
        ))
    (nreverse entries)))

(eval-when-compile
  (require 'cl))
(require 'cl-macs)
;;
;; topological sort, see
;; http://rosettacode.org/wiki/Topological_sort#Common_Lisp
;;
(defun* cq-topological-sort (graph &key (test 'eql))
  "Graph is an association list whose keys are objects and whose
values are lists of objects on which the corresponding key depends.
Test is used to compare elements, and should be a suitable test for
hash-tables.  Topological-sort returns two values.  The first is a
list of objects sorted toplogically.  The second is a boolean
indicating whether all of the objects in the input graph are present
in the topological ordering (i.e., the first value)."
  (let ((entries (make-hash-table :test test)))
    (cl-flet ((entry (v)
             "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
	     (or (gethash v entries)
		 (puthash v (cons 0 '()) entries))))
      ;; populate entries initially
      (dolist (gvertex graph)
        (destructuring-bind (vertex &rest dependencies) gvertex
          (let ((ventry (entry vertex)))
            (dolist (dependency dependencies)
              (let ((dentry (entry dependency)))
                (unless (funcall test dependency vertex)
		  (incf (car ventry))
                  (push vertex (cdr dentry))))))))
      ;; L is the list of sorted elements, and S the set of vertices
      ;; with no outstanding dependencies.
      (let ((L '())
            (S (loop for entry being each hash-value of entries
                     using (hash-key vertex)
                     when (zerop (car entry)) collect vertex)))
        ;; Until there are no vertices with no outstanding dependencies,
        ;; process vertices from S, adding them to L.
        (do* () ((endp S))
          (let* ((v (pop S)) (ventry (entry v)))
            (remhash v entries)
            (dolist (dependant (cdr ventry) (push v L))
              (when (zerop (decf (car (entry dependant))))
                (push dependant S)))))
        ;; return (1) the list of sorted items, (2) whether all items
        ;; were sorted, and (3) if there were unsorted vertices, the
        ;; hash table mapping these vertices to their dependants
        (let ((all-sorted-p (zerop (hash-table-count entries))))
          (values (nreverse (nreverse L))
                  all-sorted-p
                  (unless all-sorted-p
                    entries)))))))

;; (defun print-elements-recursively (list)
;;   "Print each element of LIST on a line of its own.
;; Uses recursion."
;;   (print (car list))                  ; body
;;   (if list                            ; do-again-test
;;       (print-elements-recursively     ; recursive call
;;        (cdr list))))                  ; next-step-expression

;; (defun flatten (mylist)
;;   (cond
;;    ((null mylist) nil)
;;    ((atom mylist) (list mylist))
;;    (t
;;     (append (flatten (car mylist)) (flatten (cdr mylist))))))

(defvar cq-convert-config-dep-org-debug nil)
(defun cq-print-recursively (level value alist)
  (let (subelt debug-log)
    (when (not (string= value ""))
      (setq subelt (copy-list (cdr (assoc-string value alist))))
      (dotimes (i level)
        (insert "*"))
      ;;(message "value=%s subelt=%s" value subelt)

      (when cq-convert-config-dep-org-debug
        (setq debug-log (get-buffer-create "*cqdebug*"))
        (when (string= value "S003_Component SB_Manager 002.008.000")
          (with-current-buffer debug-log
            (goto-char (point-max))
            (insert "#Manager#\n" (pp subelt)))))

      (insert (format " %s\n" value))

      (setq subelt
            (sort subelt
                  #'(lambda (a b)
                      (string< (downcase a) (downcase b)))
                  ))

      (dolist (elt subelt)
        (unless (string= elt "")
          (cq-print-recursively (1+ level) elt alist)))
      ))
  )

(defun cq-convert-config-dep-org ()
  "Convert given list of ConfigUnits of ClearQuest to dependency list of this ConfigUnits. Following columnas are needed:
- CompleteName or Type, Name, Release
- Children.CompleteName."
  (interactive)
  (let* ((entries (cq-scan-buffer (current-buffer)))
         (cu-hash (make-hash-table :test 'equal))
         cu cu-child hash-elt
         depends sorted-graph column-name
         debug-log)

    (when cq-convert-config-dep-org-debug
      (setq debug-log (get-buffer-create "*cqdebug*"))
      )

    ;; (:CompleteName "P001_ProductGroup SB_Plattformen -" :Children\.CompleteName #1#)
    ;; (:Type "P001_ProductGroup" :Name "SB_Plattformen" :Release "-" :Children\.CompleteName #1#)
    (unless entries
      (error "No entries found"))
    (cond
     ((member :CompleteName (car entries))
      (setq column-name 'completename)
      )
     ((and
       (member :Type (car entries))
       (member :Name (car entries))
       (member :Release (car entries)))
      (setq column-name 'type-name-release)
      )
     )
    (when (null column-name)
      (error "No columns \"CompleteName\" found"))
    (unless (member :Children.CompleteName (car entries))
      (error "No column \"Children.CompleteName\" found"))

    (dolist (elt entries)
      (cond
       ((eq column-name 'completename)
        (setq cu (plist-get elt :CompleteName))
        )
       ((eq column-name 'type-name-release)
        (setq cu (concat (plist-get elt :Type)
                         " "
                         (plist-get elt :Name)
                         " "
                         (plist-get elt :Release)))
        )
       )
      (setq cu-child (plist-get elt :Children.CompleteName)
            hash-elt (gethash cu cu-hash))
      (when (or (null hash-elt)
                (not (member cu-child hash-elt)))
        (setq hash-elt (push cu-child hash-elt))
        (puthash cu hash-elt cu-hash)))

    (maphash
     #'(lambda(key value)
         (let ((elt (list key value)))
           (push elt depends)))
     cu-hash)

    (setq sorted-graph (cq-topological-sort depends))
    (when (null sorted-graph)
      (error "No contend found"))
    (let ((sorted (nth 1 sorted-graph))
          (unsorted-elems (nth 2 sorted-graph)))
      (when (not sorted)
        (message "Found unsorted elems: %s" unsorted-elems)
        ))

    (if (get-buffer "*cqtmp*")
        (kill-buffer "*cqtmp*"))
    (with-current-buffer (get-buffer-create "*cqtmp*")
      (let ((graph (car sorted-graph))
            alist first-elems included-elems)

        (when cq-convert-config-dep-org-debug
          (with-current-buffer debug-log
            (goto-char (point-max))
            (insert "#Graph#\n" (pp graph))))

        ;;(("SYS A 1" ("SUBSYS BB 1" "SUBSYS AA 1") "SUBSYS AA 1" ("KOMP AA 1") "SUBSYS BB 1" ("KOMP BB 1")) t nil)
        (let (elt value)
          (while (setq elt (pop graph))
            (setq value (pop graph)
                  alist (cons (cons elt value) alist))
            (dolist (elem value)
              (unless (member elem included-elems)
                (push elem included-elems)))
            (push elt first-elems))
          )

        (org-mode)
        ;; (insert "* Graph\n")
        ;; (mapc #'(lambda (key)
        ;;           (insert (format "** %s\n  %s\n" key (cdr (assoc-string key alist)))))
        ;;       first-elems)

        ;; remove all elements from first-elem which are included in other elements
        (mapc #'(lambda (key)
                  (when (member key first-elems)
                    ;;(message "delete %s" key)
                    (setq first-elems (delete key first-elems))))
              included-elems)

        (when cq-convert-config-dep-org-debug
          (with-current-buffer debug-log
            (goto-char (point-max))
            (insert "#First-Elemes#\n" (pp first-elems)))

          (with-current-buffer debug-log
            (goto-char (point-max))
            (insert "#AList#\n" (pp alist))))

        (insert "* Dependencies\n")
        ;;(insert (format "%s\n\n\n" first-elems))
        (setq first-elems
              (sort first-elems
                    #'(lambda (a b)
                        (string< (downcase a) (downcase b)))
                    ))
        (mapc #'(lambda (key)
                  (cq-print-recursively 2 key alist))
              first-elems)
      ))
    (switch-to-buffer "*cqtmp*")))

(defun cq-yy ()
  "If SP_P or GNATS search PR and open via search result."
  (interactive)
  (let* ((url (cq-set-authentication
               "https://champweb.siemens.net/cqweb/#/01_CHAMP/CFX/QUERY/Public%20Queries/Projects_RD/SP_Platforms/A_Base/Simis_Basis/Queries/06a_All_ConfigUnits&loginId={loginid}&password={password}&noframes=true"))
         (buf (url-retrieve-synchronously url))
	count url-list xml tree node node-name children attributes parsed-tree)
    (when (not (null buf))
      (switch-to-buffer buf)
      (re-search-forward "^$" nil 'move)
      (setq xml (xml-parse-region (point) (point-max)))
      (goto-char (point-min))
      (xml-print xml)
      (insert "\n\n")
      )))

(defun cq-xxx ()
  "If SP_P or GNATS search PR and open via search result."
  (interactive)
  (let* ((url (cq-set-authentication
               "https://champweb.siemens.net/cqweb/restapi/01_CHAMP/CFX/RECORD/CFX0042441?recordType=CFXRequest&loginId={loginid}&password={password}&noframes=true"))
         (buf (url-retrieve-synchronously url))
	count url-list xml tree node node-name children attributes parsed-tree)
    (when (not (null buf))
      (switch-to-buffer buf)
      (re-search-forward "^$" nil 'move)
      (setq xml (xml-parse-region (point) (point-max)))
      (goto-char (point-min))
      ;;(xml-print xml)
      (insert "\n\n")
      (insert (pp-to-string xml))
      (insert "\n\n")
      (setq tree xml)
      ;; (unless (string= (xml-node-name tree) "cqresponse")
      ;;   (error "No cqresponse answer"))
      ;; (setq tree (xml-node-children tree))
      (dolist (node tree)
        (setq node-name (xml-node-name node))
        (when (string= node-name "cqresponse")
          (setq attributes (xml-node-attributes node))
          (setq children (xml-node-children node))
          (when (not (null children))
            (dolist (child children)
              (setq node-name (xml-node-name child))
              (setq attributes (xml-node-attributes child))
              (setq children (xml-node-children child))
              (cond
               ((string= node-name "displayname")
                )
               ((string= node-name "fields")
                (dolist (fields children)
                  (setq node-name (xml-node-name fields))
                  (setq attributes (xml-node-attributes fields))
                  (setq children (xml-node-children fields))
                  (when (string= node-name "field")
                    (let (fieldname datatype value)
                      (dolist (field children)
                        (setq node-name (xml-node-name field))
                        (setq attributes (xml-node-attributes field))
                        (setq children (xml-node-children field))
                        (cond
                         ((string= node-name "fieldname")
                          (setq fieldname (car children))
                          )
                         ((string= node-name "datatype")
                          (setq datatype (car children))
                          )
                         ((string= node-name "value")
                          (setq value (cq-deescape-string (car children)))
                          )
                         )
                        )
                      (push `(:fieldname ,fieldname :datatype ,datatype :value ,value) parsed-tree)
                      )
                    )
                  )))))))
      (insert "\n\n" (format "%s" parsed-tree) "\n\n")

      )))

;;       <cqresponse xmlns="http://ibm.com/rational/clearquest/web/v7.1">
;;   <displayname>CFX00042441</displayname>
;;   <fields>
;;     <field>
;;       <fieldname>ChangeImpact</fieldname>
;;       <datatype>MULTILINE_STRING</datatype>
;;       <value>[
;;     &quot;&quot;
;; ]</value>
;;     </field>

(defun cq-xx ()
  (interactive)
  (let ((url "https://champweb.siemens.net/cqweb/restapi/01_CHAMP/CFX/RECORD/CFX00194370?&loginId={loginid}&password={password}&recordType=CFXRequest&noframes=true"))
    (cq-search-and-open-url-xml "CFX00194370" url)))

(defconst cq-table-border-regexp "^[ \t]*|[ \t]"
  "Searching a table (any type) this finds the first line of the table.")

(defun cq-column-number-of-colname (colname headline)
  "Find `colname' in `headline'. Return column number or nil."
  (cond
   ((string= "" colname)
    nil)
   ((member colname headline)
    (catch 'return
      (let ((col 0) found-col)
        (dolist (entry headline)
          (if (and (not (string= entry ""))
                   (string= colname (nth col headline)))
              (throw 'return col))
          (setq col (1+ col)))
        )
      nil))
   (t nil)
   ))

(require 'ert)
(ert-deftest cq/test-cq-column-number-of-colname ()
  "Test der Funktion `cq-column-number-of-colname'."
  (interactive)
  (let ((headline '("" "abc" "CFXID" "doedel"))
        col)
    (should (= 1 (cq-column-number-of-colname "CFXID" headline)))
    (should (null (cq-column-number-of-colname "husel" headline)))
    (should (null (cq-column-number-of-colname "" headline)))))
;;(ert "cq/test-cq-column-number-of-colname")

;; (defun xx ()
;;   (interactive)
;;   (cq-table-merge-column "20141127-atavus.org" "RES" "20141128-sys-diff.org"))

;; examples: http://orgmode.org/worg/org-contrib/babel/examples/lob-table-operations.html
;; Ideen und code von: https://github.com/tbanel/orgtbljoin.git

(defface cq-face
  '((t nil))
  "`cq'."
  :version "25.1"
  :group 'cq)

(defface cq-changed-face
  '((default
      :inherit cq-face)
    (((class color) (min-colors 88) (background light))
     :background "#ffdddd")
    (((class color) (min-colors 88) (background dark))
     :background "#553333")
    (((class color))
     :foreground "red"))
  "`cq-merge' face used to highlight changed values."
  :group 'cq)

;; (defun xxcq--merge-query-column (prompt table)
;;   "Interactively query a column.
;; PROMPT is displayed to the user to explain what answer is expected.
;; TABLE is the org mode table from which a column will be choosen
;; by the user.  Its header is used for column names completion.  If
;; TABLE has no header, completion is done on generic column names:
;; $1, $2..."
;;   (while (eq 'hline (car table))
;;     (setq table (cdr table)))
;;   (org-icompleting-read
;;     prompt
;;     (if (memq 'hline table) ;; table has a header
;; 	(car table)
;;       (let ((i 0))
;; 	(mapcar (lambda (x) (format "$%s" (setq i (1+ i))))
;; 		(car table))))))

(defun cq--merge-query-column (prompt headline table)
  "Interactively query a column.
PROMPT is displayed to the user to explain what answer is expected.
TABLE is the org mode table from which a column will be choosen
by the user.  Its header is used for column names completion.  If
TABLE has no header, completion is done on generic column names:
$1, $2..."
  (completing-read
    prompt
    (if headline ;; table has a header
	headline
      (let ((i 0))
	(mapcar (lambda (x) (format "$%s" (setq i (1+ i))))
		(car table))))))

(defun cq--scan-org-table (buffer)
  "Check if point is within org-table. If this is the case, read
it and convert it to a lisp table."
  (with-current-buffer buffer
;;    (org-table-check-inside-data-field)
    (save-restriction
      ;;(widen)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward cq-table-border-regexp nil t)
            (progn
              (goto-char (org-table-begin))
              (let ((beg (point))
                    (end (progn (save-excursion (org-table-end)))))
                (org-table-to-lisp)))
          (error "No org-table found in buffer %s" buffer)
          )
        )
      )
    )
  )

(defun cq--get-headline-from-table (table)
  "Check if table contains a headline. Skip formatting parameters
before headline. Return headline of table or nil."
  ;; skipt 'hlines before headline
  (while (eq 'hline (car table))
    (setq table (cdr table)))
  (if (memq 'hline table) ;; table has a header
      ;; skip formatting line before headline
      (catch 'found
        (dolist (row table)
          (cond
           ((eq row 'hline)
            ;; no headline
            (throw 'found nil))
           ((eq (cadr table) 'hline)
            ;; headline found
            (setq table (car table))
            (throw 'found table))
           (t
            (setq table (cdr table)))
           )
          )
        )
    )
  )

(require 'ert)
(ert-deftest cq/test-cq--get-headline-from-table ()
  "Test der Funktion `cq--get-headline-from-table'."
  (interactive)
  (let ((formatline '("" "<10>" "" ""))
        (headline '("" "abc" "CFXID" "doedel"))
        (data '("a" "b" "c" "d"))
        )
    (should (eq headline
                (cq--get-headline-from-table
                 (list formatline headline 'hline data data))))
    (should (eq headline
                (cq--get-headline-from-table
                 (list 'hline 'hline headline 'hline data data))))
    (should (null
                (cq--get-headline-from-table
                 (list 'hline 'hline headline data data))))
    (should (eq headline
                (cq--get-headline-from-table
                 (list headline 'hline data data))))
    (should (null
                (cq--get-headline-from-table
                 (list formatline headline data data))))
    )
  )
;;(ert "cq/test-cq--get-headline-from-table")

(defun new-cq-table-merge-column ()
  "Merge one column of table from buffer-A as new left column to
table in buffer-B. Same table rows are identified by fields of
column `CFXID'. Rows of buffer-A which are not contained in
buffer-B are added at the end. There is only one table in each
buffer allowed. buffer-A content is assumed to newer than
buffer-B content and so used to fill."
  (interactive)
  (let ((buffer-A (read-buffer "Buffer A (newer) with column to merge: "
                               (ediff-other-buffer "") t))
        table-A
        headline-A
        column
        buffer-B
        )
    (setq table-A (cq--scan-org-table buffer-A))
    (setq headline-A (cq--get-headline-from-table table-A))
    ;; (if (null headline-A)
    ;;     (error "No headline found in table"))
    (setq column (cq--merge-query-column
                  "Column to merge: "
                  headline-A table-A))

    (setq buffer-B (read-buffer "Buffer B (older) for merge in: "
                                (progn
                                  ;; realign buffers so that two visible bufs will be
                                  ;; at the top
                                  (save-window-excursion (other-window 1))
                                  (ediff-other-buffer buffer-A))
                                t))


    )
  )

(defun cq-table-merge-column (buffer-A column buffer-B)
  "Merge one column of table from buffer-A as new left column to
table in buffer-B. Same table rows are identified by fields of
column `CFXID'. Rows of buffer-A which are not contained in
buffer-B are added at the end. There is only one table in each
buffer allowed. buffer-A content is assumed to newer than
buffer-B content and so used to fill."
  (interactive
   (let (bf)
     (list (setq bf (read-buffer "Buffer A (newer) with column to merge: "
				 (ediff-other-buffer "") t))
           (read-string "Column to merge: ")
	   (read-buffer "Buffer B (older) for merge in: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (let ((hash-buffer-A (make-hash-table :test 'equal))
        beg-table-B end-table-B
        table-A table-B
        headline-A headline-B
        cfxid-column-A column-A
        cfxid-column-B
        cfxids-seen
        column-B-in-A
        last-line)

    (with-current-buffer buffer-A
      (save-restriction
        ;;(widen)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward cq-table-border-regexp nil t)
              (progn
                (goto-char (org-table-begin))
                (let ((beg (point))
                      (end (progn (save-excursion (org-table-end)))))
                  (setq table-A (org-table-to-lisp))))
            (error "No org-table found in buffer %s" (buffer-name buffer-A))
            ))))

    (with-current-buffer buffer-B
      (save-restriction
        ;;(widen)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward cq-table-border-regexp nil t)
              (progn
                (goto-char (org-table-begin))
                (setq beg-table-B (point)
                      end-table-B (progn (save-excursion (org-table-end))))
                (setq table-B (org-table-to-lisp)))
            (error "No org-table found in buffer %s" buffer-B)
            ))))

    ;; find column and CFXID in table-A
    (catch 'skip
      (let (col)
        (dolist (entry table-A)
          (cond
           ((eq entry 'hline)
            (throw 'skip nil))
           (t
            (if (setq col (cq-column-number-of-colname "CFXID" entry))
                (setq cfxid-column-A col
                      headline-A entry))
            (if (setq col (cq-column-number-of-colname column entry))
                (setq column-A col))
            )))))
    (if (null cfxid-column-A)
        (error "No CFXID column found in buffer %s" buffer-A))
    (if (null column-A)
        (error "No %s column found in buffer %s" column buffer-A))

    ;; collect row data
    (dolist (entry table-A)
      (cond
       ((eq entry 'hline))
       (t
        (puthash (nth cfxid-column-A entry)
                 entry hash-buffer-A)
            )))

    ;; find CFXID in table-B
    (catch 'skip
      (let ((col 0))
        (dolist (entry table-B)
          (cond
           ((eq entry 'hline)
            (throw 'skip nil))
           (t
            (if (setq col (cq-column-number-of-colname "CFXID" entry))
                (setq cfxid-column-B col
                      headline-B entry))
            )))))

    (if (null cfxid-column-B)
        (error "No CFXID column found in buffer %s" buffer-B))

    ;; build transformation for column-B in column-A
    ;; column-B-in-A[column-B] ==> column-A or nil
    (setq column-B-in-A
          (mapcar #'(lambda (col)
                      (cq-column-number-of-colname col headline-A))
                  headline-B))

    (with-current-buffer buffer-B
      (save-excursion
        (delete-region beg-table-B end-table-B)
        (goto-char beg-table-B)

        ;; insert new table
        (dolist (entry table-B)
          (setq last-line entry)
          (cond
           ((eq entry 'hline)
            (insert-before-markers "|-\n"))
           (t
            (let ((cfxid (nth cfxid-column-B entry))
                  row-A
                  (value "")
                  )
              (when cfxid
                (push cfxid cfxids-seen)
                (setq row-A (gethash cfxid hash-buffer-A))
                (if row-A (setq value (nth column-A row-A)))
                )

              (insert-before-markers
               (format "| %s " value))

              (let ((col 0))
                (mapc #'(lambda (v)

                          (when (and row-A
                                     (nth col column-B-in-A))
                            (let* ((col-A (nth col column-B-in-A))
                                   (val-A (nth col-A row-A)))
                              (unless (string= v val-A)
                                (setq v val-A)
                                ;; nicht möglich in Tabelle, da diese selbst Text properties setzt
                                ;; (put-text-property 0 (length v)
                                ;;                    'font-lock-face 'cq-changed
                                ;;                    v)
                                )
                              ))

                          (insert-before-markers
                           (format "| %s " v))

                          (incf col)
                          )
                       entry))

              ;; (mapconcat #'(lambda (v)
              ;;                 (format "| %s " v))
              ;;             entry "")
              (insert-before-markers "\n")
              ))))

        ;; append all entries to buffer-B of table-A which are not contained in table-B
        (let (use-entry)
          (dolist (entry table-A)
            (cond
             ((eq entry 'hline)
              (setq use-entry t))
             (use-entry
              (let ((cfxid (nth cfxid-column-A entry)))
                (when (and cfxid
                         (not (member cfxid cfxids-seen)))
                  ;; insert dividing rule if last entry is not such one
                  (when last-line
                    (when (not (eq entry 'hline))
                      (insert-before-markers "|-\n"))
                    (setq last-line nil)
                    )
                  ;; find elements in row which can be inserted into table

                  (insert-before-markers (format "| %s " (nth column-A entry)))
                  (dotimes (col (length headline-B))
                    (let (col-A)
                      (if (setq col-A (nth col column-B-in-A))
                          (insert-before-markers (format "| %s " (nth col-A entry)))
                        (insert-before-markers "| ")))
                    )
                  (insert-before-markers "\n")
                   ;; (mapconcat #'(lambda (unsed)
                   ;;                (let ((col-A (nth col column-B-in-A))
                   ;;                      (col (1+ col)))
                   ;;                  (format "| %s " (if col-A (nth col-A entry) ""))
                   ;;                  )
                   ;;               )
                   ;;            headline-B "") ;; headline-B just as side effect, content not used
                   ;; "\n")
                  )
                )))))

        ;;(backward-delete-char 1) ;; letzte '\n' löschen
        ;; Back to beginning, align the table, recalculate if necessary
        (goto-char beg-table-B)
        (skip-chars-forward "^|") ;; Tabellenanfang suchen
        (org-table-align)
        )
      (goto-char beg-table-B)
      )
    (switch-to-buffer buffer-B)
  ))




;; Make a readable representation of the invocation sequence for FUNC-DEF.
;; It would either be a key or M-x something.
(defun cq-format-bindings-of (func-def)
  (let ((desc (car (where-is-internal func-def
				      overriding-local-map
				      nil nil))))
    (if desc
	(key-description desc)
      (format "M-x %s" func-def))))

;; Select the lowest window on the frame.
(defun cq-select-lowest-window ()
  (if (featurep 'xemacs)
      (select-window (frame-lowest-window))
    (let* ((lowest-window (selected-window))
	   (bottom-edge (car (cdr (cdr (cdr (window-edges))))))
	   (last-window (save-excursion
			  (other-window -1) (selected-window)))
	   (window-search t))
      (while window-search
	(let* ((this-window (next-window))
	       (next-bottom-edge
		(car (cdr (cdr (cdr (window-edges this-window)))))))
	  (if (< bottom-edge next-bottom-edge)
	      (setq bottom-edge next-bottom-edge
		    lowest-window this-window))
	  (select-window this-window)
	  (when (eq last-window this-window)
	    (select-window lowest-window)
	    (setq window-search nil)))))))

(defmacro cq-buffer-live-p (buf)
  `(and ,buf (get-buffer ,buf) (buffer-name (get-buffer ,buf))))


(defface cq-cursor-face
  '((t (:inverse-video t)))
  "The face used for fake cursors"
  :group 'multiple-cursors)

(defun cq-make-cursor-overlay-inline (pos)
  "Create overlay to look like cursor inside text."
  (let ((overlay (make-overlay pos (1+ pos) nil nil nil)))
    (overlay-put overlay 'face 'cq-cursor-face)
    overlay))

(defun cq-remove-fake-cursor (o)
  "Delete overlay."
  (delete-overlay o))

(defun cq-table-merge-this-column ()
  "Merge current column of table to another column in table."
  (interactive)
  (let ((pop-up-windows t)
        (this-buf (current-buffer))
        (this-buf-name (buffer-name (current-buffer)))
        window-config wind other-wind
        column-A col-A-line col-A-ov
	column-B col-B-pos
        beg-table end-table table
        )

    ;; check if in table and get column
    (org-table-check-inside-data-field) ;; else error
    (setq column-A (org-table-current-column)) ;; else error

    ;; save table position and read table
    (save-excursion
      (goto-char (org-table-begin))
      (setq beg-table (point)
            end-table (progn (save-excursion (org-table-end))))
      (setq table (org-table-to-lisp)))

    (setq col-A-line (line-number-at-pos))
    (setq col-A-ov (cq-make-cursor-overlay-inline (point)))
    (org-table-goto-column (1+ column-A))

    ;; save window config an split window to show help
    (setq window-config (current-window-configuration))
    (setq wind (get-buffer-window this-buf 'visible))
    (select-window wind)
    (delete-other-windows)
    (split-window-vertically)
    (cq-select-lowest-window)
    (setq other-wind (selected-window))

    ;; show help buffer
    (with-temp-buffer
      (erase-buffer)
      (insert
       (format "\n   *******  Mark table destination column to merge in buffer %s (or confirm the existing one)  *******\n"
	       this-buf-name)
       (format "\n\t      When done, type %s       Use %s to abort\n    "
               (cq-format-bindings-of 'exit-recursive-edit)
               (cq-format-bindings-of 'abort-recursive-edit)))
      (goto-char (point-min))
      (set-window-buffer other-wind (current-buffer))
      (shrink-window-if-larger-than-buffer)
      (if (window-live-p wind)
	  (select-window wind))

      (condition-case nil
          (recursive-edit)
        (exit
         (message "received exit"))
        (quit
         ;; handle user abortion
         (set-window-configuration window-config)
         (org-table-goto-column column-A)
         (cq-remove-fake-cursor col-A-ov)
         (error "Aborted merge by user")))
      )

    (if (not (cq-buffer-live-p this-buf))
        (error "Buffer %s faded away" this-buf-name))
    (unless (eq (window-buffer) this-buf)
      (set-window-configuration window-config)
      (org-table-goto-column column-A)
      (cq-remove-fake-cursor col-A-ov)
      (error "Not in buffer %s" this-buf-name))
    (set-buffer this-buf)

    ;; chek position
    (when (null (org-table-check-inside-data-field t))
      (set-window-configuration window-config)
      (org-table-goto-column column-A)
      (cq-remove-fake-cursor col-A-ov)
      (error "Not in table data field"))

    (setq column-B (org-table-current-column))
    (setq col-B-pos (point))

    ;; restore windows
    (set-window-configuration window-config)
    (org-table-goto-column column-A)
    (cq-remove-fake-cursor col-A-ov)

    (if (or (< col-B-pos beg-table)
            (> col-B-pos end-table))
        (error "Second position not in same table."))
    (if (= column-A column-B)
        (error "Please choose different columns."))

    (message "Merge column %s->%s" column-A column-B)

    (setq column-A (1- column-A))
    (setq column-B (1- column-B))
    (save-excursion
      (delete-region beg-table end-table)
      (goto-char beg-table)

      ;; insert new table
      (dolist (entry table)
        (cond
         ((eq entry 'hline)
          (insert-before-markers "|-\n"))
         (t
          (let ((col-A (nth column-A entry))
                (col-B (nth column-B entry))
                value col
                )
            (cond
             ((string= col-A col-B)
              (setq value col-A))
             ((string= col-A "")
              (setq value col-B))
             ((string= col-B "")
              (setq value col-A))
             (t
              (setq value (concat col-B "," col-A))))

            (setq col 0)
            (dolist (elt entry)
              (cond
               ((= col column-A))
               ((= col column-B)
                (insert-before-markers (format "| %s " value)))
               (t
                (insert-before-markers (format "| %s " elt))))
              (setq col (1+ col)))
            (insert-before-markers "\n")
            ))))

      ;;(backward-delete-char 1) ;; letzte '\n' löschen
      ;; Back to beginning, align the table, recalculate if necessary
      (goto-char beg-table)
      (skip-chars-forward "^|") ;; Tabellenanfang suchen
      (org-table-align)
      )
    (goto-char (point-min))
    (forward-line (1- col-A-line))
    (message "Merge column %s->%s done" column-A column-B)
    ))

(defun cq-table-duplicate-column ()
  "Duplicate current column in table."
  (interactive)
  (let (column-A col-A-line col-A-ov
        beg-table end-table table
        )

    ;; check if in table and get column
    (org-table-check-inside-data-field) ;; else error
    (setq column-A (org-table-current-column)) ;; else error

    ;; save table position and read table
    (save-excursion
      (goto-char (org-table-begin))
      (setq beg-table (point)
            end-table (progn (save-excursion (org-table-end))))
      (setq table (org-table-to-lisp)))

    (setq col-A-line (line-number-at-pos))

    (message "Duplicate column %s" column-A)

    (setq column-A (1- column-A))
    (save-excursion
      (delete-region beg-table end-table)
      (goto-char beg-table)

      ;; insert new table
      (dolist (entry table)
        (cond
         ((eq entry 'hline)
          (insert-before-markers "|-\n"))
         (t
          (let ((col 0))
            (dolist (elt entry)
              (insert-before-markers (format "| %s " elt))
              (if (= col column-A)
                  (insert-before-markers (format "| %s " (nth col entry))))
              (setq col (1+ col)))
              (insert-before-markers "\n")))))

      ;;(backward-delete-char 1) ;; letzte '\n' löschen
      ;; Back to beginning, align the table, recalculate if necessary
      (goto-char beg-table)
      (skip-chars-forward "^|") ;; Tabellenanfang suchen
      (org-table-align)
      )
    (goto-char (point-min))
    (forward-line (1- col-A-line))
    (message "Duplicate column %s done" column-A)
    ))

(defun cq-table-remove-duplicates ()
  "Remove duplicate lines with same CFXID from current table."
  (interactive)
  (let (col-A-line
        beg-table end-table table
        cfxid-column
        found-cxfids found-hline)

    ;; check if in table and get column
    (org-table-check-inside-data-field) ;; else error

    ;; save table position and read table
    (save-excursion
      (goto-char (org-table-begin))
      (setq beg-table (point)
            end-table (progn (save-excursion (org-table-end))))
      (setq table (org-table-to-lisp)))

    (setq col-A-line (line-number-at-pos))

    ;; find CFXID in table
    (catch 'skip
      (let ((col 0))
        (dolist (entry table)
          (cond
           ((eq entry 'hline)
            (throw 'skip nil))
           (t
            (if (setq cfxid-column (cq-column-number-of-colname "CFXID" entry))
                (throw 'skip nil)
            ))))))

    (if (null cfxid-column)
        (error "No CFXID column found in table in buffer %s"
               (buffer-name (current-buffer))))

    (message "Remove duplicate CFXIDs...")

    (save-excursion
      (delete-region beg-table end-table)
      (goto-char beg-table)

      ;; insert new table
      (dolist (entry table)
        (cond
         ((eq entry 'hline)
          (insert-before-markers "|-\n")
          (setq found-hline t))
         (t
          (let ((col 0)
                (cfxid (nth cfxid-column entry))
                (doit t))
            (if (and
                 found-hline
                 (not (string= cfxid "")))
                (if (member cfxid found-cxfids)
                    (setq doit nil)
                  (push cfxid found-cxfids)
                  (setq doit t)))
            (when doit
              (dolist (elt entry)
                (insert-before-markers (format "| %s " elt))
                )
              (insert-before-markers "\n"))))))

        ;;(backward-delete-char 1) ;; letzte '\n' löschen
        ;; Back to beginning, align the table, recalculate if necessary
        (goto-char beg-table)
        (skip-chars-forward "^|") ;; Tabellenanfang suchen
        (org-table-align)
        )
    (goto-char (point-min))
    (forward-line (1- col-A-line))
    (message "Removed duplicates")))

(defun cq-table-export-column ()
  "Export content of one column from current table."
  (interactive)
  (let (column-A
        beg-table end-table table
        found-cxfids found-hline
        (buf (get-buffer-create "*cqtmp*"))
	)

    ;; check if in table and get column
    (org-table-check-inside-data-field) ;; else error
    (setq column-A (org-table-current-column)) ;; else error

    ;; save table position and read table
    (save-excursion
      (goto-char (org-table-begin))
      (setq beg-table (point)
            end-table (progn (save-excursion (org-table-end))))
      (setq table (org-table-to-lisp)))

    (message "Export column %d..." column-A)

    (with-current-buffer buf

      (setq column-A (1- column-A))
      (dolist (entry table)
        (cond
         ((eq entry 'hline)
          (setq found-hline t))
         (t
          (let ((col 0)
                (value (nth column-A entry))
                (doit t))
            (if (and
                 found-hline
                 (not (string= value "")))
                (if (member value found-cxfids)
                    (setq doit nil)
                  (push value found-cxfids)
                  (setq doit t)))
            (insert-before-markers (format "%s\n" value)))))))
    (message "Export column %d done" column-A)
    (switch-to-buffer buf)))

(defconst cq-state-order
  '(("Submitted" . 0)
    ("Analysed" . 1)
    ("Assigned" . 2)
    ("Resolved" . 3)
    ("Verified" . 4)
    ("Validated" . 5)
    ("Postponed" . 6)
    ("Rejected" . 7)
    ("Closed" . 8)))

(defun cq--sort-state< (a b)
  (let ((ai (cdr (assoc a cq-state-order)))
        (bi (cdr (assoc b cq-state-order))))
    (cond
     ((and ai bi)
      (< ai bi))
     ((null ai)
      t)
     ((null bi)
      nil))))

;; (cq--sort-state< "Closed" "Submitted") nil
;; (cq--sort-state< "Rejected" "Closed") t
;; (cq--sort-state< "Submitted" "Analysed") t
;; (cq--sort-state< "doedel" "Submitted") t
;; (cq--sort-state< "Submitted" "doedel") nil
;; (cq--sort-state< "doedel" "doedel") t

(defun cq-org-table-sort-state ()
  "Sort table column as `State' column."
  (interactive)
  (let ((inhibit-read-only t))
    (org-table-sort-lines nil ?f #'org-sort-remove-invisible #'cq--sort-state<)))


;;;###autoload
(defun cq-find-file-hook ()
  "Check if loaded file is alist of problem reports. In this case
format the content."
  (when (and (buffer-file-name)
             (string-match "QueryResult-?.*\\.txt$"
                           (buffer-file-name)))
    (let ((gc-cons-threshold 10000000)
          (garbage-collection-messages nil))
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (if (string-match "CFXID" (buffer-substring-no-properties
                                   (point) (point-at-eol)))
            (progn
              (message "Convert query result to table...")
              (cq-format-result))
          (if (string-match "Type|Name|Release" (buffer-substring-no-properties
                                                 (point) (point-at-eol)))
              (progn
                (message "Convert config unit hierarchy to org...")
                (cq-convert-config-dep-org)
                (goto-char (point-min)))
            )
          )
        )
      (read-only-mode -1)
      )
    )
  )

(defun orgtbl--create-table-full-outer-joined (mastable mascol reftable refcol)
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
        (mascols)
        (masform)
        (mashead)
        (seenkeys))
    ;; skip any hline a the top of both tables
    (while (eq (car mastable) 'hline)
      (setq result (cons 'hline result))
      (setq mastable (cdr mastable)))
    (while (eq (car reftable) 'hline)
      (setq reftable (cdr reftable)))
    ;; convert column-names to numbers
    (setq mascol (orgtbl--join-colname-to-int mascol mastable))
    (setq refcol (orgtbl--join-colname-to-int refcol reftable))
    ;; convert reference table into fast-lookup hashtable
    (setq reftable (orgtbl--join-convert-to-hashtable reftable refcol)
	  refhead (car reftable)
	  refhash (cdr reftable))
    ;; (if (= (length refhead) 2)
    ;;     (setq refform refhead
    ;;           refhead (cadr refhead)))
    ;; iterate over master table header if any
    ;; and join it with reference table header if any
    (if (memq 'hline mastable)
	(while (listp (car mastable))
          (unless mascols
            (setq mascols (length (car mastable))))
	  (setq result
		(cons (orgtbl--join-append-mas-ref-row
		       (car mastable)
		       (and refhead (car refhead))
		       refcol)
		      result))
	  (setq mastable (cdr mastable))
	  (if refhead
	      (setq refhead (cdr refhead)))))
    ;; create the joined table
    (mapc (lambda (masline)
	    (if (not (listp masline))
		(setq result (cons masline result))
	      (let ((result0 result))
                (push (nth mascol masline) seenkeys)
		;; if several ref-lines match, all of them are considered
		(mapc (lambda (refline)
			(setq result
			      (cons
			       (orgtbl--join-append-mas-ref-row
				masline
				refline
				refcol)
			       result)))
		      (gethash (nth mascol masline) refhash))
		;; if no ref-line matches, add the non-matching master-line anyway
		(if (eq result result0)
		    (setq result (cons masline result))))))
	  mastable)
    ;; now all rows from reftable which where not used
    (maphash (lambda (key value)
               (unless (member key seenkeys)
                 (let ((mline) (i 0))
                   (while (< i mascols)
                     (if (= i mascol)
                         (setq mline (cons key mline))
                       (setq mline (cons "" mline)))
                     (incf i))
                   (setq mline (nreverse mline))
                   (mapc (lambda (refline)
                           (setq result
                                 (cons
                                  (orgtbl--join-append-mas-ref-row
                                   mline
                                   refline
                                   refcol)
                                  result)))
                         (gethash key refhash))
                   )))
             refhash)
    (nreverse result)))


;;;###autoload
(defun cq-table-join-tables (master-buffer column ref-buffer)
  "Join given two tables. Master table comes from master-buffer,
 reference table comes from ref-buffer. Key colum is named column
 in both tables.  Result will overwrite master table."
  (interactive
   (let (bf)
     (list (setq bf (read-buffer "Buffer A (master) with column to merge: "
				 (ediff-other-buffer "") t))
           (read-string "Column to merge: ")
	   (read-buffer "Buffer B (ref) for merge in: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (let (beg-master end-master
        master-table ref-table
        joined-table
        )

    (with-current-buffer master-buffer
      (save-restriction
        ;;(widen)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward cq-table-border-regexp nil t)
              (progn
                (goto-char (org-table-begin))
                (setq beg-master (point)
                      end-master (progn (save-excursion (org-table-end))))
                (setq master-table (org-table-to-lisp)))
            (error "No org-table found in buffer %s" (buffer-name master-buffer))
            ))))

    (with-current-buffer ref-buffer
      (save-restriction
        ;;(widen)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward cq-table-border-regexp nil t)
              (progn
                (goto-char (org-table-begin))
                (let ((beg (point))
                      (end (progn (save-excursion (org-table-end)))))
                (setq ref-table (org-table-to-lisp))))
            (error "No org-table found in buffer %s" ref-buffer)
            ))))


    ;;(setq joined-table (orgtbl--create-table-joined master-table "CFXID" ref-table "CFXID"))
    (setq joined-table (orgtbl--create-table-full-outer-joined master-table column ref-table column))

    (with-current-buffer master-buffer
      (save-excursion
        ;;(delete-region beg-master end-master)
        ;;(goto-char beg-master)
        (goto-char end-master)
        (forward-line 1)
        (orgtbl-insert-elisp-table joined-table)
        )
      (goto-char beg-master)
      )
    (switch-to-buffer master-buffer)
    ))


;; borrowed from orgtbljoin
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
	    (set-text-properties 0 (length text) () text)
	    (setq tables (cons text tables))))))
    tables))

;; Make a readable representation of the invocation sequence for FUNC-DEF.
;; It would either be a key or M-x something.
(defun cq-doc-format-bindings-of (func-def)
  (let ((desc (car (where-is-internal func-def
				      overriding-local-map
				      nil nil))))
    (if desc
	(key-description desc)
      (format "M-x %s" func-def))))

(defun cq--get-buffer-table ()
  "Let the user switch to a buffer and choose a table from that buffer.
All tables with `#+TBLNAME:' where found as valid tables.  Return
a cons with buffer and position of the tabel."
  (interactive)
  (let (res buffer pos)
    (save-window-excursion
      (message "Choose buffer for table. When done, type %s. Use %s to abort"
               (cq-doc-format-bindings-of 'exit-recursive-edit)
               (cq-doc-format-bindings-of 'abort-recursive-edit))
      (catch 'exit
        (recursive-edit)
        ;; execute if exit-recursive-edit only:
        (if (cq-find-tables--ivy)
            (cons (current-buffer) (point))
          )
        )
      )
    )
  )


(defun cq-table-join-tables2 ()
  "Join given two tables. Master table comes from master-buffer,
 reference table comes from ref-buffer. Key colum is named column
 in both tables.  Result will overwrite master table."
  (interactive)
  (let ((master (cq--get-buffer-table))
        (ref (cq--get-buffer-table))
        master-buffer master-pos master-table beg-master end-master
        ref-buffer ref-pos ref-table beg-ref end-ref
        dcol
        joined-table
        )
    (when (and master ref)
      (setq master-buffer (car master)
            master-pos (cdr master)
            ref-buffer (car ref)
            ref-pos (cdr ref))

      (with-current-buffer master-buffer
        (save-restriction
          ;;(widen)
          (save-excursion
            (goto-char master-pos)
            (forward-line 1)
            (goto-char (org-table-begin))
            (setq beg-master (point)
                  end-master (progn (save-excursion (org-table-end))))
            (setq master-table (org-table-to-lisp))
            (unless master-table
              (error "No org-table found in buffer %s" (buffer-name master-buffer)))
            )))

      (with-current-buffer ref-buffer
        (save-restriction
          ;;(widen)
          (save-excursion
            (goto-char ref-pos)
            (forward-line 1)
            (goto-char (org-table-begin))
            (setq beg-ref (point)
                  end-ref (progn (save-excursion (org-table-end))))
            (setq ref-table (org-table-to-lisp))
            (unless ref-table
              (error "No org-table found in buffer %s" (buffer-name ref-buffer)))
            )))

      (setq dcol (cq--merge-query-column
                  "Column contains key for join: "
                  (cq--get-headline-from-table ref-table) ref-table))

      (setq joined-table (orgtbl--create-table-full-outer-joined master-table dcol ref-table dcol))

      (with-current-buffer master-buffer
        (save-excursion
          ;;(delete-region beg-master end-master)
          ;;(goto-char beg-master)
          (goto-char end-master)
          (forward-line 1)
          (orgtbl-insert-elisp-table joined-table)
          )
        (goto-char beg-master)
        )
      (switch-to-buffer master-buffer)

      )
    )
  )


(provide 'cq)

;;; cq.el ends here
