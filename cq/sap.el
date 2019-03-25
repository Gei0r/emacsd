;; -*- coding: utf-8-unix; -*-
;;; sap.el --- Links to SAP
;;
;; Copyright (C) 2012, 2013 Stefan-W. Hahn
;;
;; Author: Stefan-W. Hahn <stefan dot hahn at s-hahn dot de>
;; Keywords: sap
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
;;; Commentary:
;;
;;
;; This file implements links to SAP based on regualr expressions for
;; SAP ids.
;;
;;; History:

;;; Code:

(require 'org)
(require 'url)
(require 'cl)

;; Folgende Links führen zum Aufruf des SAP-Frontends oder des Webbrowsers:

;; SAP: Ausführen Web-Links https://docs.mobility.siemens.com/pages/viewpage.action?pageId=1607306

;;
;; SAP-CS (Complaint Management)
;; Aufruf des SAP-Frontends:
;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=IW53&param1=RIWO00-QMNUM%3d<Complaint>
;;
;; Beispiel:
;; Complaint Nr. 179880
;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=IW53&param1=RIWO00-QMNUM%3d179880
;;
;; Aufruf des SAP-Frontends, Aufruf einer Massnahme:
;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=/SIE/TS_CS02002&param1=XH_QMEL%3d205460&param2=XH_MNUM%3d0011

;; SW-Transaktion (A8A)
;; Aufruf des SAP-Frontends für eine bestimmte Version:
;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=%2fSIE%2fTS_PL1801&param1=DRAW-DOKAR%3d<Dokumentenart>&param2=DRAW-DOKNR%3d<Dokumentnummer>&param3=DRAW-DOKTL%3d<Teildokument>&param4=DRAW-DOKVR%3d<Dokumentversion>
;;
;; https://p25.transportation.siemens.com/sap/bc/bsp/sie/ts_pl03_anonym/default.htm?dokar=SW1&doknr=A8A00036361592&dokvr=-&doktl=000&filenam=&dokopt=F&fileopt=1&folddisp=D&ph_id=
;; https://p25.transportation.siemens.com/sap/bc/bsp/sie/ts_pl03_trlink/main.htm?SAP_TRANSACTION=%2fSIE%2fTS_PL1801&param1=DRAW-DOKAR%3dSW1&param2=DRAW-DOKNR%3dA8A00036361592&param3=DRAW-DOKTL%3d000&param4=DRAW-DOKVR%3dB-
;;
;; Beispiel:
;; Aufruf des SAP-Frontends für die SW A8A00003015258/SW1/000/B:
;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=%2fSIE%2fTS_PL1801&param1=DRAW-DOKAR%3dSW1&param2=DRAW-DOKNR%3dA8A00003015258&param3=DRAW-DOKTL%3d000&param4=DRAW-DOKVR%3dB

;; Dokument (A6Z)
;; Aufruf des Webbrowsers für ein Dokument. Hierfür gibt es vier verschiedene Möglichkeiten:
;; - Alle Versionen, alle Originale
;; - Alle Versionen, ein Original
;; - bestimmte Version, alle Originale
;; - bestimmte Version, ein Original
;;
;; Alle Versionen, alle Originale
;; https://p25.transportation.siemens.com/sap/bc/bsp/sie/ts_pl03_anonym/default.htm?dokar=<Dokumententyp>&doknr=<Dokumentennummer>&dokvr=<Dokumentenversion>&doktl=<Teildokument>&filenam=&dokopt=A&fileopt=1&ph_id="
;;
;; Beispiel:
;; Aufruf des Web-Frontends für A6Z00002796583/PM2/000/A:
;; https://p25.transportation.siemens.com/sap/bc/bsp/sie/ts_pl03_anonym/default.htm?dokar=PM2&doknr=A6Z00002796583&dokvr=B&doktl=000&filenam=&dokopt=A&fileopt=1&ph_id="
;;
;; Diese Version, alle Originale
;; https://p25.transportation.siemens.com/sap/bc/bsp/sie/ts_pl03_anonym/default.htm?dokar=<Dokumententyp>&doknr=<Dokumentennummer>&dokvr=<Dokumentenversion>&doktl=<Teildokument>&filenam=&dokopt=A&fileopt=3&ph_id="
;;
;; Beispiel:
;; Aufruf des Web-Frontends für A6Z00002796583/PM2/000/A:
;; https://p25.transportation.siemens.com/sap/bc/bsp/sie/ts_pl03_anonym/default.htm?dokar=PM2&doknr=A6Z00002796583&dokvr=B&doktl=000&filenam=&dokopt=A&fileopt=3&ph_id="
;;

;; Dokument (A6Z)
;; Aufruf des SAP-Frontends für ein Dokument. Hierbei kann nur unterschieden werden zwischen den
;; Transaktionen CV02n (Dokument ändern) oder CV03n (Dokument ansehen). Es muss hierbei
;; immer die korrekte Version des Dokuments angegeben werden.
;;
;; Für CV03n (Dokument ansehen):
;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=CV03N&param1=DRAW-DOKAR%3d<Dokumententyp>&param2=DRAW-DOKNR%3d<Dokumentennummer>&param3=DRAW-DOKTL%3d<Teildokument>&param4=DRAW-DOKVR%3d<Dokumentenversion>
;;
;; Beispiel:
;; Aufruf des SAP-Frontends zum Dokument ansehen für A6Z00002796583/PM2/000/B:
;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=CV03N&param1=DRAW-DOKAR%3dPM2&param2=DRAW-DOKNR%3dA6Z00002796583&param3=DRAW-DOKTL%3d000&param4=DRAW-DOKVR%3dB

;; Aufruf SAP Frontend Dokumentenbrowser:
;;
;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=%2fSIE%2fTS_PL03133&param1=XP_DOKAR%3d<Dokumententyp>&param2=XP_DOKNR%3d<Dokumentennummer>&param3=XP_DOKTL%3d<Teildokument>&param4=XP_DOKVR%3d<Dokumentenversion>
;;
;; Beispiel:
;; Aufruf des Dokumentenbrowsers zum Dokument A6Z00002999476/PM2/000/-
;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=%2fSIE%2fTS_PL03133&param1=XP_DOKAR%3dPM2&param2=XP_DOKNR%3dA6Z00002999476&param3=XP_DOKTL%3d000&param4=XP_DOKVR%3d-

;; Für CV04n (Dokument suchen):
;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=CV04N

;;   Felder können aber ohne weitere Funktionalitätsentwicklung nur die auf dem ersten Reiter angesprochen werden, z.B: Dokumentart
;;   https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=CV04N&param1=STDOKAR-LOW%3dCA2

;; Beispiel:
;;   https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=CV04N&param1=STDOKAR-LOW%3d&param2=STDOKNR-LOW%3dA6Z00001464281

;; Für das Erkennen von SAP-Nummern in Dokumenten werden folgende Regeln festgelegt:
;;
;; Dokumentnummer (A6Z):
;;
;;   A6Z00032708041/PM1/000/-
;;   PM1_A6Z00032708041_-_000
;;   PM1/A6Z00034028147/000/-
;;
;; SW-Nummer (A8A):
;;
;;   A8A00003015258/SW1/000/B
;;
;; SAP-Complaint:
;;
;;   SAP-CS: 179880
;;


;; SAP-Nummer besteht aus:
;; A6Z00032708041/PM1/000/-
;; --------------            id
;;                ---        type
;;                    ---    subid
;;                        -  version

(defgroup sap nil
  "Options specific for sap."
  :tag "Sap Links"
  :group 'sap)

(defcustom sap-template-this-version
  "https://p25.transportation.siemens.com/sap/bc/bsp/sie/ts_pl03_anonym/default.htm?dokar=<type>&doknr=<id>&dokvr=<version>&doktl=<subid>&filenam=&dokopt=T&fileopt=3&ph_id="
  "Template for accessing SAP via http. Parameter order is
  `<type>', `<id>', `<version>', `<subid>'. If no `<version>' is
  available replace it with \"-\". If no `<subid>' is available
  replace it with \"000\"."
  :group 'sap
  :type 'regexp)

(defcustom sap-template-all-versions
  "https://p25.transportation.siemens.com/sap/bc/bsp/sie/ts_pl03_anonym/default.htm?dokar=<type>&doknr=<id>&dokvr=<version>&doktl=<subid>&filenam=&dokopt=A&fileopt=1&ph_id="
  "Template for accessing SAP via http. Parameter order is
  `<type>', `<id>', `<version>', `<subid>'. If no `<version>' is
  available replace it with \"-\". If no `<subid>' is available
  replace it with \"000\"."
  :group 'sap
  :type 'regexp)

(defcustom sap-template-documentbrowser
;;  "https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=CV04N&param1=STDOKAR-LOW%3d<type>&param2=STDOKNR-LOW%3d<id>"
  "https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=%2fSIE%2fTS_PL03133&param1=XP_DOKAR%3d<type>&param2=XP_DOKNR%3d<id>&param3=XP_DOKTL%3d<subid>&param4=XP_DOKVR%3d<version>"
  "Template for accessing SAP documentbrowser via http. Parameter
order is `<type>', `<id>', `<subid>', `<version>'. Access without
`<subid>' or `<version>' seems not possible."
  :group 'sap
  :type 'regexp)

;; https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=%2fSIE%2fTS_PL03133&param1=XP_DOKAR%3d<Dokumententyp>&param2=XP_DOKNR%3d<Dokumentennummer>&param3=XP_DOKTL%3d<Teildokument>&param4=XP_DOKVR%3d<Dokumentenversion>


(defcustom sap-sap-cs-template
  "https://p25.transportation.siemens.com/SAP/BC/BSP/SIE/TS_PL03_TRLINK/main.htm?SAP_TRANSACTION=IW53&param1=RIWO00-QMNUM%%3d%s"
  "Template for accessing SAP-CS via http. Parameter order is `id'"
  :group 'sap
  :type 'regexp)

;; ;; id type subid version
;; (defcustom xsap-sap-id-re-alist
;;   (list
;;    ;; "A6Z00032708041/PM1/000/-"
;;    (list "\\(\\(A6Z[0-9]\\{11\\}\\)/\\([A-Z0-9]+\\)/\\([0-9]\\{3\\}\\)/\\([-A-Z0-9*]+\\)\\)"
;; 	 1 2 3 4)
;;    ;; "PM1_A6Z00032708041_-_000
;;    (list "\\(\\([A-Z0-9]+\\)[ _]\\(\\(A6Z[0-9]\\{11\\}\\)[ _]\\([-A-Z0-9*]+\\)[ _]\\([0-9]\\{3\\}\\)\\)"
;; 	 2 1 4 3)
;;    ;; PM1/A6Z00034028147/000/-
;;    (list "\\(\\([A-Z0-9]+\\)/\\(A6Z[0-9]\\{11\\}\\)//\\([0-9]\\{3\\}\\)/\\([-A-Z0-9*]+\\)\\)"
;; 	 2 1 3 4)
;;    )


;;   "SAP-ID"
;;   :group 'sap
;;   :type 'regexp)

(defcustom sap-id-regexp-alist
  '((sap-id-1
     "\\(\\([A-Z0-9]+\\)/\\(A6Z[0-9]\\{11\\}\\)//\\([0-9]\\{3\\}\\|0[A-Z]\\{2\\}\\)/\\([-A-Z0-9*]+\\)\\)"
     'sap-id 'type 'id 'subid 'version)
    (sap-id-2
     "\\(\\([A-Z0-9]+\\)[ _]\\(\\(A6Z[0-9]\\{11\\}\\)[ _]\\([-A-Z0-9*]+\\)[ _]\\([0-9]\\{3\\}\\|0[A-Z]\\{2\\}\\)\\)"
     'sap-id 'type 'id 'version 'subid)
    (sap-id-3
     "\\(\\(A6Z[0-9]\\{11\\}\\)\\(?:[/_,]\\([A-Z0-9]+\\)[/_,]\\([0-9]\\{3\\}\\|0[A-Z]\\{2\\}\\)[/_,]\\([-A-Z0-9*]+\\)\\)?\\)"
     'sap-id 'id 'type 'subid 'version)
    )
  "SAP-ID A6Z00032708041/PM1/000/-:
- match 1: Whole SAP-ID
- match 2: ID
- match 3: Type
- match 4: Subid
- match 5: Version"
  :group 'sap
  :type 'regexp)

(defcustom sap-sap-id-re
  "\\(\\(A6Z[0-9]\\{11\\}\\)\\(?:[/_,]\\([A-Z0-9]+\\)[/_,]\\([0-9]\\{3\\}\\|0[A-Z]\\{2\\}\\)[/_,]\\([-A-Z0-9*]+\\)\\)?\\)"
  "SAP-ID A6Z00032708041/PM1/000/-:
- match 1: Whole SAP-ID
- match 2: ID
- match 3: Type
- match 4: Subid
- match 5: Version"
  :group 'sap
  :type 'regexp)

(defcustom sap-sap-cs-id-re
  "\\(\\(SAP-CS\\): *\\([0-9]+\\)\\)"
  "SAP-CS-ID"
  :group 'sap
  :type 'regexp)

;; id=2 type=3 subid=4 version=5
(defvar sap-sap-id-re-alist
  '(;; "A6Z00032708041/PM1/000/-"
    ("\\(\\(A6Z[0-9]\\{11\\}\\)/\\([A-Z0-9]+\\)/\\([0-9]\\{3\\}\\|0[A-Z]\\{2\\}\\)/\\([-A-Z0-9*]+\\)\\)"
     'sap-doc-template
     2 3 4 5)
    ;; "PM1_A6Z00032708041_-_000
    ("\\(\\([A-Z0-9]+\\)[ _]\\(A6Z[0-9]\\{11\\}\\)[ _]\\([-A-Z0-9*]+\\)[ _]\\(([0-9]\\{3\\}\\|0[A-Z]\\{2\\}\\)\\)"
     'sap-doc-template
     3 2 5 4)
    ;; PM1/A6Z00034028147/000/-
    ("\\(\\([A-Z0-9]+\\)/\\(A6Z[0-9]\\{11\\}\\)/\\([0-9]\\{3\\}\\|0[A-Z]\\{2\\}\\)/\\([-A-Z0-9*]+\\)\\)"
     'sap-doc-template
     3 2 4 5)
    ;; A6Z00034028147,-
    ("\\(\\(A6Z[0-9]\\{11\\}\\),\\([-A-Z0-9*]+\\)\\)"
     'sap-doc-template
     2 5)
    ;; A8A00003015258/SW1/000/B
    ("\\(\\(A8A[0-9]\\{11\\}\\)/\\([A-Z0-9]+\\)/\\([0-9]\\{3\\}\\)/\\([-A-Z0-9*]+\\)\\)"
     'sap-sw1-template
     2 3 4 5)
    ;; "SW1_A8A00032708041_-_000
    ("\\(\\([A-Z0-9]+\\)[ _]\\(A8A[0-9]\\{11\\}\\)[ _]\\([-A-Z0-9*]+\\)[ _]\\([0-9]\\{3\\}\\)\\)"
     'sap-sw1-template
     3 2 5 4)
    ;; SW1/A8A00034028147/000/-
    ("\\(\\([A-Z0-9]+\\)/\\(A8A[0-9]\\{11\\}\\)/\\([0-9]\\{3\\}\\)/\\([-A-Z0-9*]+\\)\\)"
     'sap-sw1-template
     3 2 4 5)
    ;; SAP-CS: 179880
    ("\\(\\(SAP-CS\\):\\sw+?\\([0-9]\\)\\)"
     'sap-cs-template
     1 2)
   ))


(defun sap-replace-placeholder (url id &optional type subid version)
  "Replace placeholder <id>, <type>, <subid>, <version>. Default
  <subid> to \"000\" and <version> to \"-\"."
  (mapc #'(lambda (elt)
            (let* ((re (if (atom (car elt)) (car elt) (caar elt)))
                   (default (if (consp (car elt)) (cdr (car elt)) ""))
                   (value (or (cdr elt) default)))
              (setq url (replace-regexp-in-string (regexp-quote re) value url))))
        `(("<id>" . ,id)
          (("<type>" . "PM1") . ,type)
          (("<subid>" . "000") . ,subid)
          (("<version>" . "-") . ,version)))
  url)

(defun sap-sap-url (which sapid)
  "Generate url for web access to sap for a given sapid. SAPID is a
list with ID, TYPE, SUBDID, VERSION."
  (let ((id (nth 0 sapid))
        (type (nth 1 sapid))
        (subid (nth 2 sapid))
        (version (nth 3 sapid)))
    (when (or (string= version "*")
              (string= version ""))
      (setq which :all))
    (cond
     ((eq which :this)
      (sap-replace-placeholder sap-template-this-version
                               id type subid version))
     ((eq which :all)
      (sap-replace-placeholder sap-template-all-versions
                               id type subid version))
     (t
      (error "wrong which %s" which)))))

(defun sap-sap-cs-url (id)
  "Generate url for web access to sap for a given sap-cs-id."
  (format sap-sap-cs-template
          id))

(defun sap-get-val (key lst)
  "Read value of property `key' from list `lst'."
  (cdr (assoc key lst)))

(defun sap-open-url (url)
  "Start program via `url'."
  (message "sap: %s" url)
  (cond
   ((fboundp 'w32-shell-execute)
    (w32-shell-execute nil url))
    ;; (let ((file-name-handler-alist nil))
    ;;   (w32-shell-execute nil url)))
   ;; ((fboundp 'browse-url)
   ;;    (let ((browse-url-firefox-program "C:/Program Files (x86)/Internet Explorer/iexplore.exe")
   ;;          (browse-url-browser-function 'browse-url-firefox)
   ;;          (browse-url-new-window-flag nil)
   ;;          (browse-url-firefox-new-window-is-tab nil))
   ;;      (browse-url url)))
   (t
    (message "no function to call %s" url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SAP-Nummern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sap-get-sap-id-from-point (beg end)
  "Try to read SAP-ID from point. Returns `nil' if there is
no SAP-ID at point, otherwise returns a list with
   (ID TYPE SUBID VERSION)."
  (let (id)
    (save-match-data
      (save-excursion
	(goto-char beg)
	(if (looking-at sap-sap-id-re)
	    (setq id (list (match-string-no-properties 2)
			   (match-string-no-properties 3)
			   (match-string-no-properties 4)
			   (match-string-no-properties 5))))))
      id))

(defun sap-get-sap-id-from-string (str)
  "Try to read SAP-ID from point. Returns `nil' if there is
no SAP-ID at point, otherwise returns a list with
   (ID TYPE SUBID VERSION)."
  (let (id)
    (save-match-data
      (save-excursion
        (with-temp-buffer
          (insert str)
          (goto-char (point-min))
          (if (looking-at sap-sap-id-re)
              (setq id (list (match-string-no-properties 2)
                             (match-string-no-properties 3)
                             (match-string-no-properties 4)
                             (match-string-no-properties 5))))))
      id)))

(defun sap-get-sap-cs-id-from-point (beg end)
  "Try to read SAP-CS-ID from point. Returns `nil' if there is
no SAP-CS-ID at point, otherwise returns a list with
   (ID)."
  (let (id)
    (save-match-data
      (save-excursion
	(goto-char beg)
	(if (looking-at sap-sap-cs-id-re)
	    (setq id (list (match-string-no-properties 3))))))
      id))

(defun sap-get-this-url-from-kill-ring ()
  "Yank from kill-ring and get the URL for the SAP document."
  (interactive)
  (with-temp-buffer
    (yank)
    (sap-font-lock)
    (font-lock-ensure)
    (goto-char (point-min))
    (sap-get-url t)))

(defun sap-get-url (&optional this no-kill)
  "Get SAP url from link."
  (interactive)
  (let ((type (if this :this :all))
        id rc)
    (setq rc
          (cond
           ((setq id (sap-get-sap-id-from-string
                      (get-text-property (point) 'sap-id)))
            (if (string= "*" (nth 3 id))
                (setq type :all))
            (sap-sap-url type id)
            )
           ((setq id (get-text-property (point) 'sap-cs-id))
            (sap-sap-cs-url (nth 0 id))
            )
           (t
            nil)))
    (unless no-kill
      (if rc (kill-new rc)))
    rc))

(defun sap-get-this-url ()
  (interactive)
  (sap-get-url t))

(defun sap-get-all-url ()
  (interactive)
  (sap-get-url nil))

;; (defun sap-open-from-sap-id (browser-type version-type)
;;   "Try to read a file via sap if point is on an SAP-ID."
;;   (interactive)
;;   (cond
;;    ((eq browser-type :web)
;;     (sap-open-url (sap-get-url (if (version-type
;;     )
;;    ((eq browser-type :gui)
;;     )
;;    (t
;;     (error "sap-open-from-sap-id: wrong browser-type %s",
;;            browser-type))))
;;   (sap-open-url (sap-get-url this t)))

(defun sap-open-web-from-sap-id (this)
  "Try to read a file via sap if point is on an SAP-ID."
  (interactive)
  (sap-open-url (sap-get-url this t)))

(defun sap-open-gui-from-sap-id (this)
  (interactive)
  (let* ((sap-id (sap-get-sap-id-from-string
                  (get-text-property (point) 'sap-id)))
         (id (nth 0 sap-id))
         (type (nth 1 sap-id))
         (subid (nth 2 sap-id))
         (version (nth 3 sap-id))
         (url (sap-replace-placeholder
               sap-template-documentbrowser
               id type subid version))
         )
    (when (and id)
      (sap-open-url url))))

(defun sap-open-web-from-sap-id-all ()
  (interactive)
  (sap-open-web-from-sap-id nil))

;; (defun xxsap-mouse-2 (event)
;;   "Move point to the position clicked on with the mouse and query
;;  the pr at that point.  This should be bound to a mouse click
;;  event type."
;;   (interactive "e")
;;   (let ((position (event-end event)))
;;     (when (windowp (posn-window position))
;;       (with-selected-window (posn-window position)
;;         (goto-char (posn-point (event-start event)))
;;         (sap-open-web-from-sap-id-all)
;;         ))))

(defun sap-mouse-1 (event)
  "Move point to the position clicked on with the mouse and query
 the pr at that point.  This should be bound to a mouse click
 event type."
  (interactive "e")
  (mouse-set-point event)
  (sap-open-gui-from-sap-id 'this-version))

(defun sap-mouse-2 (event)
  "Move point to the position clicked on with the mouse and query
 the pr at that point.  This should be bound to a mouse click
 event type."
  (interactive "e")
  (mouse-set-point event)
  (sap-open-web-from-sap-id-all))

(defun sap-open-file-from-sap-id-ask (&optional par)
  "Try to read a file via sap if point is on an SAP-ID."
  (interactive)
  (let ((id (get-text-property (point) 'sap-id))
	c)
    (when id
      (message "Open file in Webbrowser [t]his version or [a]ll versions,
Open file in SAP GUI [g]this version or [G]all versions,
Copy URL t[h]is version or a[l]l versions? ")
      (setq c (read-char-exclusive))
      (message "")
      (cond
       ((equal c ?t)
	(sap-open-web-from-sap-id 'this-version))
       ((equal c ?a)
	(sap-open-web-from-sap-id nil))
       ((equal c ?g)
        (sap-open-gui-from-sap-id 'this-version))
       ((equal c ?G)
        (sap-open-gui-from-sap-id nil))
       ((equal c ?h)
        (sap-get-this-url))
       ((equal c ?h)
        (sap-get-this-url))
       ((equal c ?l)
        (sap-get-all-url))
       ((equal c ?q) (error "Abort"))
       (t (error "Invalid key %c" c))))
    id))

(add-hook 'org-open-at-point-functions 'sap-open-file-from-sap-id-ask t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add font lock for SAP-IDs for several modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sap-mouse-map (make-sparse-keymap))
;;(define-key sap-mouse-map [mouse-1] 'sap-open-gui-from-sap-id)
(define-key sap-mouse-map [mouse-1] 'sap-mouse-1) ;; cv04n search
(define-key sap-mouse-map [mouse-2] 'sap-mouse-2) ;; all versions
(define-key sap-mouse-map [follow-link] 'mouse-face)
(define-key sap-mouse-map [(tab)] 'sap-open-file-from-sap-id-ask)
(define-key sap-mouse-map [\C-c \C-o] 'sap-open-file-from-sap-id-ask)
;;(define-key sap-mouse-map "\C-i" 'sap-open-web-from-sap-id)

(require 'font-lock)

(defface sap-sap-face '((t (:box (:line-width 1 :color grey40))))
  "The special face for sap ids."
  :group 'sap)

(defvar sap-sap-face 'sap-sap-face
  "Face name to use for sap id's.")

(defun sap-fontify-sap-ids (limit)
  (condition-case nil
      (sap-fontify-sap-ids-1 limit)
    (error (message "sap fontification error"))))

;; (defun xx ()
;;   (interactive)
;;   (sap-fontify-sap-ids-1 (point-max)))

(defun sap-fontify-sap-ids-1 (limit)
  "Fontify and add text properties for mouse keymap."
  (if (re-search-forward sap-sap-id-re limit t)
      (let* ((beg (match-beginning 0))
	     (end (match-end 0))
	     (sap-id (sap-get-sap-id-from-point beg end)))
	(org-remove-flyspell-overlays-in beg end)
	(add-text-properties beg end
			     (list 'mouse-face 'highlight
				   'face 'org-link ;;sap-sap-face
				   'keymap sap-mouse-map
				   'help-echo (format "LINK SAP/PLM:%s/%s/%s/%s"
						      (nth 0 sap-id)
                                                      (or (nth 1 sap-id) "")
                                                      (or (nth 2 sap-id) "")
                                                      (or (nth 3 sap-id) ""))
				   'sap-id sap-id
                                   'rear-nonsticky '(mouse-face keymap help-echo sap-id)))
	t)))

(defun sap-fontify-sap-cs-ids (limit)
  (condition-case nil
      (sap-fontify-sap-cs-ids-1 limit)
    (error (message "oab fontification error"))))

(defun sap-fontify-sap-cs-ids-1 (limit)
  "Fontify and add text properties for mouse keymap."
  (if (re-search-forward sap-sap-cs-id-re limit t)
      (let* ((beg (match-beginning 0))
	     (end (match-end 0))
	     (id (sap-get-sap-cs-id-from-point beg end)))
	(org-remove-flyspell-overlays-in beg end)
	(add-text-properties beg end
			     (list 'mouse-face 'highlight
				   'face 'org-link ;;sap-sap-face
				   'keymap sap-mouse-map
				   'help-echo (format "LINK SAP/CS:%s"
						      (nth 0 id))
				   'sap-cs-id id
                                   'rear-nonsticky '(mouse-face keymap help-echo sap-cs-id)))
	t)))

;; (defun xxx ()
;;   (interactive)
;;   (xsap-fontify-sap-ids-1 nil))

(defun sap-fontify-sap-ids-2 (item limit)
  "Check for current re from `sap-sap-id-re-alist'."
  (let* ((template (nth 1 item))
         (pat (car item))
         (positions (cdr (cdr item)))
         (num (nth 0 positions))
         (type (nth 1 positions))
         (subnum (nth 2 positions))
         (version (nth 3 positions)))
    (if (re-search-forward pat limit t)
        (let* ((inhibit-read-only t)
               (beg (match-beginning 0))
               (end (match-end 0))
               sap-id sap-num sap-type sap-subnum sap-version)
          (save-match-data
            (setq sap-num (match-string-no-properties num)
                  sap-type (match-string-no-properties type)
                  sap-subnum (match-string-no-properties subnum)
                  sap-version (match-string-no-properties version)
                  sap-id (format "%s/%s/%s/%s"
                                 sap-num
                                 (or sap-type "")
                                 (or sap-subnum "")
                                 (or sap-version ""))))
          (org-remove-flyspell-overlays-in beg end)
          (add-text-properties beg end
                               (list 'mouse-face 'highlight
                                     'face 'org-link ;;sap-sap-face
                                     'keymap sap-mouse-map
                                     'help-echo (format "LINK SAP/PLM:%s" sap-id)
                                     'sap-id sap-id
                                     'rear-nonsticky '(mouse-face keymap help-echo sap-id)))
          t))))


;;      (defmacro for (var from init to final do &rest body)
;;        "Execute a simple for loop: (for i from 1 to 10 do (print i))."
;;        (let ((tempvar (make-symbol "max")))
;;          `(let ((,var ,init)
;;                 (,tempvar ,final))
;;             (while (<= ,var ,tempvar)
;;               ,@body
;;               (inc ,var)))))

;;      (defmacro for (var from init to final do &rest body)
;;        "Execute a simple for loop: (for i from 1 to 10 do (print i))."
;;        (let ((tempvar (make-symbol "max")))
;;          `(let ((,var ,init)
;;                 (,tempvar ,final))
;;             (while (<= ,var ,tempvar)
;;               ,@body
;;               (inc ,var)))))

;; (macroexpand '(sap-do-fontify-sap-id-command name item))
;; (let ((patx (nth 0 sap-sap-id-re-alist)))
;;   (sap-do-fontify-sap-id-command "abc" ,patx))
;; (eval-and-compile
;;   (defmacro sap-do-fontify-sap-id-command (spec item)
;;     (let ((pat (car item)))
;;       `(progn
;;          (defun ,(intern "sap-xxx") (limit)
;;            ,(concat "Fontify function for SAP id for pattern `"
;;                     "'."
;;                     )
;;            ,pat
;;            item))))
;;   (def-edebug-spec sap-do-fontify-sap-id-command (sexp form))
;;   )

;; (sap-xxx 100)
      
;;          (let ((pat ,(car item))
;;                ;;(positions (last 2 item))
;;                (num (nth 1 item))
;;                (type (nth 2 item))
;;                (subnum (nth 3 item))
;;                (version (nth 4 item))
;;                )
;;            (if (re-search-forward pat limit t)
;;                (let* ((beg (match-beginning 0))
;;                       (end (match-end 0))
;;                       sap-id sap-num sap-type sap-subnum sap-version)
;;                  (save-match-data
;;                    (setq sap-num (match-string-no-properties num)
;;                          sap-type (match-string-no-properties type)
;;                          sap-subnum (match-string-no-properties subnum)
;;                          sap-version (match-string-no-properties version)
;;                          sap-id (format "%s/%s/%s/%s" sap-num sap-type sap-subnum sap-version)))
;;                  (org-remove-flyspell-overlays-in beg end)
;;                  (add-text-properties beg end
;;                                       (list 'mouse-face 'highlight
;;                                             'face 'org-link ;;sap-sap-face
;;                                             'keymap sap-mouse-map
;;                                             'help-echo (format "LINK SAP/PLM:%s" sap-id)
;;                                             'sap-id sap-id))
;;                  t)
;;              )))
;;          )
;;     )
;; )  )

;; (defun xxxxx ()
;;   (interactive)
;;   (let ((name (format "sap-fontify-sap-id-%d" 0))
;;         (item (nth 0 sap-sap-id-re-alist)))
;;     (sap-do-fontify-sap-id-command name item)
;;   )
;;   )

;; (defun sap-create-fontify-functions ()
;;   (let ((count 0))
;;     (dolist (item sap-sap-id-re-alist)
;;       (sap-do-fontify-sap-id-command
;;        (intern (format "sap-fontify-sap-id-%d" count))
;;        item)
;;       (setq count (1+ count))))
;;   )


;; (eval-and-compile
;;   (defmacro paredit-do-commands (vars string-case &rest body)
;;     (let ((spec     (nth 0 vars))
;;           (keys     (nth 1 vars))
;;           (fn       (nth 2 vars))
;;           (examples (nth 3 vars)))
;;       `(dolist (,spec paredit-commands)
;;          (if (stringp ,spec)
;;              ,string-case
;;            (let ((,keys (let ((k (car ,spec)))
;;                           (cond ((stringp k) (list k))
;;                                 ((listp k) k)
;;                                 (t (error "Invalid paredit command %s."
;;                                           ,spec)))))
;;                  (,fn (cadr ,spec))
;;                  (,examples (cddr ,spec)))
;;              ,@body)))))

;;   (put 'paredit-do-commands 'lisp-indent-function 2))

;; (defun paredit-define-keys ()
;;   (paredit-do-commands (spec keys fn examples)
;;       nil       ; string case
;;     (dolist (key keys)
;;       (define-key paredit-mode-map (read-kbd-macro key) fn))))

(defun sap-fontify-sap-ids-x (limit)
  (condition-case nil
      (sap-fontify-sap-ids-x-1 limit)
    (error (message "sap fontification error"))))

;; TODO activate
;; es fehlt noch die Beschreibung der Customeinstellungen für sap-sap-id-re-alist
(defun sap-fontify-sap-ids-x-1 (limit)
  "Fontify and add text properties for mouse keymap."
  (let ((start (point)))
    (catch 'return
      (dolist (item sap-sap-id-re-alist)
        (goto-char start)
        (if (sap-fontify-sap-ids-2 item limit)
            (throw 'return t)))
      )))

(defun x ()
  (interactive)
  (xsap-fontify-sap-ids-1 (point-max)))

(defun sap-font-lock ()
  "Add font locking search function."
  (font-lock-add-keywords
   nil
   (list
    ;; function for applying text-properties
    '(sap-fontify-sap-ids-x)
    ;;'(sap-fontify-sap-ids)
    ;;'(sap-fontify-sap-cs-ids)
    )
   t))

;; ================================================================================

(defvar sap-cdi-result nil)
(defun sap-cdisapcall ()
  (interactive)
  (let ((buffer (get-buffer-create "*tmp*"))
        (filename (make-temp-file "cdisap"))
        (username "HAHN_STE1")
        (sap-id "A6Z00005663187")
        )
    (with-current-buffer buffer
      (insert
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<cti:ToolCommunication xmlns:cti=\"http:///cti.ecore\" toolName=\"CdiSapCall\" toolVersion=\"v.1.1.2.1 Build: 20130311-105805\" >

 <toolFunction description=\"\" name=\"logon_r3\" >
  <toolParameterBlock>
   <toolParameter description=\"\" cardinality=\"1\" type=\"rfc_char\" name=\"ih_client\" >
    <value>747</value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"rfc_char\" name=\"ih_user\" >
    <value>" username "</value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"rfc_char\" name=\"ih_language\" >
    <value>DE</value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"rfc_int\" name=\"ih_host\" >
    <value>saprfc.mobility.siemens.com</value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"rfc_int\" name=\"ih_systemnumber\" >
    <value>21</value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"rfc_int\" name=\"ih_gateway_host\" >
    <value>saprfc.mobility.siemens.com</value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"rfc_char\" name=\"ih_gateway_service\" >
    <value>sapgw21</value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"rfc_char\" name=\"ih_group\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"rfc_char\" name=\"ih_connectionstring\" >
    <value>SNC_Mode=\"1\" SNC_PARTNERNAME=\"p:CN=sapP22.mobility.siemens.com, C=DE, O=Siemens, OU=I MO IT SAP\"</value>
   </toolParameter>
  </toolParameterBlock>
 </toolFunction>

 <toolFunction description=\"Generated CTI signature by CdiSapCall.\" name=\"BAPI_DOCUMENT_GETLIST2\" >
  <toolParameterBlock>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 18\" name=\"classno\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"classtype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"folder_key::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"folder_key::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"folder_key::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"folder_key::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"fulltext_active\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"fulltext_and\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"fulltext_or\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"fulltext_searchstring\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"fulltext_searchtype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"getclassification\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"getcomponents\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"getdocdata\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"getdocdescriptions\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"getdocfiles\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"getlongtexts\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"getobjectlinks\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"getstatuslog\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"getstructures\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"getwhereused\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"get_fulltext_active\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"language\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"language_iso\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"latest\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"latestreleased\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"maxrows\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"select_documentdata::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"select_documentdata::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"select_documentdata::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"select_documentdata::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 40\" name=\"select_documentdata::description\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 12\" name=\"select_documentdata::username\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"select_documentdata::statusextern\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"select_documentdata::statusintern\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 20\" name=\"select_documentdata::statuslog\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"select_documentdata::laboratory\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 12\" name=\"select_documentdata::ecnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"DATE 8\" name=\"select_documentdata::validfromdate\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"select_documentdata::revlevel\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"select_documentdata::deleteindicator\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"select_documentdata::cadindicator\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"select_documentdata::structureindicator\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"select_documentdata::predocumentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"select_documentdata::predocumentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"select_documentdata::predocumentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"select_documentdata::predocumenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 4\" name=\"select_documentdata::authoritygroup\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"select_documentdata::docfile1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"select_documentdata::datacarrier1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"select_documentdata::wsapplication1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"select_documentdata::docfile2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"select_documentdata::datacarrier2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"select_documentdata::wsapplication2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"DATE 8\" name=\"select_documentdata::vrldat\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 14\" name=\"select_documentdata::userdefined1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 14\" name=\"select_documentdata::userdefined2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 14\" name=\"select_documentdata::userdefined3\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 14\" name=\"select_documentdata::userdefined4\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"select_documentdata::savedocfile1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"select_documentdata::savedatacarrier1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"select_documentdata::savedocfile2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"select_documentdata::savedatacarrier2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"DATE 8\" name=\"select_documentdata::createdate\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"select_documentdata::refdocumentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"select_documentdata::refdocumentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"select_documentdata::refdocumentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"NUM 12\" name=\"select_documentdata::filesize1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"NUM 12\" name=\"select_documentdata::filesize2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"select_documentdata::cmfixed\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"select_documentdata::cmrelevance\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"DATE 8\" name=\"status_enddate\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"DATE 8\" name=\"status_startdate\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"NUM 18\" name=\"alloclist::object_key\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 18\" name=\"alloclist::classnum\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"alloclist::status\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"alloclist::standardclass\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"all_returns::type\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 20\" name=\"all_returns::id\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"NUM 3\" name=\"all_returns::number\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 220\" name=\"all_returns::message\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 20\" name=\"all_returns::log_no\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"NUM 6\" name=\"all_returns::log_msg_no\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 50\" name=\"all_returns::message_v1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 50\" name=\"all_returns::message_v2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 50\" name=\"all_returns::message_v3\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 50\" name=\"all_returns::message_v4\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 32\" name=\"all_returns::parameter\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"all_returns::row\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 30\" name=\"all_returns::field\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"all_returns::system\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"characteristicvalues::classtype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 18\" name=\"characteristicvalues::classname\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 30\" name=\"characteristicvalues::charname\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 30\" name=\"characteristicvalues::charvalue\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"characteristicvalues::deletevalue\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"characteristicvalues::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"characteristicvalues::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"characteristicvalues::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"characteristicvalues::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"characteristicvalues::tab_index\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"classallocations::classtype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 18\" name=\"classallocations::classname\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"classallocations::status\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"classallocations::standardclass\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"classallocations::deleteallocation\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 12\" name=\"classallocations::ecnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"classallocations::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"classallocations::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"classallocations::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"classallocations::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"classallocations::tab_index\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"classnumrange::sign\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"classnumrange::option\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 18\" name=\"classnumrange::low\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 18\" name=\"classnumrange::high\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 250\" name=\"class_selection::characteristic\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 250\" name=\"class_selection::value\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"components::deletevalue\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"components::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"components::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"components::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"components::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 5\" name=\"components::originaltype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"components::language\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"components::sourcedatacarrier\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"components::storagecategory\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"components::docfile\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 16\" name=\"components::format\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 40\" name=\"components::description\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 32\" name=\"components::ph_objid\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"components::tab_index\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"docnumberselection::sign\" >
    <value>I</value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"docnumberselection::option\" >
    <value>EQ</value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"docnumberselection::documentnumber_low\" >
    <value>" sap-id "</value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"docnumberselection::documentnumber_high\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentdata::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"documentdata::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentdata::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentdata::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 40\" name=\"documentdata::description\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 12\" name=\"documentdata::username\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentdata::statusextern\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentdata::statusintern\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 20\" name=\"documentdata::statuslog\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentdata::laboratory\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 12\" name=\"documentdata::ecnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"DATE 8\" name=\"documentdata::validfromdate\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentdata::revlevel\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentdata::deleteindicator\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentdata::cadindicator\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentdata::structureindicator\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"documentdata::predocumentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentdata::predocumentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentdata::predocumentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentdata::predocumenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 4\" name=\"documentdata::authoritygroup\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"documentdata::docfile1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"documentdata::datacarrier1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentdata::wsapplication1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"documentdata::docfile2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"documentdata::datacarrier2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentdata::wsapplication2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"DATE 8\" name=\"documentdata::vrldat\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 14\" name=\"documentdata::userdefined1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 14\" name=\"documentdata::userdefined2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 14\" name=\"documentdata::userdefined3\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 14\" name=\"documentdata::userdefined4\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"documentdata::savedocfile1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"documentdata::savedatacarrier1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"documentdata::savedocfile2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"documentdata::savedatacarrier2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"DATE 8\" name=\"documentdata::createdate\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"documentdata::refdocumentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentdata::refdocumentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentdata::refdocumentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"NUM 12\" name=\"documentdata::filesize1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"NUM 12\" name=\"documentdata::filesize2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentdata::cmfixed\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentdata::cmrelevance\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentdescriptions::deletevalue\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentdescriptions::language\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentdescriptions::language_iso\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 40\" name=\"documentdescriptions::description\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentdescriptions::textindicator\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentdescriptions::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"documentdescriptions::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentdescriptions::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentdescriptions::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"documentdescriptions::tab_index\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentfiles::deletevalue\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentfiles::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"documentfiles::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentfiles::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentfiles::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 5\" name=\"documentfiles::originaltype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"documentfiles::sourcedatacarrier\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"documentfiles::storagecategory\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentfiles::wsapplication\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"documentfiles::docpath\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 255\" name=\"documentfiles::docfile\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentfiles::statusintern\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentfiles::statusextern\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 20\" name=\"documentfiles::statuslog\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 32\" name=\"documentfiles::application_id\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 32\" name=\"documentfiles::file_id\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 40\" name=\"documentfiles::description\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentfiles::language\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentfiles::checkedin\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentfiles::active_version\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"NUM 14\" name=\"documentfiles::created_at\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"NUM 14\" name=\"documentfiles::changed_at\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 12\" name=\"documentfiles::created_by\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 12\" name=\"documentfiles::changed_by\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 50\" name=\"documentfiles::content_description\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"documentfiles::tab_index\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentstructures::deletevalue\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentstructures::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"documentstructures::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentstructures::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentstructures::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 18\" name=\"documentstructures::quantity\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"documentstructures::sortstring\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentstructures::recallowed\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"documentstructures::cad_pos\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentstructures::documenttypepar\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"documentstructures::documentnumberpar\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"documentstructures::documentversionpar\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"documentstructures::documentpartpar\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"documentstructures::tab_index\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"longtexts::deletevalue\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"longtexts::language\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"longtexts::language_iso\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 132\" name=\"longtexts::textline\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"longtexts::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"longtexts::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"longtexts::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"longtexts::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"longtexts::tab_index\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"objectlinks::deletevalue\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"objectlinks::objecttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 50\" name=\"objectlinks::objectkey\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"objectlinks::documentdirection\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 40\" name=\"objectlinks::objectdescription\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 32\" name=\"objectlinks::objectlinkid\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 30\" name=\"objectlinks::addobjecttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 50\" name=\"objectlinks::addobjectkey\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"objectlinks::cad_pos\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"objectlinks::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"objectlinks::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"objectlinks::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"objectlinks::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"objectlinks::tab_index\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"objlinkselection::dokob\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 50\" name=\"objlinkselection::objky\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"statuslog::statusintern\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"statuslog::statusextern\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"DATE 8\" name=\"statuslog::logdate\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 8\" name=\"statuslog::logtime\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 12\" name=\"statuslog::username\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 20\" name=\"statuslog::logfield\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"statuslog::auditflag1\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"statuslog::auditflag2\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"statuslog::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"statuslog::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"statuslog::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"statuslog::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"statuslog::tab_index\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"whereusedlists::deletevalue\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"whereusedlists::documenttype\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"whereusedlists::documentnumber\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"whereusedlists::documentpart\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"whereusedlists::documentversion\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 18\" name=\"whereusedlists::quantity\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 10\" name=\"whereusedlists::sortstring\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"whereusedlists::recallowed\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 1\" name=\"whereusedlists::cad_pos\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"whereusedlists::documenttypepar\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 25\" name=\"whereusedlists::documentnumberpar\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 2\" name=\"whereusedlists::documentversionpar\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"CHAR 3\" name=\"whereusedlists::documentpartpar\" >
    <value></value>
   </toolParameter>
   <toolParameter description=\"\" cardinality=\"1\" type=\"INT 4\" name=\"whereusedlists::tab_index\" >
    <value></value>
   </toolParameter>
  </toolParameterBlock>
 </toolFunction>
</cti:ToolCommunication>
")
      (set-buffer-file-coding-system 'utf-8-dos)
      (write-file filename)
      (let ((old-size (nth 7 (file-attributes filename 'string))))
        (with-current-buffer (get-buffer-create "*cdisapcall*")
          (call-process "d:/home/hs/basesystem/src/cdisapcall/cdiexamples/cdisapcall.exe"
                        nil '(t t) t filename))
        (let ((changed t)
              attrs size)
          (while changed
            (if (setq attrs (file-attributes filename 'string))
                (progn
                  (setq size (nth 7 attrs))
                  (setq changed (/= old-size size))
                  (setq old-size size)
                  (sit-for 0))
              (setq changed nil))))
        (pop-to-buffer (current-buffer))
        (revert-buffer nil t t)
        (goto-char (point-max))
        (require 'xml)
        (let ((tree (xml-parse-region (point-min) (point-max) nil nil nil))
              result)
          ;;(erase-buffer)
          (insert (format "\n\n%s\n\n" tree))
          (setq sap-cdi-result tree)

          ;; (setq result (xml-get-children tree 'toolResultBlock))
          ;; (insert "resultStatus %s"
          ;;         (xml-get-attribute-or-nil result 'resultStatus))


          )
        ))))

(defun sap-print-children-of (node)
  (insert (format "children of %s: %s\n"
                  (xml-node-name node)
                  (mapcar #'(lambda (elem)
                              (xml-node-name elem))
                          (sap-value-node-children node)))
  ))

(defun sap-value-node-children (node)
  (remove-if (function stringp) (xml-node-children node)))

(defun sap-cdisapcall-tree ()
  (interactive)
  (let ((tree (car sap-cdi-result))
        tool-function
        tool-result)
    ;;(sap-print-children-of sap-cdi-result)

    (with-current-buffer (get-buffer-create "*tmp*")
      (pop-to-buffer (current-buffer))
      (sap-print-children-of tree)
      ;;(sap-walk tree 0)
      (setq tool-function (xml-get-children tree 'toolFunction))
      (sap-print-children-of tool-function)
      )
    (setq tool-result (xml-get-children tool-function 'toolResultBlock))
    (xml-get-attribute-or-nil tool-result 'resultStatus))


  )


(defun sap-walk (root deep)
  (cond ((null root) nil)
	((listp root) (let ((elem (xml-node-name root))
			    (children (remove-if (function stringp) (xml-node-children root))))
                        (insert (format "%s[%s]\n" (apply 'concat (make-list deep " ")) elem))
                        (mapcar (lambda (x) (sap-walk x (1+ deep))) children)))))

;; A6Z00036031059/PM1/000/E
;; (defun xsap-open-internal ()
;;   "Test."
;;   ;;(interactive)
;;   (let* ((sap-id (get-text-property (point) 'sap-id))
;;          (id (nth 0 sap-id))
;;          (type (nth 1 sap-id))
;;          (subid (nth 2 sap-id))
;;          (version (nth 3 sap-id))
;;          (url (sap-replace-placeholder
;;                sap-template-documentbrowser
;;                id type subid version))
;;     ;;(when (and id)
;;          (url-debug t)
;;          (buf (url-retrieve-synchronously url 'no-silent))
;; 	count url-list id)
;;     (when (not (null buf))

;;       (switch-to-buffer buf)

;;       (with-current-buffer buf
;; 	(goto-char (point-min))
;; ;; TODO
;; ;;	(if (re-search-forward "The document hast moved.*<address>.* at \\([a-zA-Z:.]+\\) Port \\([0-9]+\\)</address>" nil t)

;; 	(when (re-search-forward "<oslc_cm:totalCount.*?>\\([0-9]+\\)</oslc_cm:totalCount>" nil t)
;; 	    (setq count (string-to-number (match-string 1)))

;; 	    (cond
;; 	     ((= count 1)

;; 	      (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
;; 		  (setq id (match-string 1)))
;; 	      (if (re-search-forward "<link rel=\"alternate\" href=\"\\(.*\\)\"/>" nil t)
;; 		  (progn
;; 		    (message "open %s -> %s" pr id)
;;                     id))
;; 	      )
;; 	     ((> count 1)

;; 	      (while (> count 0)
;; 		(let (id url)
;; 		  (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
;; 		      (setq id (match-string 1)))
;; 		  (if (re-search-forward "<link rel=\"alternate\" href=\"\\(.*\\)\"/>" nil t)
;; 		      (setq url (match-string 1)))

;; 		  (push (cons id (list id url)) url-list))
;; 		(setq count (1- count)))

;; 	      (let* ((all-ids (mapcar (function
;; 				       (lambda (x)
;; 					(car x))) url-list))
;; 		     (id (ido-completing-read "PR: " all-ids))
;; 		     url)
;;                 id)
;; 	      )
;; 	     (t
;; 	      (message "No PR %s found" pr)
;; 	      ;;
;; 	      ))))

;;       (kill-buffer buf)
;;       )
;;     id))

(defun my-org-export-replace-sap-link (backend)
  "Replace all SAP Id with Link to the web fronted of sap."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward sap-sap-id-re nil t)
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (sap-id (sap-get-sap-id-from-point beg end)))
        (replace-match (format "[[%s][%s/%s/%s/%s]]"
                               (sap-sap-url :this sap-id)
                               (nth 0 sap-id)
                               (or (nth 1 sap-id) "")
                               (or (nth 2 sap-id) "")
                               (or (nth 3 sap-id) "")))
        ))))

(add-hook 'org-export-before-parsing-hook 'my-org-export-replace-sap-link)


;; (defun xxxxxxx ()
;;   (interactive)
;;   (let ((url "https://p25.transportation.siemens.com/sap/bc/bsp/sie/ts_pl03_anonym/default.htm?dokar=PM1&doknr=A6Z00005349899&dokvr=C&doktl=000&filenam=&dokopt=A&fileopt=1&ph_id=")
;;         (url-using-proxy nil)
;;         (url-proxy-services nil)
;;         (url-cookie-trusted-urls '("^https?://"))
;;         ;;(url-request-method "GET")
;;         ;; (url-request-extra-headers
;;         ;;  `(("Content-Type" . "application/json; charset=UTF-8")
;;         ;;     )
;;         ;;  )
;;         )
;;     (url-retrieve url
;;                   (lambda (status)
;;                     (switch-to-buffer (current-buffer))))
;;     )
;;   )


(provide 'sap)

;;; sap.el ends here
