;;; org-vcard.el --- org-mode support for vCard export and import.

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2014-07-31
;; Keywords: outlines, org, vcard
     
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; org-vcard is a package for exporting and importing vCards from
;; within Emacs' Org mode.

;; The main user commands are org-vcard-export and org-vcard-import.
;; Enabling org-vcard-mode will add an 'Org-vCard' menu to the menu
;; bar, from which one can access the various export, import and
;; customisation options.

;; This package is working towards full compliance with the
;; vCard specifications:

;; vCard 4.0: https://tools.ietf.org/html/rfc6350
;; vCard 3.0: https://tools.ietf.org/html/rfc2426
;; vCard 2.1: http://www.imc.org/pdi/vcard-21.txt

;; If you find any apparent instances of non-compliance that aren't
;; already noted in the TODO section of the org-vcard README.md
;; document, please let the maintainers know.

;; Differences between 4.0 and 3.0 can be found in Appendix A of
;; RFC6350: https://tools.ietf.org/html/rfc6350#page-73
;; Note that vCard 3.0 'types' became vCard 4.0 'properties'.

;; Differences between 3.0 and 2.1 can be found in Section 5 of
;; RFC2426: https://tools.ietf.org/html/rfc2426#page-37

;; Point of amusement:
;; In section 7 of RFC2426, the authors of the standard don't
;; include the 'N' type in their supposed-version-3.0 vCards.

;; Please refer to the TODO section of the org-vcard README.md
;; document for known limitations and/or issues.
;; 

;;; Code:

(require 'org)

(defgroup org-vcard nil
  "vCard support for Org mode."
  :group 'org-vcard
  :prefix "org-vcard-")

(defconst org-vcard-mode-keymap (make-sparse-keymap))

;; The per-file keyword #+VCARD_VERSION can be "4.0", "3.0" or "2.1".

(defcustom org-vcard-default-version "4.0"
  "Default version of the vCard standard to use.
Initially set to 4.0."
  :type '(radio (const "4.0") (const "3.0") (const "2.1"))
  :group 'org-vcard)

;; The per-file keyword #+CONTACTS_STYLE can be "flat" or "tree".
;;
;; "flat" is the style used by org-contacts.el. In the "flat"
;; mapping, we include all properties that org-contacts.el uses,
;; plus several more.
;;
;; "tree" is a new contacts style. The org-vcard README.md document
;; contains examples of tree-style contacts.

(defcustom org-vcard-default-style "flat"
  "The default contacts style to use."
  :type 'string
  :group 'org-vcard)

(defcustom org-vcard-contacts-styles '(("flat" .
                                        (org-vcard-export-from-flat org-vcard-import-to-flat))
                                       ("tree" .
                                        (org-vcard-export-from-tree org-vcard-import-to-tree)))
  "List representing available org-contacts styles and their associated
export/import functions."
  :type '(repeat (cons string (list symbol symbol)))
  :group 'org-vcard)

(defcustom org-vcard-contacts-styles-mappings '(("flat" .
                                                 (("4.0" .
                                                   (("[HEADING]" . "FN")
                                                    ("ADDRESS" . "ADR")
                                                    ("ADDRESS_HOME" . "ADR;TYPE=home")
                                                    ("ADDRESS_WORK" . "ADR;TYPE=work")
                                                    ("ALIAS" . "NICKNAME")
                                                    ("ANNIVERSARY" . "ANNIVERSARY")
                                                    ("BIRTHDAY" . "BDAY")
                                                    ("CELL" . "TEL;TYPE=cell")
                                                    ("EMAIL" . "EMAIL")
                                                    ("EMAIL_HOME" . "EMAIL;TYPE=home")
                                                    ("EMAIL_WORK" . "EMAIL;TYPE=work")
                                                    ("GENDER" . "GENDER")
                                                    ("ICON" . "PHOTO")
                                                    ("IMPP" . "IMPP")
                                                    ("IMPP_HOME" . "IMPP;TYPE=home")
                                                    ("IMPP_WORK" . "IMPP;TYPE=work")
                                                    ("LANDLINE" . "TEL;TYPE=voice")
                                                    ("LANDLINE_HOME" . "TEL;TYPE=\"voice,home\"")
                                                    ("LANDLINE_WORK" . "TEL;TYPE=\"voice,work\"")
                                                    ("MOBILE" . "TEL;TYPE=cell")
                                                    ("MOBILE_HOME" . "TEL;TYPE=\"cell,home\"")
                                                    ("MOBILE_WORK" . "TEL;TYPE=\"cell,work\"")
                                                    ("N" . "N")
                                                    ("NICKNAME" . "NICKNAME")
                                                    ("NOTE" . "NOTE")
                                                    ("PHONE" . "TEL;TYPE=voice")
                                                    ("PHONE_HOME" . "TEL;TYPE=\"voice,home\"")
                                                    ("PHONE_WORK". "TEL;TYPE=\"voice,work\"")
                                                    ("PHOTO" . "PHOTO")
                                                    ("VERSION" . "VERSION")))
                                                  ("3.0" .
                                                   (("[HEADING]" . "FN")
                                                    ("ADDRESS" . "ADR")
                                                    ("ADDRESS_HOME" . "ADR;TYPE=home")
                                                    ("ADDRESS_WORK" . "ADR;TYPE=work")
                                                    ("ALIAS" . "NICKNAME")
                                                    ("BIRTHDAY" . "BDAY")
                                                    ("CELL" . "TEL;TYPE=cell")
                                                    ("CELL_HOME" . "TEL;TYPE=cell,home")
                                                    ("CELL_WORK" . "TEL;TYPE=cell,work")
                                                    ("EMAIL" . "EMAIL")
                                                    ("EMAIL_HOME" . "EMAIL;TYPE=home")
                                                    ("EMAIL_WORK" . "EMAIL;TYPE=work")
                                                    ("ICON" . "PHOTO")
                                                    ("IMPP" . "IMPP") ; Defined in RFC 4770
                                                    ("IMPP_HOME" . "IMPP;TYPE=home")
                                                    ("IMPP_WORK" . "IMPP;TYPE=work")
                                                    ("LANDLINE" . "TEL;TYPE=voice")
                                                    ("LANDLINE_HOME" . "TEL;TYPE=voice,home")
                                                    ("LANDLINE_WORK" . "TEL;TYPE=voice,work")
                                                    ("MOBILE" . "TEL;TYPE=cell")
                                                    ("MOBILE_HOME" . "TEL;TYPE=cell,home")
                                                    ("MOBILE_WORK" . "TEL;TYPE=cell,work")
                                                    ("N" . "N")
                                                    ("NICKNAME" . "NICKNAME")
                                                    ("NOTE" . "NOTE")
                                                    ("PHONE" . "TEL;TYPE=voice")
                                                    ("PHONE_HOME" . "TEL;TYPE=voice,home")
                                                    ("PHONE_WORK". "TEL;TYPE=voice,work")
                                                    ("PHOTO" . "PHOTO")
                                                    ("VERSION" . "VERSION")))
                                                  ("2.1" .
                                                   (("[HEADING]" . "FN")
                                                    ("ADDRESS" . "ADR")
                                                    ("ADDRESS_HOME" . "ADR;HOME")
                                                    ("ADDRESS_WORK" . "ADR;WORK")
                                                    ("BIRTHDAY" . "BDAY")
                                                    ("CELL" . "TEL;CELL")
                                                    ("CELL_HOME" . "TEL;CELL;HOME")
                                                    ("CELL_WORK" . "TEL;CELL;WORK")
                                                    ("EMAIL" . "EMAIL")
                                                    ("EMAIL_HOME" . "EMAIL;HOME")
                                                    ("EMAIL_WORK" . "EMAIL;WORK")
                                                    ("LANDLINE" . "TEL;VOICE")
                                                    ("LANDLINE_HOME" . "TEL;VOICE;HOME")
                                                    ("LANDLINE_WORK" . "TEL;VOICE;WORK")
                                                    ("MOBILE" . "TEL;CELL")
                                                    ("MOBILE_HOME" . "TEL;CELL;HOME")
                                                    ("MOBILE_WORK" . "TEL;CELL;WORK")
                                                    ("N" . "N")
                                                    ("NAME" . "NAME")
                                                    ("NOTE" . "NOTE")
                                                    ("PHONE" . "TEL;VOICE")
                                                    ("PHONE_HOME" . "TEL;VOICE;HOME")
                                                    ("PHONE_WORK" . "TEL;VOICE;WORK")
                                                    ("PHOTO" . "PHOTO")
                                                    ("VERSION" . "VERSION")))))
                                                ("tree" .
                                                 (("4.0" .
                                                   (("address" . "ADR")
                                                    ("address-home" . "ADR;TYPE=home")
                                                    ("address-work" . "ADR;TYPE=work")
                                                    ("alias" . "NICKNAME")
                                                    ("anniversary" . "ANNIVERSARY")
                                                    ("birthday" . "BDAY")
                                                    ("cell" . "TEL;TYPE=cell")
                                                    ("email" . "EMAIL")
                                                    ("email-home" . "EMAIL;TYPE=home")
                                                    ("email-work" . "EMAIL;TYPE=work")
                                                    ("gender" . "GENDER")
                                                    ("icon" . "PHOTO")
                                                    ("impp" . "IMPP")
                                                    ("impp-home" . "IMPP;TYPE=home")
                                                    ("impp-work" . "IMPP:TYPE=work")
                                                    ("landline" . "TEL;TYPE=voice")
                                                    ("landline-home" . "TEL;TYPE=\"voice,home\"")
                                                    ("landline-work" . "TEL;TYPE=\"voice,home\"")
                                                    ("mobile" . "TEL;TYPE=cell")
                                                    ("mobile-home" . "TEL;TYPE=\"cell,home\"")
                                                    ("mobile-work" . "TEL;TYPE=\"cell,home\"")
                                                    ("n" . "N")
                                                    ("name" . "FN")
                                                    ("nickname" . "NICKNAME")
                                                    ("note" . "NOTE")
                                                    ("phone" . "TEL;TYPE=voice")
                                                    ("phone-home" . "TEL;TYPE=\"voice,home\"")
                                                    ("phone-work" . "TEL;TYPE=\"voice,work\"")
                                                    ("photo" . "PHOTO")))
                                                  ("3.0" .
                                                   (("address" . "ADR")
                                                    ("address-home" . "ADR;TYPE=home")
                                                    ("address-work" . "ADR;TYPE=work")
                                                    ("alias" . "NICKNAME")
                                                    ("birthday" . "BDAY")
                                                    ("cell" . "TEL;TYPE=cell")
                                                    ("cell-home" . "TEL;TYPE=cell,home")
                                                    ("cell-work" . "TEL;TYPE=cell,work")
                                                    ("email" . "EMAIL")
                                                    ("email-home" . "EMAIL;TYPE=home")
                                                    ("email-work" . "EMAIL;TYPE=work")
                                                    ("icon"  . "PHOTO")
                                                    ("impp" . "IMPP") ; Defined in RFC 4770
                                                    ("impp-home" . "IMPP;TYPE=home")
                                                    ("impp-work" . "IMPP;TYPE=work")
                                                    ("landline" . "TEL;TYPE=voice")
                                                    ("landline-home" . "TEL;TYPE=voice,home")
                                                    ("landline-work" . "TEL;TYPE=voice,work")
                                                    ("mobile" . "TEL;TYPE=cell")
                                                    ("mobile-home" . "TEL;TYPE=cell,home")
                                                    ("mobile-work" . "TEL;TYPE=cell,work")
                                                    ("n" . "N")
                                                    ("nickname" . "NICKNAME")
                                                    ("note" . "NOTE")
                                                    ("phone" . "TEL;TYPE=voice")
                                                    ("phone-home" . "TEL;TYPE=voice,home")
                                                    ("phone-work" . "TEL;TYPE=voice,work")
                                                    ("photo" . "PHOTO")
                                                    ("version" . "VERSION")))
                                                  ("2.1" .
                                                   (("address" . "ADR")
                                                    ("address-home" . "ADR;HOME")
                                                    ("address-work" . "ADR;WORK")
                                                    ("birthday" . "BDAY")
                                                    ("cell" . "TEL;CELL")
                                                    ("cell-home" . "TEL;CELL;HOME")
                                                    ("cell-work" . "TEL;CELL;WORK")
                                                    ("email" . "EMAIL")
                                                    ("email-home" . "EMAIL;HOME")
                                                    ("email-work" . "EMAIL;WORK")
                                                    ("landline" . "TEL;VOICE")
                                                    ("landline-home" . "TEL;VOICE;HOME")
                                                    ("landline-work" . "TEL;VOICE;WORK")
                                                    ("mobile" . "TEL;CELL")
                                                    ("mobile-home" . "TEL;CELL;HOME")
                                                    ("mobile-work" . "TEL;CELL;WORK")
                                                    ("n" . "N")
                                                    ("name" . "NAME")
                                                    ("note" . "NOTE")
                                                    ("phone" . "TEL;VOICE")
                                                    ("phone-home" . "TEL;VOICE;HOME")
                                                    ("phone-work" . "TEL;VOICE;WORK")
                                                    ("photo" . "PHOTO")
                                                    ("version" . "VERSION"))))))
  "List of per-vCard-version mappings for each contacts style."
  :type '(repeat (cons string (repeat (cons string (repeat (cons string string))))))
  :group 'org-vcard)

(defcustom org-vcard-default-export-file "~/org-vcard-export.vcf"
  "The default file to export to."
  :type 'file
  :group 'org-vcard)

(defcustom org-vcard-default-import-file "~/org-vcard-import.vcf"
  "The default file to import from."
  :type 'file
  :group 'org-vcard)

(define-minor-mode org-vcard-mode
  "Toggle org-vcard mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When org-vcard mode is enabled, an Org-vCard entry is added
to Emacs' menu bar."
      nil                     ; The initial value.
      nil                     ; The indicator for the mode line.
      org-vcard-mode-keymap ; The minor mode bindings.
      :group 'org-vcard)


;;
;; Internal variables.
;;

(defvar org-vcard-active-version ""
  "The currently-active version of vCard.")

(defvar org-vcard-active-style ""
  "The currently-active org-contacts style.")


;;
;; Utility functions.
;;


(defun org-vcard-check-contacts-styles ()
  "Utility function to check integrity of org-vcard-contacts-styles
variable."
  (let ((styles '()))
    (dolist (style org-vcard-contacts-styles)
      (if (not (member (car style) styles))
          (setq styles (append styles `(,(car style))))
        (error (concat "Style '" (car style) "' appears more than once in org-vcards-contacts-styles")))
      (if (not (functionp (nth 0 (cdr style))))
          (error (concat "Style '" (car style) "' has an invalid export function")))
      (if (not (functionp (nth 1 (cdr style))))
          (error (concat "Style '" (car style) "' has an invalid import function"))))))


(defun org-vcard-escape-value-string (characters value)
  "Utility function to escape each instance of each character
specified in CHARACTERS.

CHARACTERS must be a list of strings. VALUE is the string to be
escaped."
  (if (member "\134" characters)
      ;; Process backslashes first.
      (setq value (replace-regexp-in-string "\134\134" "\134\134" value nil t)))
  (dolist (char characters)
    (if (not (string= "\134" char))
        ;; We're escaping a non-backslash character.
        (setq value (replace-regexp-in-string char (concat "\134" char) value nil t))))
  value)


(defun org-vcard-export-line (property value &optional noseparator)
  "Utility function to ensure each line is exported as appropriate
for each vCard version.

PROPERTY is the vCard property/type to output, VALUE its value.
If NOSEPARATOR is non-nil, don't output colon to separate PROPERTY
from VALUE."
  (let ((separator ":"))
    (if noseparator
        (setq separator ""))
    (cond
     ((string= org-vcard-active-version "4.0")
      ;; In values, escape commas, semicolons and backslashes.
      ;; End line with U+000D U+000A.
      ;; Output must be UTF-8.
      (encode-coding-string (concat
                             property
                             separator
                             (org-vcard-escape-value-string '("," ";" "\134") value)
                             "\u000D\u000A")
                            'utf-8))
     ((string= org-vcard-active-version "3.0")
      ;; In values, escape commas and semicolons.
      ;; End line with CRLF.
      ;; RFC2426 doesn't seem to mandate an encoding, so output UTF-8.
      (encode-coding-string (concat
                             property
                             separator
                             (org-vcard-escape-value-string '("," ";") value)
                             "\015\012")
                            'utf-8))
     ((string= org-vcard-active-version "2.1")
      ;; In values, escape semicolons.
      ;; End line with CRLF.
      ;; Output ASCII.
      (encode-coding-string (concat
                             property
                             separator
                             (org-vcard-escape-value-string '(";") value)
                             "\015\012")
                            'us-ascii)))))


(defun org-vcard-set-active-settings ()
  "Utility function to set active settings based on value of last
instance of in-buffer setting; fall back to value of custom
variables."
  (save-excursion
    (goto-char (point-min))
    (let* ((valid-styles (mapcar 'car org-vcard-contacts-styles-mappings))
           (valid-versions (let ((result '()))
                             (dolist (style valid-styles)
                               (let ((this-version '()))
                                 (setq this-version (car (assoc style org-vcard-contacts-styles-mappings)))
                                 (let ((version-list '()))
                                   (dolist (version (cdr (assoc style org-vcard-contacts-styles-mappings)))
                                     (setq version-list (append version-list
                                                                (list (car version)))))
                                   (setq result (append result (list (cons this-version (list version-list))))))))
                             result))
          (found-keywords '()))
      (while (not (eobp))
        (if (looking-at "^#+")
            (let ((this-line (org-element-keyword-parser nil nil)))
              (when (eq 'keyword (car this-line))
                (cond
                 ((string= "CONTACTS_STYLE" (plist-get (cadr this-line) :key))
                  (if (member (plist-get (cadr this-line) :value) valid-styles)
                      (progn
                        (setq org-vcard-active-style (plist-get (cadr this-line) :value))
                        (setq found-keywords (append found-keywords '("CONTACTS_STYLE"))))
                    (error "Invalid in-buffer setting for CONTACTS_STYLE")))
                 ((string= "VCARD_VERSION" (plist-get (cadr this-line) :key))
                  (if (member (plist-get (cadr this-line) :value) (cadr (assoc org-vcard-active-style valid-versions)))
                      (progn
                        (setq org-vcard-active-version (plist-get (cadr this-line) :value))
                        (setq found-keywords (append found-keywords '("VCARD_VERSION"))))
                    (error "Invalid in-buffer setting for VCARD_VERSION")))))))
        (forward-line))
      (cond
       ((not (member "CONTACTS_STYLE" found-keywords))
        (setq org-vcard-active-style org-vcard-default-style))
       ((not (member "VCARD_VERSION" found-keywords))
        (setq org-vcard-active-version org-vcard-default-version))))))


(defun org-vcard-import-parser (source)
  "Utility function to read from SOURCE and return a list of
vCards, each in the form of a list of cons cells, with each
cell containing the vCard property in the car, and the value
of that property in the cdr.

SOURCE must be one of \"file\", \"buffer\" or \"region\"."
  (let ((current-line nil)
        (property "")
        (value "")
        (cards '())
        (current-card '()))
    (cond
     ((string= "file" source)
      (find-file (read-from-minibuffer "Filename? " org-vcard-default-import-file)))
     ((string= "region" source)
      (narrow-to-region (region-beginning) (region-end)))
     ((string= "buffer" source)
      t)
     (t
      (error "Invalid source type")))
    (goto-char (point-min))
    (setq case-fold-search t)
    (while (re-search-forward "BEGIN:VCARD" (point-max) t)
      (setq current-card '())
      (forward-line)
      (while (not (looking-at "END:VCARD"))
        (setq current-line
              (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (string-match "\\([^:]+\\): *\\(.*?\\)\\(?:\u000D\\|\015\\)?$" current-line)
        (setq property (match-string 1 current-line))
        (setq value (match-string 2 current-line))
        (setq current-card (append current-card (list (cons property value))))
        (forward-line))
      (setq cards (append cards (list current-card))))
   cards))


(defun org-vcard-write-to-destination (content destination)
  "Utility function to write CONTENT to DESTINATION.

CONTENT must be a string. DESTINATION must be either \"buffer\" or \"file\"."
  (if (not (stringp content))
      (error "Received non-string as CONTENT"))
  (cond
   ((string= "buffer" destination)
    (progn
      (generate-new-buffer "*org-vcard-export*")
      (set-buffer "*org-vcard-export*")
      (insert content)))
   ((string= "file" destination)
    (let ((filename (read-from-minibuffer "Filename? " org-vcard-default-export-file)))
        (with-temp-buffer
          (insert content)
          (when (file-writable-p filename)
            (write-region (point-min)
                          (point-max)
                          filename)))))
   (t
    (error "Invalid destination type"))))


;;
;; The core export/import functions.
;;


(defun org-vcard-export-from-flat (source destination)
  "Export flat-style SOURCE to vCard format, sending output
to DESTINATION.

SOURCE must be either \"buffer\", \"region\" or \"subtree\".
DESTINATION must be either \"buffer\" or \"file\"."
  (let* ((scope (cond
                 ((string= "buffer" source) nil)
                 ((string= "region" source) 'region)
                 ((string= "subtree" source) 'tree)
                 (t (error "Invalid source type"))))
         (encoding (cond
                    ((string= "4.0" org-vcard-active-version) 'utf-8)
                    ((string= "3.0" org-vcard-active-version) 'utf-8)
                    ((string= "2.1" org-vcard-active-version) 'us-ascii)))
         (output (encode-coding-string "" encoding)))
    (org-map-entries
     (lambda ()
       (let ((properties (org-entry-properties))
             (in-contact-entry nil)
             (flat-style-properties
              (or (assoc org-vcard-active-version (cdr (assoc "flat" org-vcard-contacts-styles-mappings)))
                 (error "No mapping available for specified vCard version")))
             (content (encode-coding-string "" encoding)))
         ;; Does this entry contain a PROPERTY listed in
         ;; the "flat" mapping? If so, assume we're in
         ;; a contact entry.
         (dolist (p properties)
           (if (assoc (car p) flat-style-properties)
               (setq in-contact-entry t)))
         (when in-contact-entry
           ;; vCard 2.1 and 3.0 require the 'N' property be present.
           ;; Trying to create this by parsing the heading is
           ;; fraught with challenges - cf.
           ;; http://www.kalzumeus.com/2010/06/17/falsehoods-programmers-believe-about-names/
           ;; - so we just create an empty 'N' property.
           (if (and (or (string= "3.0" org-vcard-active-version)
                     (string= "2.1" org-vcard-active-version))
                  (not (member "N" properties)))
               (setq content (concat
                              content
                              (org-vcard-export-line "N" ""))))
           (dolist (p properties)
             (if (assoc (car p) flat-style-properties)
                 (setq content (concat
                                content
                                (org-vcard-export-line
                                 (cdr (assoc (car p) flat-style-properties))
                                 (cdr p))))))
           (setq output
                 (concat
                  output
                  (org-vcard-export-line "BEGIN" "VCARD")
                  (org-vcard-export-line "VERSION" org-vcard-active-version)
                  (org-vcard-export-line "FN" (plist-get
                                               (nth 1 (org-element-headline-parser (line-end-position)))
                                               :raw-value))
                  content
                  (org-vcard-export-line "END" "VCARD"))))))
     nil scope)
    (org-vcard-write-to-destination output destination)))


(defun org-vcard-import-to-flat (source destination)
  "Import contents of SOURCE from vCard format, sending flat-style
OUTPUT to DESTINATION.

SOURCE must be one of \"buffer\", \"file\" or \"region\".
DESTINATION must be one of \"buffer\" or \"file\"."
  (let ((cards (org-vcard-import-parser source))
        (filename "")
        (vcard-property-for-heading "")
        (heading ""))
    (if (not (member source '("buffer" "file" "region")))
        (error "Invalid source type"))
    (cond
     ((string= "buffer" destination)
      (generate-new-buffer "*org-vcard-import*")
      (set-buffer "*org-vcard-import*"))
     ((string= "file" destination)
      (setq filename (read-from-minibuffer "Filename? " "org-vcard-import.org"))
      (find-file filename))
     (t
      (error "Invalid destination type")))
    (dolist (card cards)
      (if (assoc "VERSION" card)
          (setq org-vcard-active-version (cdr (assoc "VERSION" card)))
        (setq org-vcard-active-version org-vcard-default-version))
      (setq vcard-property-for-heading
            (cdr
             (assoc "[HEADING]"
                    (assoc org-vcard-active-version
                           (assoc "flat" org-vcard-contacts-styles-mappings)))))
      (setq heading (cdr (assoc vcard-property-for-heading card)))
      (insert (concat "* " heading "\n"))
      (insert ":PROPERTIES:\n")
      (dolist (entry card)
        (if (not (string= vcard-property-for-heading (car entry)))
            (insert (concat ":"
                            (car (rassoc (car entry)
                                         (cdr (assoc org-vcard-active-version
                                                     (cdr (assoc org-vcard-active-style
                                                                 org-vcard-contacts-styles-mappings))))))
                            ": "
                            (cdr entry)
                            "\n"))))
      (insert ":END:\n"))
    (if (string= "file" destination)
        (write-file filename))))


(defun org-vcard-export-from-tree (source destination)
  "Export tree-style SOURCE to vCard format, sending output
to DESTINATION.

SOURCE must be \"buffer\", \"region\" or \"subtree\".
DESTINATION must be either \"buffer\" or \"file\"."
  (let* ((in-contact-entry nil)
         (tree-style-properties
          (or (assoc org-vcard-active-version (cdr (assoc "tree" org-vcard-contacts-styles-mappings)))
             (error "No mapping available for specified vCard version")))
         (encoding (cond
                    ((string= "4.0" org-vcard-active-version) 'utf-8)
                    ((string= "3.0" org-vcard-active-version) 'utf-8)
                    ((string= "2.1" org-vcard-active-version) 'us-ascii)))
         (output (encode-coding-string "" encoding)))
    (if (not (member source '("buffer" "region" "subtree")))
        (error "Invalid source type"))
    (save-excursion
      (let ((mapping (assoc org-vcard-active-version
                            (assoc "tree" org-vcard-contacts-styles-mappings)))
            (search-result nil))
        (cond
         ((string= "region" source)
          (narrow-to-region (region-beginning) (region-end)))
         ((string= "subtree" source)
          (org-narrow-to-subtree)))
        (goto-char (point-min))
        (setq case-fold-search t)
        (while (re-search-forward "\\s *:FIELDTYPE:\\s *name" nil t)
          (let ((content (concat (org-vcard-export-line "BEGIN:VCARD" "" t)
                                 (org-vcard-export-line "VERSION" org-vcard-active-version)))
                (end-vcard nil))
            (setq content (concat content
                                  (org-vcard-export-line "FN" (org-get-heading t t))))
            ;; vCard 2.1 and 3.0 require the 'N' property be present.
            ;; Trying to create this by parsing the heading which has
            ;; FIELDTYPE 'name' is fraught with challenges - cf.
            ;; http://www.kalzumeus.com/2010/06/17/falsehoods-programmers-believe-about-names/
            ;; - so we just create an empty 'N' property.
            (if (or (string= "3.0" org-vcard-active-version)
                   (string= "2.1" org-vcard-active-version))
                (setq content (concat content
                                      (org-vcard-export-line "N" ""))))
            (while (and (setq search-result (re-search-forward "\\s *:FIELDTYPE:\\s *\\(\\(?:\\w\\|-\\)+\\)" nil t))
                      (not end-vcard))
              (let ((fieldtype (match-string 1)))
                (if (not (string= "name" (downcase fieldtype)))
                    (setq content (concat content
                                          (org-vcard-export-line
                                           (cdr (assoc (downcase fieldtype) mapping))
                                           (org-get-heading t t))))
                  (setq end-vcard t))))
            (setq content (concat content
                                  (org-vcard-export-line "END:VCARD" "" t)))
            (setq output (concat output content)))
          (if search-result
              (re-search-backward "\\s *:FIELDTYPE:\\s *name")))
        (org-vcard-write-to-destination output destination)))))


(defun org-vcard-import-to-tree (source destination)
  "Import contents of SOURCE from vCard format, sending tree-style
OUTPUT to DESTINATION.

SOURCE must be one of \"buffer\", \"file\" or \"region\".
DESTINATION must be one of \"buffer\" or \"file\"."
  (let ((cards (org-vcard-import-parser source))
        (filename "")
        (sorted-card-properties nil))
    (if (not (member source '("buffer" "file" "region")))
        (error "Invalid source type"))
    (cond
     ((string= "buffer" destination)
      (generate-new-buffer "*org-vcard-import*")
      (set-buffer "*org-vcard-import*"))
     ((string= "file" destination)
      (setq filename (read-from-minibuffer "Filename? " "org-vcard-import.org"))
      (find-file filename))
     (t
      (error "Invalid destination type")))
    (dolist (card cards)
      (if (assoc "VERSION" card)
          (setq org-vcard-active-version (cdr (assoc "VERSION" card)))
        (setq org-vcard-active-version org-vcard-default-version))
      (insert (concat "* " (cdr (assoc "FN" card)) "\n"
                      ":PROPERTIES:\n"
                      ":KIND: " (if (assoc "KIND" card)
                                    (cdr (assoc "KIND" card))
                                  "individual") "\n"
                      ":FIELDTYPE: name\n"
                      ":END:\n"))
      (setq sorted-card-properties (sort (mapcar 'car card) 'string<))
      (dolist (property sorted-card-properties)
        (if (not (member property '("FN" "KIND" "VERSION")))
            (progn
              (insert (concat "** " (cdr (assoc property card)) "\n"))
              (insert (concat ":PROPERTIES:\n"
                              ":FIELDTYPE: "
                              (car (rassoc property
                                           (cdr (assoc org-vcard-active-version
                                                       (cdr (assoc "tree"
                                                                   org-vcard-contacts-styles-mappings))))))
                              "\n"
                              ":END:\n"))))
        (setq card (delq (assoc property card) card))))))


(defun org-vcard-transfer-helper (source destination style version direction)
  "Utility function via which other functions can dispatch export
and import requests to the appropriate functions.

Appropriate values for SOURCE and DESTINATION are determined by
the functions called. Appropriate values for STYLE and VERSION are
determined by the contents of the org-vcard-contacts-styles-mappings
variable. DIRECTION must be either the symbol 'export or the symbol
'import."
  (let ((position nil))
    (org-vcard-check-contacts-styles)
    (setq org-vcard-active-style style)
    (setq org-vcard-active-version version)
    (cond
     ((eq 'export direction)
      (setq position 0))
     ((eq 'import direction)
      (setq position 1))
     (t
      (error "Invalid direction type")))
    (dolist (style org-vcard-contacts-styles)
      (if (string= (car style) org-vcard-active-style)
          (funcall (nth position (cdr style)) source destination)))))


;;
;; User-facing commands for export and import.
;;


;;;###autoload

(defun org-vcard-export (source destination)
  "User command to export to vCard."
  (interactive (list
                (completing-read "Source: " '(buffer region subtree))
                (completing-read "Destination: " '(file buffer))))
  (let ((style "")
        (version ""))
    (setq style (completing-read "Style: " (mapcar 'car org-vcard-contacts-styles)))
    (setq version (completing-read "Version: " (mapcar 'car (cdr (assoc style org-vcard-contacts-styles-mappings)))))
    (org-vcard-transfer-helper source destination style version 'export)))


;;;###autoload
(defun org-vcard-import (source destination)
  "User command to import from vCard."
  (interactive (list
                (completing-read "Source: " '(file buffer region))
                (completing-read "Destination: " '(file buffer))))
  (let ((style "")
        (version ""))
    (setq style (completing-read "Style: " (mapcar 'car org-vcard-contacts-styles)))
    (setq version (completing-read "Version: " (mapcar 'car (cdr (assoc style org-vcard-contacts-styles-mappings)))))
    (org-vcard-transfer-helper source destination style version 'import)))


;;;###autoload
(defun org-vcard-export-via-menu (style version)
  "User command for exporting to vCard via Emacs' menu bar."
  (let ((source nil)
        (destination nil))
    (setq source (completing-read "Source: " '(buffer region subtree)))
    (setq destination (completing-read "Destination: " '(file buffer)))
    (org-vcard-transfer-helper source destination style version 'export)))


;;;###autoload
(defun org-vcard-import-via-menu (style version)
  "User command for importing from vCard via Emacs' menu bar."
  (let ((source nil)
        (destination nil))
    (setq source (completing-read "Source: " '(file buffer region)))
    (setq destination (completing-read "Destination: " '(file buffer)))
    (org-vcard-transfer-helper source destination style version 'import)))


(easy-menu-define org-vcard-menu org-vcard-mode-keymap "Menu bar entry for org-vcard"
  `("Org-vCard"
    ,(let ((export '("Export")))
       (let ((style-list nil))
         (dolist (style org-vcard-contacts-styles-mappings)
           (setq style-list (list (concat "from " (car style))))
           (let ((version-list '()))
             (dolist (version (cdr (assoc (car style) org-vcard-contacts-styles-mappings)))
               (setq version-list (append version-list
                                          (list (vector
                                                 (concat "to vCard " (car version))
                                                 `(org-vcard-export-via-menu ,(car style) ,(car version)) t)))))
             (setq style-list (append style-list version-list)))
           (setq export (append export `(,style-list)))))
      export)
    ,(let ((import '("Import")))
      (let ((style-list nil))
        (dolist (style org-vcard-contacts-styles-mappings)
          (setq style-list (list (concat "to " (car style))))
          (let ((version-list '()))
            (dolist (version (cdr (assoc (car style) org-vcard-contacts-styles-mappings)))
              (setq version-list (append version-list
                                         (list (vector
                                                (concat "from vCard " (car version))
                                                `(org-vcard-import-via-menu ,(car style) ,(car version)) t)))))
            (setq style-list (append style-list version-list)))
          (setq import (append import `(,style-list)))))
      import)
    ["Customize" (customize-group 'org-vcard) t]))


;; --

(provide 'org-vcard)

;;; org-vcard.el ends here
