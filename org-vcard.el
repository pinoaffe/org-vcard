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

;;
;; Internal variables not dependent on custom settings.
;;

(defconst org-vcard-elisp-dir (file-name-directory load-file-name)
  "Absolute path of the directory containing org-vcard.el.")

(defvar org-vcard-active-style ""
  "The currently-active contacts style.")

(defvar org-vcard-active-language ""
  "The currently-active language.")

(defvar org-vcard-active-version ""
  "The currently-active version of vCard.")

(defvar org-vcard-styles-functions
  (let ((the-list) '())
    (dolist (style (directory-files
                    (file-name-as-directory
                     (concat org-vcard-elisp-dir "styles"))
                    t))
      (if (and (not (string= "." (file-name-nondirectory style)))
             (not (string= ".." (file-name-nondirectory style))))
          (progn
            (load (concat
                   (file-name-as-directory style)
                   "functions.el"))
            (add-to-list 'the-list
                         `(,(file-name-nondirectory style)
                           ,(list
                             (intern (concat "org-vcard-export-from-" (file-name-nondirectory style)))
                             (intern (concat "org-vcard-import-to-" (file-name-nondirectory style)))))))))
    (sort the-list #'(lambda (a b)
                       (if (string< (car a) (car b))
                           t
                         nil))))
    "org-vcard internal variable, containing available styles and
their associated export and import functions.")

(defvar org-vcard-mappings-list
  (let ((mappings-list '())
        (styles-dir (file-name-as-directory
                     (concat org-vcard-elisp-dir "styles"))))
    (dolist (style (directory-files styles-dir))
      (if (and (not (string= "." (file-name-nondirectory style)))
             (not (string= ".." (file-name-nondirectory style))))
          (progn
            (dolist (mapping (directory-files
                              (file-name-as-directory
                               (concat
                                (file-name-as-directory
                                 (concat styles-dir style))
                                "mappings"))
                              t))
              (if (and (not (string= "." (file-name-nondirectory mapping)))
                     (not (string= ".." (file-name-nondirectory mapping))))
                  (add-to-list 'mappings-list
                               `(const :tag ,(concat
                                              style
                                              " - "
                                              (file-name-nondirectory mapping))
                                       ,mapping)))))))
    (sort mappings-list #'(lambda (a b)
                            (if (string< (nth 2 a) (nth 2 b))
                                t
                              nil))))
  "org-vcard internal variable, containing list of style mappings
suitable for use by the org-vcard-default-style-language-mapping
defcustom.")

(defvar org-vcard-styles-languages-mappings
  (let ((styles-dir (file-name-as-directory
                     (concat org-vcard-elisp-dir "styles")))
        (style-mappings '()))
    (dolist (style (directory-files styles-dir))
      (if (and (not (string= "." style))
             (not (string= ".." style)))
          (progn
            (let ((language-mapping '()))
              (dolist (mapping (directory-files
                                (file-name-as-directory
                                 (concat
                                  (file-name-as-directory
                                   (concat styles-dir style))
                                  "mappings"))
                                t))
                (if (and (not (string= "." (file-name-nondirectory mapping)))
                       (not (string= ".." (file-name-nondirectory mapping))))
                    (progn
                      (add-to-list 'language-mapping
                                    `(,(file-name-nondirectory mapping)
                                      ,@(list (car
                                               (read-from-string
                                                (with-temp-buffer
                                                  (insert-file-contents-literally mapping)
                                                  (buffer-string))))))))))
              (setq language-mapping (list language-mapping))
              (add-to-list 'style-mappings
                            `(,style
                              ,@language-mapping))))))
    style-mappings)
  "org-vcard internal variable, containing all styles and their
associated mappings.")


;;
;; Customisation setup.
;;


(defgroup org-vcard nil
  "vCard support for Org mode."
  :group 'org
  :prefix "org-vcard-")

;; We default to "flat", the style used by org-contacts.el.

(defcustom org-vcard-default-style-language-mapping (concat
                                                     (file-name-as-directory
                                                      (concat
                                                       (file-name-as-directory
                                                        (concat
                                                         (file-name-as-directory
                                                          (concat org-vcard-elisp-dir "styles"))
                                                         "flat"))
                                                       "mappings"))
                                                     "en")
  "The human-language contacts style mapping to use. Note:
Modifying this will require an Emacs restart before changes will
take effect."
  :type `(radio ,@org-vcard-mappings-list)
  :group 'org-vcard)

(defcustom org-vcard-default-export-file "~/org-vcard-export.vcf"
  "The default file to export to."
  :type 'file
  :group 'org-vcard)

(defcustom org-vcard-default-import-file "~/org-vcard-import.vcf"
  "The default file to import from."
  :type 'file
  :group 'org-vcard)

;; The in-buffer setting #+CONTACT_STYLE.

(defcustom org-vcard-default-style "flat"
  "Default contact style to use.
Initially set to \"flat\"."
  :type 'string
  :group 'org-vcard)

;; The in-buffer setting #+VCARD_VERSION;
;; can be "4.0", "3.0" or "2.1".

(defcustom org-vcard-default-version "4.0"
  "Default version of the vCard standard to use.
Initially set to 4.0."
  :type '(radio (const "4.0") (const "3.0") (const "2.1"))
  :group 'org-vcard)


;;
;; org-vcard-mode setup
;;


(defconst org-vcard-mode-keymap (make-sparse-keymap))

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
;; Utility functions.
;;


(defun org-vcard-check-contacts-styles ()
  "Utility function to check integrity of org-vcard-contacts-styles
variable."
  (let ((styles '()))
    (dolist (style org-vcard-styles-functions)
      (if (not (member (car style) styles))
          (setq styles (append styles `(,(car style))))
        (error (concat "Style '" (cadr style) "' appears more than once in org-vcards-contacts-styles")))
      (if (not (functionp (nth 0 (cadr style))))
          (error (concat "Style '" (car style) "' has an invalid export function")))
      (if (not (functionp (nth 1 (cadr style))))
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
    (let* ((valid-styles (mapcar 'car org-vcard-styles-functions))
           (valid-versions '("4.0" "3.0" "2.1"))
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
                  (if (member (plist-get (cadr this-line) :value) valid-versions)
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


(defun org-vcard-transfer-helper (source destination style language version direction)
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
    (setq org-vcard-active-language language)
    (setq org-vcard-active-version version)
    (cond
     ((eq 'export direction)
      (setq position 0))
     ((eq 'import direction)
      (setq position 1))
     (t
      (error "Invalid direction type")))
    (dolist (style org-vcard-styles-functions)
      (if (string= (car style) org-vcard-active-style)
          (funcall (nth position (cadr style)) source destination)))))


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
        (language "")
        (version ""))
    (setq style (completing-read "Style: " (mapcar 'car org-vcard-styles-functions)))
    (setq language (completing-read "Language: " (mapcar 'car (cadr (assoc style org-vcard-styles-languages-mappings)))))
    (setq version (completing-read "Version: " (mapcar 'car (cadr (assoc language (cadr (assoc style org-vcard-styles-languages-mappings)))))))
    (org-vcard-transfer-helper source destination style language version 'export)))


;;;###autoload
(defun org-vcard-import (source destination)
  "User command to import from vCard."
  (interactive (list
                (completing-read "Source: " '(file buffer region))
                (completing-read "Destination: " '(file buffer))))
  (let ((style "")
        (language "")
        (version ""))
    (setq style (completing-read "Style: " (mapcar 'car org-vcard-styles-functions)))
    (setq language (completing-read "Language: " (mapcar 'car (cadr (assoc style org-vcard-styles-languages-mappings)))))
    (setq version (completing-read "Version: " (mapcar 'car (cadr (assoc language (cadr (assoc style org-vcard-styles-languages-mappings)))))))
    (org-vcard-transfer-helper source destination style language version 'import)))


;;;###autoload
(defun org-vcard-export-via-menu (style language version)
  "User command for exporting to vCard via Emacs' menu bar."
  (let ((source nil)
        (destination nil))
    (setq source (completing-read "Source: " '(buffer region subtree)))
    (setq destination (completing-read "Destination: " '(file buffer)))
    (org-vcard-transfer-helper source destination style language version 'export)))


;;;###autoload
(defun org-vcard-import-via-menu (style language version)
  "User command for importing from vCard via Emacs' menu bar."
  (let ((source nil)
        (destination nil))
    (setq source (completing-read "Source: " '(file buffer region)))
    (setq destination (completing-read "Destination: " '(file buffer)))
    (org-vcard-transfer-helper source destination style language version 'import)))


(easy-menu-define org-vcard-menu org-vcard-mode-keymap "Menu bar entry for org-vcard"
  `("Org-vCard"
    ,(let ((export '("Export")))
       (let ((style-list '()))
         (dolist (style (sort (mapcar 'car org-vcard-styles-languages-mappings) 'string<))
           (setq style-list (list (concat "from " style)))
           (let ((language-list '()))
             (dolist (language (sort (mapcar 'car (cadr (assoc style org-vcard-styles-languages-mappings))) 'string<))
               (setq language-list (list language))
               (let ((version-list '()))
                 (dolist (version (sort (mapcar 'car (cadr (assoc language (cadr (assoc style org-vcard-styles-languages-mappings))))) 'string<))
                   (setq version-list (append version-list
                                              (list (vector
                                                     (concat "to vCard " version)
                                                     `(org-vcard-export-via-menu ,style ,language ,version) t)))))
                 (setq language-list (append language-list version-list)))
             (setq style-list (append style-list `(,language-list)))))
         (setq export (append export `(,style-list)))))
       export)
    ,(let ((import '("Import")))
       (let ((style-list '()))
         (dolist (style (sort (mapcar 'car org-vcard-styles-languages-mappings) 'string<))
           (setq style-list (list (concat "to " style)))
           (let ((language-list '()))
             (dolist (language (sort (mapcar 'car (cadr (assoc style org-vcard-styles-languages-mappings))) 'string<))
               (setq language-list (list language))
               (let ((version-list '()))
                 (dolist (version (sort (mapcar 'car (cadr (assoc language (cadr (assoc style org-vcard-styles-languages-mappings))))) 'string<))
                   (setq version-list (append version-list
                                              (list (vector
                                                     (concat "from vCard " version)
                                                     `(org-vcard-import-via-menu ,style ,language ,version) t)))))
                 (setq language-list (append language-list version-list)))
             (setq style-list (append style-list `(,language-list)))))
         (setq import (append import `(,style-list)))))
       import)
    ["Customize" (customize-group 'org-vcard) t]))


;; --

(provide 'org-vcard)

;;; org-vcard.el ends here
