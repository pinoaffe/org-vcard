;;; org-vcard --- org-mode support for vCard export and import. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org-element)

;; Declare the variables and functions of library `org-vcard' that
;; we use, to avoid compiler warnings.
;;
;; We can't simply `(require 'org-vcard)' as that would create a
;; circular dependency.

(defvar org-vcard-active-language)
(defvar org-vcard-active-version)
(defvar org-vcard-compound-properties)
(defvar org-vcard-default-property-for-heading)
(defvar org-vcard-default-version)
(defvar org-vcard-include-import-unknowns)
(defvar org-vcard-remove-external-semicolons)
(defvar org-vcard-styles-languages-mappings)

(declare-function org-vcard--export-line "org-vcard.el")
(declare-function org-vcard-import-parse "org-vcard.el")
(declare-function org-vcard--transfer-write "org-vcard.el")
(declare-function org-vcard--resolve-element-properties "org-vcard.el")
(declare-function org-vcard--remove-external-semicolons "org-vcard.el")
(declare-function org-vcard--get-encoding "org-vcard.el")
(declare-function org-vcard--get-mapping "org-vcard.el")
(declare-function org-vcard--card-name "org-vcard.el")
(declare-function org-vcard--property-name "org-vcard.el")
(declare-function org-vcard--ensure-n-property "org-vcard.el")


;;

(defun org-vcard-export-from-flat (source destination)
  "Export flat-style SOURCE to vCard format to DESTINATION.

SOURCE must be either \"buffer\", \"region\" or \"subtree\".
DESTINATION must be either \"buffer\" or \"file\"."
  (let* ((scope
          (cond
           ((string= "buffer" source) nil)
           ((string= "region" source) 'region)
           ((string= "subtree" source) 'tree)
           (t (error "Invalid source type"))))
         (encoding (org-vcard--get-encoding org-vcard-active-version
                                            org-vcard-active-language))
         (mappings
          (org-vcard--get-mapping org-vcard-active-version
                                  org-vcard-active-language
                                  "flat"))
         (output (encode-coding-string "" encoding)))
    (org-mode)
    (org-map-entries
     (lambda ()
       (let ((properties (org-entry-properties)))
         (when (seq-some (lambda (p) (assoc (car p) mappings))
                         properties)
           ;; Does this entry contain a PROPERTY listed in
           ;; the "flat" mapping? If so, assume we're in
           ;; a contact entry.
           (setq output
                 (concat
                  output
                  (org-vcard--export-line "BEGIN" "VCARD")
                  (org-vcard--export-line "VERSION" org-vcard-active-version)
                  (org-vcard--export-line org-vcard-default-property-for-heading
                                          (plist-get
                                           (cadr (org-vcard--resolve-element-properties
	    	                                      (org-element-headline-parser (line-end-position))))
                                           :raw-value))
                  (org-vcard--ensure-n-property org-vcard-active-version)
                  (string-join
                   (mapcar (lambda (p)
                             (org-vcard--export-line (cdr (assoc (car p) mappings))
                                                     (cdr p)))
                           (seq-filter (lambda (p)
                                         (and (not (string= "VERSION" (car p)))
                                              (assoc (car p) mappings)))
                                       properties)))
                  (org-vcard--export-line "END" "VCARD"))))))
     nil scope)
    (org-vcard--transfer-write 'export output destination)))

(defun org-vcard-import-entry-to-flat (mappings entry)
  "Import vCard contentline ENTRY to org flat style according to MAPPINGS."
  (let* ((property (car entry))
         (property-name (org-vcard--property-name property))
         (property-value (cdr entry)))
    (if (and (not (string= org-vcard-default-property-for-heading (car entry)))
             (or (car (rassoc property mappings))
                 org-vcard-include-import-unknowns))
        (format ":%s: %s\n"
                (or (car (rassoc property mappings))
                    property)
                (if (and org-vcard-remove-external-semicolons
                         (member property-name org-vcard-compound-properties))
                    ;; Remove leading and trailing semicolons from value of
                    ;; property.
                    (org-vcard--remove-external-semicolons property-value)
                  property-value))
      "")))

(defun org-vcard-import-card-to-flat (card)
  "Import vCard CARD to org flat style."
  (if (assoc "VERSION" card)
      (setq org-vcard-active-version (cdr (assoc "VERSION" card)))
    (setq org-vcard-active-version org-vcard-default-version))
  (let ((mappings (org-vcard--get-mapping org-vcard-active-version
                                         org-vcard-active-language
                                         "flat")))
    (format "* %s\n:PROPERTIES:\n%s:END:\n"
            (org-vcard--card-name card)
            (string-join
             (mapcar (lambda (entry)
                       (org-vcard-import-entry-to-flat mappings entry))
                     card)))))

(defun org-vcard-import-to-flat (source destination)
  "Import contents of SOURCE from vCard format to DESTINATION in flat style.

SOURCE must be one of \"buffer\", \"file\" or \"region\".
DESTINATION must be one of \"buffer\" or \"file\"."
  (when (not (member source '("buffer" "file" "region")))
    (error "Invalid source type"))
  (org-vcard--transfer-write 'import
                             (string-join
                              (mapcar #'org-vcard-import-card-to-flat
                                      (org-vcard-import-parse source)))
                             destination))
