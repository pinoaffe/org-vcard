;;; org-vcard.el --- org-mode support for vCard export and import. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
(declare-function org-vcard--get-mapping "org-vcard.el")
(declare-function org-vcard--get-encoding "org-vcard.el")
(declare-function org-vcard--card-name "org-vcard.el")
(declare-function org-vcard--property-without-pref "org-vcard.el")
(declare-function org-vcard--property-with-pref "org-vcard.el")
(declare-function org-vcard--property-name "org-vcard.el")
(declare-function org-vcard--remove-external-semicolons "org-vcard.el")
(declare-function org-vcard--ensure-n-property "org-vcard.el")

(declare-function org-narrow-to-subtree "org.el")
(declare-function org-get-heading "org.el")
(declare-function org-entry-properties "org.el")
(declare-function org-map-entries "org.el")

;; 

(defun org-vcard-export-from-tree (source destination)
  "Export tree-style SOURCE to vCard format DESTINATION.

SOURCE must be \"buffer\", \"region\" or \"subtree\".
DESTINATION must be either \"buffer\" or \"file\"."
  (let* ((mappings
          (org-vcard--get-mapping org-vcard-active-version
                                  org-vcard-active-language
                                  "tree"))
         (encoding (org-vcard--get-encoding org-vcard-active-version
                                            org-vcard-active-language))
         (scope
          (cond
           ((string= "buffer" source) nil)
           ((string= "region" source) 'region)
           ((string= "subtree" source) 'tree)
           (t (error "Invalid source type"))))
         (output (encode-coding-string "" encoding))
         (contact-encountered nil))
    (if (not (member source '("buffer" "region" "subtree")))
        (error "Invalid source type"))
    (org-mode)
    (org-map-entries
     (lambda ()
       (let* ((properties (org-entry-properties))
              (fieldtype (cdr (assoc "FIELDTYPE" properties))))
         (cond ((not fieldtype) t)
               ((string= fieldtype "version") t)
               ((string= fieldtype "name")
                (setq output
                      (concat output
                              (if contact-encountered
                                  (org-vcard--export-line "END" "VCARD")
                                "")
                              (org-vcard--export-line "BEGIN" "VCARD")
                              (org-vcard--export-line "VERSION" org-vcard-active-version)
                              (org-vcard--export-line org-vcard-default-property-for-heading
                                                      (org-get-heading t t))
                              (org-vcard--ensure-n-property org-vcard-active-version)))
                (setq contact-encountered t))
               ((not (assoc fieldtype mappings)) t)
               (t
                (setq output
                      (concat
                       output
                       (org-vcard--export-line
                        (if (assoc "PREFERRED" properties)
                            (org-vcard--property-with-pref org-vcard-active-version
                                                           (cdr (assoc (downcase fieldtype) mappings)))
                          (cdr (assoc (downcase fieldtype) mappings)))
                        (org-get-heading t t))))))))
     nil scope)
    (when contact-encountered
      ;; Finish the last vcard (if any)
      (setq output
            (concat output
                    (org-vcard--export-line "END" "VCARD"))))
    (org-vcard--transfer-write 'export output destination)))



(defun org-vcard-import-entry-to-tree (version _language mappings property value)
  "Output single PROPERTY/VALUE pair in tree format corresponding to MAPPINGS.

VERSION and LANGUAGE instruct what vCard variant to use."
  (let* ((property-name (org-vcard--property-name property))
         (property-without-pref (org-vcard--property-without-pref
                                 version
                                 property))
         (case-fold-search t))
    (if (and (not (member
                   property
                   `(,org-vcard-default-property-for-heading "KIND" "VERSION")))
             (or (car (rassoc property-without-pref mappings))
                 (car (rassoc property-name mappings))
                 org-vcard-include-import-unknowns))
      (format "** %s\n:PROPERTIES:\n:FIELDTYPE: %s\n%s:END:\n"
              (if (and org-vcard-remove-external-semicolons
                       (member property-name org-vcard-compound-properties))
                  ;; Remove leading and trailing semicolons from value of
                  ;; property.
                  (org-vcard--remove-external-semicolons value)
                value)
              (or (car (rassoc property-without-pref mappings))
                  (car (rassoc property-name mappings))
                  property-without-pref)
              (if (not (string= property-without-pref
                                property))
                  ;; Contents of 'property' were changed by
                  ;; replace-regexp-in-string, so it must have contained
                  ;; a 'PREF'.
                  ":PREFERRED:\n"
                ""))
      "")))

(defun org-vcard-import-card-to-tree (default-version language card)
  "Output (parsed) vCard CARD in tree format.

DEFAULT-VERSION and LANGUAGE instruct what vCard variant to use."
  (let* ((version (or (cdr (assoc "VERSION" card))
                      default-version
                      org-vcard-default-version))
         (org-vcard-active-version version)
         (mappings (org-vcard--get-mapping version language "tree")))
    (concat
     (format "* %s\n:PROPERTIES:\n:KIND: %s\n:FIELDTYPE: name\n:END:\n"
             (org-vcard--card-name card)
             (or (cdr (assoc "KIND" card))
                 "individual"))
     (string-join
      (mapcar (lambda (pair)
                (org-vcard-import-entry-to-tree version
                                                language
                                                mappings
                                                (car pair)
                                                (cdr pair)))
              (sort card (lambda (a b) (string< (car a) (car b)))))))))

(defun org-vcard-import-to-tree (source destination)
  "Import contents of SOURCE from vCard format to DESTINATION.

SOURCE must be one of \"buffer\", \"file\" or \"region\".
DESTINATION must be one of \"buffer\" or \"file\"."
  (if (not (member source '("buffer" "file" "region")))
      (error "Invalid source type"))
  (org-vcard--transfer-write 'import
                             (string-join
                              (mapcar (lambda (card)
                                        (org-vcard-import-card-to-tree
                                         org-vcard-active-version
                                         org-vcard-active-language
                                         card))
                                      (org-vcard-import-parse source)))
                             destination))
