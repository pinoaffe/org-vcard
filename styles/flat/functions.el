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
    (org-mode)
    (org-map-entries
     (lambda ()
       (let ((properties (org-entry-properties))
             (in-contact-entry nil)
             (flat-style-properties
              (or (cadr (assoc org-vcard-active-version
                               (cadr (assoc org-vcard-active-language
                                            (cadr (assoc "flat" org-vcard-styles-languages-mappings))))))
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
                    (not (member "N" (mapcar 'car properties))))
               (setq content (concat
                              content
                              (org-vcard-export-line "N" ""))))
           (dolist (p properties)
             (if (and (not (string= "VERSION" (car p)))
                      (assoc (car p) flat-style-properties))
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
    (org-vcard-write-to-destination output destination)
    (cond
     ((string= "buffer" destination)
      (message "Exported contacts data to *org-vcard-export* buffer."))
     ((string= "file" destination)
      (message "Exported contacts data to file.")))))


(defun org-vcard-import-to-flat (source destination)
  "Import contents of SOURCE from vCard format, sending flat-style
OUTPUT to DESTINATION.

SOURCE must be one of \"buffer\", \"file\" or \"region\".
DESTINATION must be one of \"buffer\" or \"file\"."
  (let ((cards (org-vcard-import-parser source))
        (import-buffer nil)
        (filename "")
        (vcard-property-for-heading "")
        (heading ""))
    (if (not (member source '("buffer" "file" "region")))
        (error "Invalid source type"))
    (cond
     ((string= "buffer" destination)
      (progn
        (if org-vcard-append-to-existing-import-buffer
            (setq import-buffer (get-buffer-create "*org-vcard-import*"))
          (setq import-buffer (generate-new-buffer "*org-vcard-import*")))
        (set-buffer import-buffer)))
     ((string= "file" destination)
      (setq filename (read-from-minibuffer "Filename? " "org-vcard-import.org"))
      (find-file filename))
     (t
      (error "Invalid destination type")))
    (let ((flat-style-properties
           (or (cadr (assoc org-vcard-active-version
                           (cadr (assoc org-vcard-active-language
                                        (cadr (assoc "flat" org-vcard-styles-languages-mappings))))))
              (error "No mapping available for specified vCard version"))))
      (dolist (card cards)
        (if (assoc "VERSION" card)
            (setq org-vcard-active-version (cdr (assoc "VERSION" card)))
          (setq org-vcard-active-version org-vcard-default-version))
        (setq vcard-property-for-heading
              (cdr (assoc "[HEADING]" flat-style-properties)))
        (setq heading
              (or (cdr (assoc vcard-property-for-heading card))
                  (replace-regexp-in-string "^;\\|;$" ""
                                            (cdr (assoc "N" card)))))
        (insert (concat "* " heading "\n"))
        (insert ":PROPERTIES:\n")
        (dolist (entry card)
          (if (not (string= vcard-property-for-heading (car entry)))
              (let* ((property (car entry))
                     (property-name (progn
                                      (string-match "^[^;:]+" property)
                                      (match-string 0 property)))
                     (property-value (cdr entry)))
                (if (and org-vcard-remove-external-semicolons
                         (member property-name org-vcard-compound-properties))
                    ;; Remove leading and trailing semicolons from value of property.
                    (setq property-value (replace-regexp-in-string "^[;]+\\|[;]+$" "" property-value)))
                (if (car (rassoc property flat-style-properties))              
                    (insert (concat ":"
                                    (car (rassoc property flat-style-properties))
                                    ": "
                                    property-value
                                    "\n"))
                  (if org-vcard-include-import-unknowns
                      (insert (concat ":"
                                      property
                                      ": "
                                      property-value
                                      "\n")))))))
        (insert ":END:\n")))
    (if (string= "file" destination)
        (write-file filename))
    (cond
     ((string= "buffer" destination)
      (message (concat "Imported contacts data to buffer " (buffer-name import-buffer) ".")))
     ((string= "file" destination)
      (message "Imported contacts data to file.")))))
