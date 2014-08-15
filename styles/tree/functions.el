(defun org-vcard-export-from-tree (source destination)
  "Export tree-style SOURCE to vCard format, sending output
to DESTINATION.

SOURCE must be \"buffer\", \"region\" or \"subtree\".
DESTINATION must be either \"buffer\" or \"file\"."
  (let* ((in-contact-entry nil)
         (tree-style-properties
          (or (cadr (assoc org-vcard-active-version
                           (cadr (assoc org-vcard-active-language
                                        (cadr (assoc "tree" org-vcard-styles-languages-mappings))))))
              (error "No mapping available for specified vCard version")))
         (encoding (cond
                    ((string= "4.0" org-vcard-active-version) 'utf-8)
                    ((string= "3.0" org-vcard-active-version) 'utf-8)
                    ((string= "2.1" org-vcard-active-version) 'us-ascii)))
         (output (encode-coding-string "" encoding)))
    (if (not (member source '("buffer" "region" "subtree")))
        (error "Invalid source type"))
    (save-excursion
      (let ((search-result nil))
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
                    (let ((property (cdr (assoc (downcase fieldtype) tree-style-properties))))
                      (save-excursion
                        (forward-line)
                        (beginning-of-line)
                        (if (looking-at "\\s *:PREFERRED:")
                            (cond
                             ((string= "4.0" org-vcard-active-version)
                              (setq property (concat property ";PREF=1")))
                             ((string= "3.0" org-vcard-active-version)
                              (if (string-match "TYPE=" property)
                                  (setq property (concat property ",pref"))
                                (setq property (concat property ";TYPE=pref"))))
                             ((string= "2.1" org-vcard-active-version)
                              (setq property (concat property ";PREF"))))))                             
                      (setq content (concat content
                                            (org-vcard-export-line
                                             property
                                             (org-get-heading t t)))))
                  (setq end-vcard t))))
            (setq content (concat content
                                  (org-vcard-export-line "END:VCARD" "" t)))
            (setq output (concat output content)))
          (if search-result
              (re-search-backward "\\s *:FIELDTYPE:\\s *name")))
        (org-vcard-write-to-destination output destination)
        (cond
         ((string= "buffer" destination)
          (message "Exported contacts data to *org-vcard-export* buffer."))
         ((string= "file" destination)
          (message "Exported contacts data to file.")))))))


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
    (let ((tree-style-properties
          (or (cadr (assoc org-vcard-active-version
                           (cadr (assoc org-vcard-active-language
                                        (cadr (assoc "tree" org-vcard-styles-languages-mappings)))))))))
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
          (let ((property-original property)
                    (case-fold-search t)
                    (preferred nil))
            (if (not (member property '("FN" "KIND" "VERSION")))
                (progn
                  (cond
                   ((or (string= "4.0" org-vcard-active-version)
                       (string= "2.1" org-vcard-active-version))
                    (setq property (replace-regexp-in-string ";PREF\\(?:=\\w+\\)?" "" property)))
                   ((string= "3.0" org-vcard-active-version)
                    (progn
                      (setq property (replace-regexp-in-string ",?pref" "" property))
                      (if (string-match ";TYPE=\\(?:;\\|$\\)" property)
                          (setq property (replace-regexp-in-string ";TYPE=" "" property))))))
                  (if (not (string= property-original property))
                      ;; Contents of 'property' were changed by replace-regexp-in-string,
                      ;; so it must have contained a 'PREF'.
                      (setq preferred t))
                  (insert (concat "** " (cdr (assoc property-original card)) "\n"))
                  (insert (concat ":PROPERTIES:\n"
                                  ":FIELDTYPE: "
                                  (car (rassoc property tree-style-properties))
                                  "\n"
                                  (if preferred
                                      ":PREFERRED:\n")
                                  ":END:\n"))))
            (setq card (delq (assoc property-original card) card)))))))
  (cond
     ((string= "buffer" destination)
      (message "Imported contacts data to *org-vcard-import* buffer."))
     ((string= "file" destination)
      (message "Imported contacts data to file."))))
