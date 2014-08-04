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
                    (setq content (concat content
                                          (org-vcard-export-line
                                           (cdr (assoc (downcase fieldtype) tree-style-properties))
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
          (if (not (member property '("FN" "KIND" "VERSION")))
              (progn
                (insert (concat "** " (cdr (assoc property card)) "\n"))
                (insert (concat ":PROPERTIES:\n"
                                ":FIELDTYPE: "
                                (car (rassoc property tree-style-properties))
                                "\n"
                                ":END:\n"))))
          (setq card (delq (assoc property card) card)))))))
