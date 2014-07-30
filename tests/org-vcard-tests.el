
;; Check if testing-related variables have been set, error if not.
;;
;; * org-vcard-elisp :
;;   string containing absolute path to directory containing org-vcard.el.
;; * org-vcard-tests-data-dir:
;;   string containing absolute path to the org-vcard tests/data directory.


(let ((status 0)
      (err-msg ""))
  (if (or (not (boundp 'org-vcard-elisp))
         (string= "" org-vcard-elisp))
      (setq status (+ 1 status)))
  (if (or (not (boundp 'org-vcard-tests-data-dir))
         (string= "" org-vcard-tests-data-dir))
      (setq status (+ 2 status)))
  (cond
   ((= 1 status)
    (setq err-msg "org-vcard-elisp not set"))
   ((= 2 status)
    (setq err-msg "org-vcard-test-data-dir not set"))
   ((= 3 status)
    (setq err-msg "org-vcard-elisp not set, org-vcard-test-data-dir not set")))
  (if (not (string= "" err-msg))
      (error (concat "Loading org-vcard tests failed: " err-msg))))

(load (concat org-vcard-elisp "org-vcard.el"))


;;;;
;;;; Tests for utility functions.
;;;;


(ert-deftest org-vcard-test-check-contacts-styles ()
  "Test the org-vcard-check-contacts-styles function."
  :tags '(org-vcard)

  (should-error (let ((org-vcard-contacts-styles
                       '(("flat" .
                          (nil org-vcard-import-to-flat))
                         ("tree" .
                          (org-vcard-export-from-tree org-vcard-import-to-tree)))))
                  (org-vcard-check-contacts-styles)))
  (should-error (let ((org-vcard-contacts-styles
                       '(("flat" .
                          (org-vcard-export-from-flat nil))
                         ("tree" .
                          (org-vcard-export-from-tree org-vcard-import-to-tree)))))
                  (org-vcard-check-contacts-styles)))
  (should-error (let ((org-vcard-contacts-styles
                       '(("flat" .
                          (org-vcard-export-from-flat org-vcard-import-to-flat))
                         ("tree" .
                          (nil org-vcard-import-to-tree)))))
                  (org-vcard-check-contacts-styles)))
  (should-error (let ((org-vcard-contacts-styles
                       '(("flat" .
                          (org-vcard-export-from-flat org-vcard-import-to-flat))
                         ("tree" .
                          (org-vcard-export-from-tree nil)))))
                  (org-vcard-check-contacts-styles))))


(ert-deftest org-vcard-test-escape-value-string ()
  "Test the org-vcard-escape-value-string function."
  :tags '(org-vcard)
  
  (should (string=
           "word"
           (org-vcard-escape-value-string '(":") "word")))
  (should (string=
           "word"
           (org-vcard-escape-value-string '(";") "word")))
  (should (string=
           "word"
           (org-vcard-escape-value-string '("\134") "word")))
  (should (string=
           "word\\:word"
           (org-vcard-escape-value-string '(":") "word:word")))
  (should (string=
           "word\\;word"
           (org-vcard-escape-value-string '(";") "word;word")))
  (should (string=
           "word\\\\word"
           (org-vcard-escape-value-string '("\134") "word\\word")))
  (should (string=
           "word\\\\word\\;word\\:word"
           (org-vcard-escape-value-string '("\134" ":" ";") "word\\word;word:word"))))


(ert-deftest org-vcard-test-export-line ()
  "Test the org-vcard-export-line function."
  :tags '(org-vcard)
  
  ;; Tests for when NOSEPARATOR is absent or nil.
  
  (should (let ((org-vcard-active-version "4.0"))
            (string=
             "FN:word\u000D\u000A"
             (org-vcard-export-line "FN" "word"))))
  (should (let ((org-vcard-active-version "3.0"))
            (string=
             "FN:word\015\012"
             (org-vcard-export-line "FN" "word"))))
  (should (let ((org-vcard-active-version "2.1"))
            (string=
             "FN:word\015\012"
             (org-vcard-export-line "FN" "word"))))
  (should (let ((org-vcard-active-version "4.0"))
            (string=
             "FN:word word\u000D\u000A"
             (org-vcard-export-line "FN" "word word"))))
  (should (let ((org-vcard-active-version "3.0"))
            (string=
             "FN:word word\015\012"
             (org-vcard-export-line "FN" "word word"))))
  (should (let ((org-vcard-active-version "2.1"))
            (string=
             "FN:word word\015\012"
             (org-vcard-export-line "FN" "word word"))))

  ;; Tests for when NOSEPARATOR is non-nil.
  
  (should (let ((org-vcard-active-version "4.0"))
            (string=
             "FNword\u000D\u000A"
             (org-vcard-export-line "FN" "word" t))))
  (should (let ((org-vcard-active-version "3.0"))
            (string=
             "FNword\015\012"
             (org-vcard-export-line "FN" "word" t))))
  (should (let ((org-vcard-active-version "2.1"))
            (string=
             "FNword\015\012"
             (org-vcard-export-line "FN" "word" t)))))


(ert-deftest org-vcard-test-set-active-settings ()
  "Test the org-vcard-set-active-settings function."
  :tags '(org-vcard)

  ;; Tests for CONTACTS_STYLE in-buffer setting.
  
  (should (with-temp-buffer
            (insert "#+CONTACTS_STYLE: flat\n")
            (org-vcard-set-active-settings)
            (string= "flat" org-vcard-active-style)))
  (should (with-temp-buffer
            (insert "#+STARTUP: overview\n")
            (insert "#+CONTACTS_STYLE: flat\n")
            (insert "#+CONSTANTS: a=1\n")
            (org-vcard-set-active-settings)
            (string= "flat" org-vcard-active-style)))
  (should (with-temp-buffer
            (insert "#+STARTUP: overview\n")
            (insert "#+CONTACTS_STYLE: flat\n")
            (insert "#+CONSTANTS: a=1\n")
            (insert "#+CONTACTS_STYLE: tree\n")
            (org-vcard-set-active-settings)
            (string= "tree" org-vcard-active-style)))
  (should (with-temp-buffer
            (insert "#+STARTUP: overview\n")
            (insert "#+CONTACTS_STYLE: flat\n")
            (insert "#+CONSTANTS: a=1\n")
            (insert "#+CONTACTS_STYLE: tree\n")
            (insert "#+CONSTANTS: b=2\n")
            (org-vcard-set-active-settings)
            (string= "tree" org-vcard-active-style)))
  (should (with-temp-buffer
            (insert "#+CONTACTS_STYLE: flat\n")
            (insert "#+VCARD_VERSION: 4.0\n")
            (org-vcard-set-active-settings)
            (string= "flat" org-vcard-active-style)))
  (should (with-temp-buffer
            (insert "#+VCARD_VERSION: 4.0\n")
            (insert "#+CONTACTS_STYLE: flat\n")
            (org-vcard-set-active-settings)
            (string= "flat" org-vcard-active-style)))

  (should-error (with-temp-buffer
                  (insert "#+CONTACTS_STYLE: syzygy\n")
                  (org-vcard-set-active-settings)))

  ;; Tests for VCARD_VERSION in-buffer setting.

  (should (with-temp-buffer
            (insert "#+VCARD_VERSION: 4.0\n")
            (org-vcard-set-active-settings)
            (string= "4.0" org-vcard-active-version)))
  (should (with-temp-buffer
            (insert "#+STARTUP: overview\n")
            (insert "#+VCARD_VERSION: 4.0\n")
            (insert "#+CONSTANTS: a=1\n")
            (org-vcard-set-active-settings)
            (string= "4.0" org-vcard-active-version)))
  (should (with-temp-buffer
            (insert "#+STARTUP: overview\n")
            (insert "#+VCARD_VERSION: 4.0\n")
            (insert "#+CONSTANTS: a=1\n")
            (insert "#+VCARD_VERSION: 3.0\n")
            (org-vcard-set-active-settings)
            (string= "3.0" org-vcard-active-version)))
  (should (with-temp-buffer
            (insert "#+STARTUP: overview\n")
            (insert "#+VCARD_VERSION: 4.0\n")
            (insert "#+CONSTANTS: a=1\n")
            (insert "#+VCARD_VERSION: 3.0\n")
            (insert "#+CONSTANTS: b=2\n")
            (org-vcard-set-active-settings)
            (string= "3.0" org-vcard-active-version)))
  (should (with-temp-buffer
            (insert "#+VCARD_VERSION: 4.0\n")
            (insert "#+CONTACTS_STYLE: flat\n")
            (org-vcard-set-active-settings)
            (string= "4.0" org-vcard-active-version)))
  (should (with-temp-buffer
            (insert "#+CONTACTS_STYLE: flat\n")
            (insert "#+VCARD_VERSION: 4.0\n")
            (org-vcard-set-active-settings)
            (string= "4.0" org-vcard-active-version)))

  (should-error (with-temp-buffer
                  (insert "#+VCARD_VERSION: 1.0\n")
                  (org-vcard-set-active-settings))))


(ert-deftest org-vcard-test-import-parser ()
  "Test the org-vcard-test-import-parser function."
  :tags '(org-vcard)

  (should (with-temp-buffer
            (insert "BEGIN:VCARD\u000D\u000A")
            (insert "FN:word\u000D\u000A")
            (insert "END:VCARD\u000D\u000A")
            (equal '((("FN" . "word")))
                   (org-vcard-import-parser "buffer"))))
  (should (with-temp-buffer
            (insert "BEGIN:VCARD\015\012")
            (insert "FN:word\015\012")
            (insert "END:VCARD\015\012")
            (equal '((("FN" . "word")))
                   (org-vcard-import-parser "buffer"))))
  (should (with-temp-buffer
            (insert "BEGIN:VCARD\u000D\u000A")
            (insert "FN:word\u000D\u000A")
            (insert "N:\u000D\u000A")
            (insert "END:VCARD\u000D\u000A")
            (equal '((("FN" . "word")("N" . "")))
                   (org-vcard-import-parser "buffer"))))
  (should (with-temp-buffer
            (insert "BEGIN:VCARD\015\012")
            (insert "FN:word\015\012")
            (insert "N:\015\012")
            (insert "END:VCARD\015\012")
            (equal '((("FN" . "word")("N" . "")))
                   (org-vcard-import-parser "buffer"))))
  (should (with-temp-buffer
            (insert "BEGIN:vcard\u000D\u000A")
            (insert "FN:word\u000D\u000A")
            (insert "END:VCARD\u000D\u000A")
            (equal '((("FN" . "word")))
                   (org-vcard-import-parser "buffer")))))


(ert-deftest org-vcard-test-write-to-destination ()
  "Test the org-vcard-write-to-destination-function."
  :tags '(org-vcard)

  (progn
    (should (string= (concat "A line of text\u000D\u000A"
                           "A second line of text\015\012"
                           "A third line of text\u000D\012")
                   (progn
                     (if (get-buffer "*org-vcard-export*")
                         (kill-buffer "*org-vcard-export*"))
                     (org-vcard-write-to-destination
                      (concat "A line of text\u000D\u000A"
                              "A second line of text\015\012"
                              "A third line of text\u000D\012")
                      "buffer")
                     (switch-to-buffer "*org-vcard-export*")
                     (buffer-string))))
    (kill-buffer "*org-vcard-export*"))

  (should-error (org-vcard-write-to-destination
                 '("A line of text\u000D\u000A"
                   "A second line of text\015\012"
                   "A third line of text\u000D\012")
                 "buffer")))


;;;;
;;;; Tests for style 'flat'
;;;;


(ert-deftest org-vcard-test-export-from-flat-vcard-40 ()
  "Test the org-vcard-export-from-flat function with vCard 4.0."
  :tags '(org-vcard)
  
  (let ((crlf "\u000D\u000A")
        (org-vcard-active-version "4.0"))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:4.0" crlf
                         "FN:Joan Smith" crlf
                         "TEL;TYPE=voice:00 9999 9999" crlf
                         "TEL;TYPE=cell:0000 999 999" crlf
                         "EMAIL:joan@example.com" crlf
                         "EMAIL:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:4.0" crlf
                         "FN:John" crlf
                         "TEL;TYPE=voice:01 9999 9999" crlf
                         "TEL;TYPE=cell:0001 999 999" crlf
                         "EMAIL;TYPE=work:john@example.com" crlf
                         "EMAIL;TYPE=home:john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (switch-to-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "flat-contacts-4_0.org"))
                   (org-mode)
                   (org-vcard-export-from-flat "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-export*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-export*")
            (kill-buffer "*org-vcard-export*"))))))


(ert-deftest org-vcard-test-export-from-flat-vcard-30 ()
  "Test the org-vcard-export-from-flat function with vCard 3.0."
  :tags '(org-vcard)

  (let ((crlf "\015\012")
        (org-vcard-active-version "3.0"))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:3.0" crlf
                         "FN:Joan Smith" crlf
                         "N:" crlf
                         "TEL;TYPE=voice:00 9999 9999" crlf
                         "TEL;TYPE=cell:0000 999 999" crlf
                         "EMAIL:joan@example.com" crlf
                         "EMAIL:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:3.0" crlf
                         "FN:John" crlf
                         "N:" crlf
                         "TEL;TYPE=voice:01 9999 9999" crlf
                         "TEL;TYPE=cell:0001 999 999" crlf
                         "EMAIL;TYPE=work:john@example.com" crlf
                         "EMAIL;TYPE=home:john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (switch-to-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "flat-contacts-3_0.org"))
                   (org-mode)
                   (org-vcard-export-from-flat "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-export*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-export*")
            (kill-buffer "*org-vcard-export*"))))))


(ert-deftest org-vcard-test-export-from-flat-vcard-21 ()
  "Test the org-vcard-export-from-flat function with vCard 2.1."
  :tags '(org-vcard)

  (let ((crlf "\015\012")
        (org-vcard-active-version "2.1"))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:2.1" crlf
                         "FN:Joan Smith" crlf
                         "N:" crlf
                         "TEL;VOICE:00 9999 9999" crlf
                         "TEL;CELL:0000 999 999" crlf
                         "EMAIL:joan@example.com" crlf
                         "EMAIL:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:2.1" crlf
                         "FN:John" crlf
                         "N:" crlf
                         "TEL;VOICE:01 9999 9999" crlf
                         "TEL;CELL:0001 999 999" crlf
                         "EMAIL;WORK:john@example.com" crlf
                         "EMAIL;HOME:john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (switch-to-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "flat-contacts-2_1.org"))
                   (org-mode)
                   (org-vcard-export-from-flat "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-export*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-export*")
            (kill-buffer "*org-vcard-export*"))))))


(ert-deftest org-vcard-test-import-to-flat-vcard-40 ()
  "Test the org-vcard-import-to-flat function with vCard 4.0."
  :tags '(org-vcard)
  
  (let ((lf "\n")
        (org-vcard-active-version "4.0"))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 4.0" lf
                         ":EMAIL: joan@example.com" lf
                         ":EMAIL: joan.2@example.com" lf
                         ":CELL: 0000 999 999" lf
                         ":LANDLINE: 00 9999 9999" lf
                         ":END:" lf
                         "* John" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 4.0" lf
                         ":EMAIL_WORK: john@example.com" lf
                         ":EMAIL_HOME: john.2@example.com" lf
                         ":CELL: 0001 999 999" lf
                         ":LANDLINE: 01 9999 9999" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "vcard-source-4_0.vcf"))
                   (org-vcard-import-to-flat "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-import*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-import*")
            (kill-buffer "*org-vcard-import*"))))))


(ert-deftest org-vcard-test-import-to-flat-vcard-30 ()
  "Test the org-vcard-import-to-flat function with vCard 3.0."
  :tags '(org-vcard)
  
  (let ((lf "\n")
        (org-vcard-active-version "3.0"))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 3.0" lf
                         ":EMAIL: joan@example.com" lf
                         ":EMAIL: joan.2@example.com" lf
                         ":CELL: 0000 999 999" lf
                         ":LANDLINE: 00 9999 9999" lf
                         ":END:" lf
                         "* John" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 3.0" lf
                         ":EMAIL_WORK: john@example.com" lf
                         ":EMAIL_HOME: john.2@example.com" lf
                         ":CELL: 0001 999 999" lf
                         ":LANDLINE: 01 9999 9999" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "vcard-source-3_0.vcf"))
                   (org-vcard-import-to-flat "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-import*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-import*")
            (kill-buffer "*org-vcard-import*"))))))


(ert-deftest org-vcard-test-import-to-flat-vcard-21 ()
  "Test the org-vcard-import-to-flat function with vCard 2.1."
  :tags '(org-vcard)
  
  (let ((lf "\n")
        (org-vcard-active-version "2.1"))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 2.1" lf
                         ":EMAIL: joan@example.com" lf
                         ":EMAIL: joan.2@example.com" lf
                         ":CELL: 0000 999 999" lf
                         ":LANDLINE: 00 9999 9999" lf
                         ":END:" lf
                         "* John" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 2.1" lf
                         ":EMAIL_WORK: john@example.com" lf
                         ":EMAIL_HOME: john.2@example.com" lf
                         ":CELL: 0001 999 999" lf
                         ":LANDLINE: 01 9999 9999" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "vcard-source-2_1.vcf"))
                   (org-vcard-import-to-flat "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-import*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-import*")
            (kill-buffer "*org-vcard-import*"))))))


;;;;
;;;; Tests for style 'tree'
;;;;


(ert-deftest org-vcard-test-export-from-tree-vcard-40 ()
  "Test the org-vcard-export-from-tree function with vCard 4.0."
  :tags '(org-vcard)
  
  (let ((crlf "\u000D\u000A")
        (org-vcard-active-version "4.0"))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:4.0" crlf
                         "FN:Joan Smith" crlf
                         "TEL;TYPE=voice:00 9999 9999" crlf
                         "TEL;TYPE=cell:0000 999 999" crlf
                         "EMAIL:joan@example.com" crlf
                         "EMAIL:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:4.0" crlf
                         "FN:John" crlf
                         "TEL;TYPE=voice:01 9999 9999" crlf
                         "TEL;TYPE=cell:0001 999 999" crlf
                         "EMAIL;TYPE=work:john@example.com" crlf
                         "EMAIL;TYPE=home:john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "tree-contacts-4_0.org"))
                   (org-mode)
                   (org-vcard-export-from-tree "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-export*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-export*")
            (kill-buffer "*org-vcard-export*"))))))


(ert-deftest org-vcard-test-export-from-tree-vcard-30 ()
  "Test the org-vcard-export-from-tree function with vCard 3.0."
  :tags '(org-vcard)
  
  (let ((crlf "\015\012")
        (org-vcard-active-version "3.0"))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:3.0" crlf
                         "FN:Joan Smith" crlf
                         "N:" crlf
                         "TEL;TYPE=voice:00 9999 9999" crlf
                         "TEL;TYPE=cell:0000 999 999" crlf
                         "EMAIL:joan@example.com" crlf
                         "EMAIL:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:3.0" crlf
                         "FN:John" crlf
                         "N:" crlf
                         "TEL;TYPE=voice:01 9999 9999" crlf
                         "TEL;TYPE=cell:0001 999 999" crlf
                         "EMAIL;TYPE=work:john@example.com" crlf
                         "EMAIL;TYPE=home:john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "tree-contacts-3_0.org"))
                   (org-mode)
                   (org-vcard-export-from-tree "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-export*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-export*")
            (kill-buffer "*org-vcard-export*"))))))


(ert-deftest org-vcard-test-export-from-tree-vcard-21 ()
  "Test the org-vcard-export-from-tree function with vCard 2.1."
  :tags '(org-vcard)

  (let ((crlf "\015\012")
        (org-vcard-active-version "2.1"))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:2.1" crlf
                         "FN:Joan Smith" crlf
                         "N:" crlf
                         "TEL;VOICE:00 9999 9999" crlf
                         "TEL;CELL:0000 999 999" crlf
                         "EMAIL:joan@example.com" crlf
                         "EMAIL:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:2.1" crlf
                         "FN:John" crlf
                         "N:" crlf
                         "TEL;VOICE:01 9999 9999" crlf
                         "TEL;CELL:0001 999 999" crlf
                         "EMAIL;WORK:john@example.com" crlf
                         "EMAIL;HOME:john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "tree-contacts-2_1.org"))
                   (org-mode)
                   (org-vcard-export-from-tree "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-export*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-export*")
            (kill-buffer "*org-vcard-export*"))))))


(ert-deftest org-vcard-test-import-to-tree-vcard-40 ()
  "Test the org-vcard-import-to-tree function with vCard 4.0."
  :tags '(org-vcard)
  
  (let ((lf "\n")
        (org-vcard-active-version "4.0"))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":KIND: individual" lf
                         ":FIELDTYPE: name" lf
                         ":END:" lf
                         "** joan@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":END:" lf
                         "** joan.2@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":END:" lf
                         "** 0000 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 00 9999 9999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: landline" lf
                         ":END:" lf
                         "* John" lf
                         ":PROPERTIES:" lf
                         ":KIND: individual" lf
                         ":FIELDTYPE: name" lf
                         ":END:" lf
                         "** john.2@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email-home" lf
                         ":END:" lf
                         "** john@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email-work" lf
                         ":END:" lf
                         "** 0001 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 01 9999 9999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: landline" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "vcard-source-4_0.vcf"))
                   (org-vcard-import-to-tree "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-import*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-import*")
            (kill-buffer "*org-vcard-import*"))))))


(ert-deftest org-vcard-test-import-to-tree-vcard-30 ()
  "Test the org-vcard-import-to-tree function with vCard 3.0."
  :tags '(org-vcard)
  
  (let ((lf "\n")
        (org-vcard-active-version "3.0"))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":KIND: individual" lf
                         ":FIELDTYPE: name" lf
                         ":END:" lf
                         "** joan@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":END:" lf
                         "** joan.2@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":END:" lf
                         "** 0000 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 00 9999 9999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: landline" lf
                         ":END:" lf
                         "* John" lf
                         ":PROPERTIES:" lf
                         ":KIND: individual" lf
                         ":FIELDTYPE: name" lf
                         ":END:" lf
                         "** john.2@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email-home" lf
                         ":END:" lf
                         "** john@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email-work" lf
                         ":END:" lf
                         "** 0001 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 01 9999 9999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: landline" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "vcard-source-3_0.vcf"))
                   (org-vcard-import-to-tree "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-import*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-import*")
            (kill-buffer "*org-vcard-import*"))))))


(ert-deftest org-vcard-test-import-to-tree-vcard-21 ()
  "Test the org-vcard-import-to-tree function with vCard 2.1."
  :tags '(org-vcard)
  
  (let ((lf "\n")
        (org-vcard-active-version "2.1"))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":KIND: individual" lf
                         ":FIELDTYPE: name" lf
                         ":END:" lf
                         "** joan@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":END:" lf
                         "** joan.2@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":END:" lf
                         "** 0000 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 00 9999 9999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: landline" lf
                         ":END:" lf
                         "* John" lf
                         ":PROPERTIES:" lf
                         ":KIND: individual" lf
                         ":FIELDTYPE: name" lf
                         ":END:" lf
                         "** john.2@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email-home" lf
                         ":END:" lf
                         "** john@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email-work" lf
                         ":END:" lf
                         "** 0001 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 01 9999 9999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: landline" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "vcard-source-2_1.vcf"))
                   (org-vcard-import-to-tree "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-import*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-import*")
            (kill-buffer "*org-vcard-import*"))))))


;;; org-vcard-tests.el ends here.
