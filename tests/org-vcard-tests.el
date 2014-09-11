
(if (not (boundp 'org-vcard-elisp-dir))
    (error "Variable org-vcard-elisp-dir not set; have you loaded org-vcard.el?"))

(defconst org-vcard-tests-data-dir (file-name-as-directory
                                    (concat org-vcard-elisp-dir "tests/data")))


;;;;
;;;; Tests for utility functions.
;;;;


(ert-deftest org-vcard-test-check-contacts-styles ()
  "Test the org-vcard-check-contacts-styles function."
  :tags '(org-vcard)

  (should-error (let ((org-vcard-styles-functions
                       '(("flat"
                          (nil org-vcard-import-to-flat))
                         ("tree"
                          (org-vcard-export-from-tree org-vcard-import-to-tree)))))
                  (org-vcard-check-contacts-styles)))
  (should-error (let ((org-vcard-styles-functions
                       '(("flat"
                          (org-vcard-export-from-flat nil))
                         ("tree"
                          (org-vcard-export-from-tree org-vcard-import-to-tree)))))
                  (org-vcard-check-contacts-styles)))
  (should-error (let ((org-vcard-styles-functions
                       '(("flat"
                          (org-vcard-export-from-flat org-vcard-import-to-flat))
                         ("tree"
                          (nil org-vcard-import-to-tree)))))
                  (org-vcard-check-contacts-styles)))
  (should-error (let ((org-vcard-styles-functions
                       '(("flat"
                          (org-vcard-export-from-flat org-vcard-import-to-flat))
                         ("tree"
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
             "FN;CHARSET=US-ASCII:word\015\012"
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
             "FN;CHARSET=US-ASCII:word word\015\012"
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
             "FN;CHARSET=US-ASCIIword\015\012"
             (org-vcard-export-line "FN" "word" t)))))


(ert-deftest org-vcard-test-set-active-settings-style ()
  "Test the org-vcard-set-active-settings function with
the CONTACTS_STYLE in-buffer setting."
  :tags '(org-vcard)
  
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
                  (org-vcard-set-active-settings))))


(ert-deftest org-vcard-test-set-active-settings-language ()
  "Test the org-vcard-set-active-settings function with
the CONTACTS_LANGUAGE in-buffer setting."
  :tags '(org-vcard)
  
  (should (with-temp-buffer
            (insert "#+CONTACTS_LANGUAGE: en\n")
            (org-vcard-set-active-settings)
            (string= "en" org-vcard-active-language)))
  (should (with-temp-buffer
            (insert "#+STARTUP: overview\n")
            (insert "#+CONTACTS_LANGUAGE: en\n")
            (insert "#+CONSTANTS: a=1\n")
            (org-vcard-set-active-settings)
            (string= "en" org-vcard-active-language)))
  (should (with-temp-buffer
            (insert "#+STARTUP: overview\n")
            (insert "#+CONTACTS_LANGUAGE: en\n")
            (insert "#+CONSTANTS: a=1\n")
            (insert "#+CONTACTS_LANGUAGE: en_AU\n")
            (org-vcard-set-active-settings)
            (string= "en_AU" org-vcard-active-language)))
  (should (with-temp-buffer
            (insert "#+STARTUP: overview\n")
            (insert "#+CONTACTS_LANGUAGE: en\n")
            (insert "#+CONSTANTS: a=1\n")
            (insert "#+CONTACTS_LANGUAGE: en_AU\n")
            (insert "#+CONSTANTS: b=2\n")
            (org-vcard-set-active-settings)
            (string= "en_AU" org-vcard-active-language)))
  (should (with-temp-buffer
            (insert "#+CONTACTS_LANGUAGE: en\n")
            (insert "#+VCARD_VERSION: 4.0\n")
            (org-vcard-set-active-settings)
            (string= "en" org-vcard-active-language)))
  (should (with-temp-buffer
            (insert "#+VCARD_VERSION: 4.0\n")
            (insert "#+CONTACTS_LANGUAGE: en\n")
            (org-vcard-set-active-settings)
            (string= "en" org-vcard-active-language)))

  (should-error (with-temp-buffer
                  (insert "#+CONTACTS_LANGUAGE: syzygy\n")
                  (org-vcard-set-active-settings))))


(ert-deftest org-vcard-test-set-active-settings-version ()
  "Test the org-vcard-set-active-settings function with
the VCARD_VERSION in-buffer setting."
  :tags '(org-vcard)

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


(ert-deftest org-vcard-test-canonicalise-property-name-vcard-40 ()
  "Test the org-vcard-canonicalise-property-name function with
vCard 4.0."
  :tags '(org-vcard)

  (let ((org-vcard-active-version "4.0"))
    
    (should (string= "TEL"
                     (org-vcard-canonicalise-property-name "TEL")))

    (should (string= "TEL;TYPE=\"cell\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"CELL\"")))
    (should (string= "TEL;TYPE=\"cell\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"VOICE,CELL\"")))
    (should (string= "TEL;TYPE=\"cell,home\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"CELL,HOME\"")))
    (should (string= "TEL;TYPE=\"cell,work\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"CELL,WORK\"")))
    (should (string= "TEL;TYPE=\"cell,home\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"VOICE,CELL,HOME\"")))
    (should (string= "TEL;TYPE=\"cell,work\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"voice,CELL,WORK\"")))
    (should (string= "TEL;TYPE=\"cell,home\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"HOME,CELL\"")))
    (should (string= "TEL;TYPE=\"cell,work\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"WORK,CELL\"")))
    (should (string= "TEL;TYPE=\"cell,home\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"home,VOICE,CELL\"")))
    (should (string= "TEL;TYPE=\"cell,work\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"WORK,voice,CELL\"")))

    (should (string= "TEL;TYPE=\"fax\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"FAX\"")))
    (should (string= "TEL;TYPE=\"fax\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"fax\"")))
    (should (string= "TEL;TYPE=\"fax,home\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"FAX,HOME\"")))
    (should (string= "TEL;TYPE=\"fax,work\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"work,fax\"")))

    (should (string= "TEL;TYPE=\"voice\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"VOICE\"")))
    (should (string= "TEL;TYPE=\"voice,home\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"HOME\"")))
    (should (string= "TEL;TYPE=\"voice,work\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"WORK\"")))
    (should (string= "TEL;TYPE=\"voice,home\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"voice,HOME\"")))
    (should (string= "TEL;TYPE=\"voice,work\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"WORK,voice\"")))

    (should (string= "EMAIL"
                     (org-vcard-canonicalise-property-name "EMAIL")))
    (should (string= "EMAIL;TYPE=\"work\""
                     (org-vcard-canonicalise-property-name "EMAIL;TYPE=work")))
    (should (string= "EMAIL;TYPE=\"home\""
                     (org-vcard-canonicalise-property-name "EMAIL;TYPE=\"home\"")))
    (should (string= "EMAIL;PREF=1"
                     (org-vcard-canonicalise-property-name "EMAIL;PREF=1")))
    (should (string= "EMAIL;TYPE=\"home\";PREF=1"
                     (org-vcard-canonicalise-property-name "EMAIL;PREF=1;TYPE=\"home\"")))

    (should (string= "TEL;TYPE=\"PAGER\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"PAGER\"")))
    (should (string= "TEL;TYPE=\"pager\""
                     (org-vcard-canonicalise-property-name "TEL;TYPE=\"pager\"")))

    (should (string= "ADR"
                     (org-vcard-canonicalise-property-name "ADR")))
    (should (string= "ADR;TYPE=\"work\""
                     (org-vcard-canonicalise-property-name "ADR;TYPE=WORK")))
    (should (string= "ADR;TYPE=\"home\""
                     (org-vcard-canonicalise-property-name "ADR;TYPE=\"home\"")))))

  
(ert-deftest org-vcard-test-canonicalise-property-name-vcard-30 ()
  "Test the org-vcard-canonicalise-property-name function with
vCard 3.0."
  :tags '(org-vcard)
  
  (let ((org-vcard-active-version "3.0"))
    
    (should (string= "TEL"
                     (org-vcard-canonicalise-property-name "TEL")))

    (should (string= "TEL;TYPE=cell"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=CELL")))
    (should (string= "TEL;TYPE=cell"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=VOICE,CELL")))
    (should (string= "TEL;TYPE=cell,home"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=CELL,HOME")))
    (should (string= "TEL;TYPE=cell,work"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=CELL,WORK")))
    (should (string= "TEL;TYPE=cell,home"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=VOICE,CELL,HOME")))
    (should (string= "TEL;TYPE=cell,work"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=voice,CELL,WORK")))
    (should (string= "TEL;TYPE=cell,home"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=HOME,CELL")))
    (should (string= "TEL;TYPE=cell,work"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=WORK,CELL")))
    (should (string= "TEL;TYPE=cell,home"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=home,VOICE,CELL")))
    (should (string= "TEL;TYPE=cell,work"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=WORK,voice,CELL")))

    (should (string= "TEL;TYPE=fax"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=fax")))
    (should (string= "TEL;TYPE=fax"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=FAX")))
    (should (string= "TEL;TYPE=fax,home"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=FAX,HOME")))
    (should (string= "TEL;TYPE=fax,work"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=work,fax")))
    
    (should (string= "TEL;TYPE=voice"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=VOICE")))
    (should (string= "TEL;TYPE=voice,home"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=HOME")))
    (should (string= "TEL;TYPE=voice,work"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=WORK")))
    (should (string= "TEL;TYPE=voice,home"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=voice,HOME")))
    (should (string= "TEL;TYPE=voice,work"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=WORK,voice")))

    (should (string= "EMAIL"
                     (org-vcard-canonicalise-property-name "EMAIL")))
    (should (string= "EMAIL;TYPE=work"
                     (org-vcard-canonicalise-property-name "EMAIL;TYPE=work")))
    (should (string= "EMAIL;TYPE=home"
                     (org-vcard-canonicalise-property-name "EMAIL;TYPE=home")))
    (should (string= "EMAIL;TYPE=pref"
                     (org-vcard-canonicalise-property-name "EMAIL;TYPE=PREF")))
    (should (string= "EMAIL;TYPE=home,pref"
                     (org-vcard-canonicalise-property-name "EMAIL;TYPE=pref,home")))

    (should (string= "TEL;TYPE=PAGER"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=PAGER")))
    (should (string= "TEL;TYPE=pager"
                     (org-vcard-canonicalise-property-name "TEL;TYPE=pager")))

    (should (string= "ADR"
                     (org-vcard-canonicalise-property-name "ADR")))
    (should (string= "ADR;TYPE=work"
                     (org-vcard-canonicalise-property-name "ADR;TYPE=WORK")))
    (should (string= "ADR;TYPE=home"
                     (org-vcard-canonicalise-property-name "ADR;TYPE=home")))))


(ert-deftest org-vcard-test-canonicalise-property-name-vcard-21 ()
  "Test the org-vcard-canonicalise-property-name function with
vCard 2.1."
  :tags '(org-vcard)

  (let ((org-vcard-active-version "2.1"))
    
    (should (string= "TEL"
                     (org-vcard-canonicalise-property-name "TEL")))

    (should (string= "TEL;CELL"
                     (org-vcard-canonicalise-property-name "TEL;CELL")))
    (should (string= "TEL;CELL"
                     (org-vcard-canonicalise-property-name "TEL;VOICE;CELL")))
    (should (string= "TEL;CELL;HOME"
                     (org-vcard-canonicalise-property-name "TEL;CELL;HOME")))
    (should (string= "TEL;CELL;WORK"
                     (org-vcard-canonicalise-property-name "TEL;CELL;WORK")))
    (should (string= "TEL;CELL;HOME"
                     (org-vcard-canonicalise-property-name "TEL;VOICE;CELL;HOME")))
    (should (string= "TEL;CELL;WORK"
                     (org-vcard-canonicalise-property-name "TEL;VOICE;CELL;WORK")))
    (should (string= "TEL;CELL;HOME"
                     (org-vcard-canonicalise-property-name "TEL;HOME;CELL")))
    (should (string= "TEL;CELL;WORK"
                     (org-vcard-canonicalise-property-name "TEL;WORK;CELL")))
    (should (string= "TEL;CELL;HOME"
                     (org-vcard-canonicalise-property-name "TEL;HOME;VOICE;CELL")))
    (should (string= "TEL;CELL;WORK"
                     (org-vcard-canonicalise-property-name "TEL;WORK;VOICE;CELL")))

    (should (string= "TEL;FAX"
                     (org-vcard-canonicalise-property-name "TEL;FAX")))
    (should (string= "TEL;FAX;HOME"
                     (org-vcard-canonicalise-property-name "TEL;FAX;HOME")))
    (should (string= "TEL;FAX;WORK"
                     (org-vcard-canonicalise-property-name "TEL;WORK;FAX")))
    
    (should (string= "TEL;VOICE"
                     (org-vcard-canonicalise-property-name "TEL;VOICE")))
    (should (string= "TEL;VOICE;HOME"
                     (org-vcard-canonicalise-property-name "TEL;HOME")))
    (should (string= "TEL;VOICE;WORK"
                     (org-vcard-canonicalise-property-name "TEL;WORK")))
    (should (string= "TEL;VOICE;HOME"
                     (org-vcard-canonicalise-property-name "TEL;HOME;VOICE")))
    (should (string= "TEL;VOICE;WORK"
                     (org-vcard-canonicalise-property-name "TEL;WORK;VOICE")))

    (should (string= "EMAIL"
                     (org-vcard-canonicalise-property-name "EMAIL")))
    (should (string= "EMAIL;HOME"
                     (org-vcard-canonicalise-property-name "EMAIL;HOME")))
    (should (string= "EMAIL;WORK"
                     (org-vcard-canonicalise-property-name "EMAIL;WORK")))
    (should (string= "EMAIL;PREF"
                     (org-vcard-canonicalise-property-name "EMAIL;PREF")))
    (should (string= "EMAIL;HOME;PREF"
                     (org-vcard-canonicalise-property-name "EMAIL;PREF;HOME")))

    (should (string= "TEL;PAGER"
                     (org-vcard-canonicalise-property-name "TEL;PAGER")))

    (should (string= "ADR"
                     (org-vcard-canonicalise-property-name "ADR")))
    (should (string= "ADR;HOME"
                     (org-vcard-canonicalise-property-name "ADR;home")))
    (should (string= "ADR;WORK"
                     (org-vcard-canonicalise-property-name "ADR;WORK")))))


(ert-deftest org-vcard-test-import-parser ()
  "Test the org-vcard-import-parser function."
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
        (org-vcard-active-language "en")
        (org-vcard-active-version "4.0")
        (org-vcard-remove-external-semicolons nil))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:4.0" crlf
                         "FN:Joan Smith" crlf
                         "TEL;TYPE=\"voice\":00 9999 9999" crlf
                         "TEL;TYPE=\"cell\":0000 999 999" crlf
                         "ADR;TYPE=\"work\":;123 Main Street;Any Town;;;" crlf
                         "EMAIL:joan@example.com" crlf
                         "EMAIL:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:4.0" crlf
                         "FN:John" crlf
                         "TEL;TYPE=\"voice\":01 9999 9999" crlf
                         "TEL;TYPE=\"cell\":0001 999 999" crlf
                         "EMAIL;TYPE=\"work\":john@example.com" crlf
                         "EMAIL;TYPE=\"home\":john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (switch-to-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "flat-contacts-4_0.org"))
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
        (org-vcard-active-language "en")
        (org-vcard-active-version "3.0")
        (org-vcard-remove-external-semicolons nil))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:3.0" crlf
                         "FN:Joan Smith" crlf
                         "N:" crlf
                         "TEL;TYPE=voice:00 9999 9999" crlf
                         "TEL;TYPE=cell:0000 999 999" crlf
                         "ADR;TYPE=work:;123 Main Street;Any Town;;;" crlf
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
        (org-vcard-active-language "en")
        (org-vcard-active-version "2.1")
        (org-vcard-remove-external-semicolons nil))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:2.1" crlf
                         "FN;CHARSET=US-ASCII:Joan Smith" crlf
                         "N;CHARSET=US-ASCII:" crlf
                         "TEL;VOICE;CHARSET=US-ASCII:00 9999 9999" crlf
                         "TEL;CELL;CHARSET=US-ASCII:0000 999 999" crlf
                         "ADR;WORK;CHARSET=US-ASCII:;123 Main Street;Any Town;;;" crlf
                         "EMAIL;CHARSET=US-ASCII:joan@example.com" crlf
                         "EMAIL;CHARSET=US-ASCII:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:2.1" crlf
                         "FN;CHARSET=US-ASCII:John" crlf
                         "N;CHARSET=US-ASCII:" crlf
                         "TEL;VOICE;CHARSET=US-ASCII:01 9999 9999" crlf
                         "TEL;CELL;CHARSET=US-ASCII:0001 999 999" crlf
                         "EMAIL;WORK;CHARSET=US-ASCII:john@example.com" crlf
                         "EMAIL;HOME;CHARSET=US-ASCII:john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (switch-to-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "flat-contacts-2_1.org"))
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
        (org-vcard-active-style "flat")
        (org-vcard-active-language "en")
        (org-vcard-active-version "4.0")
        (org-vcard-append-to-existing-import-buffer nil))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 4.0" lf
                         ":EMAIL: joan@example.com" lf
                         ":EMAIL: joan.2@example.com" lf
                         ":CELL: 0000 999 999" lf
                         ":LANDLINE: 00 9999 9999" lf
                         ":FAX: 00 9999 9998" lf
                         ":END:" lf
                         "* John" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 4.0" lf
                         ":EMAIL_WORK: john@example.com" lf
                         ":EMAIL_HOME: john.2@example.com" lf
                         ":CELL: 0001 999 999" lf
                         ":LANDLINE: 01 9999 9999" lf
                         ":FAX_HOME: 01 9999 9998" lf
                         ":FAX_WORK: 01 9999 9997" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "flat-vcard-source-4_0.vcf"))
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
        (org-vcard-active-style "flat")
        (org-vcard-active-language "en")
        (org-vcard-active-version "3.0")
        (org-vcard-append-to-existing-import-buffer nil))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 3.0" lf
                         ":EMAIL: joan@example.com" lf
                         ":EMAIL: joan.2@example.com" lf
                         ":CELL: 0000 999 999" lf
                         ":LANDLINE: 00 9999 9999" lf
                         ":FAX: 00 9999 9998" lf
                         ":END:" lf
                         "* John" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 3.0" lf
                         ":EMAIL_WORK: john@example.com" lf
                         ":EMAIL_HOME: john.2@example.com" lf
                         ":CELL: 0001 999 999" lf
                         ":LANDLINE: 01 9999 9999" lf
                         ":FAX_HOME: 01 9999 9998" lf
                         ":FAX_WORK: 01 9999 9997" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "flat-vcard-source-3_0.vcf"))
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
        (org-vcard-active-style "flat")
        (org-vcard-active-language "en")
        (org-vcard-active-version "2.1")
        (org-vcard-append-to-existing-import-buffer nil))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 2.1" lf
                         ":EMAIL: joan@example.com" lf
                         ":EMAIL: joan.2@example.com" lf
                         ":CELL: 0000 999 999" lf
                         ":LANDLINE: 00 9999 9999" lf
                         ":FAX: 00 9999 9998" lf
                         ":END:" lf
                         "* John" lf
                         ":PROPERTIES:" lf
                         ":VERSION: 2.1" lf
                         ":EMAIL_WORK: john@example.com" lf
                         ":EMAIL_HOME: john.2@example.com" lf
                         ":CELL: 0001 999 999" lf
                         ":LANDLINE: 01 9999 9999" lf
                         ":FAX_HOME: 01 9999 9998" lf
                         ":FAX_WORK: 01 9999 9997" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "flat-vcard-source-2_1.vcf"))
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
        (org-vcard-active-language "en")
        (org-vcard-active-version "4.0"))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:4.0" crlf
                         "FN:Joan Smith" crlf
                         "TEL;TYPE=\"voice\":00 9999 9999" crlf
                         "TEL;TYPE=\"cell\":0000 999 999" crlf
                         "EMAIL;PREF=1:joan@example.com" crlf
                         "EMAIL:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:4.0" crlf
                         "FN:John" crlf
                         "TEL;TYPE=\"voice\":01 9999 9999" crlf
                         "TEL;TYPE=\"cell\":0001 999 999" crlf
                         "EMAIL;TYPE=\"work\";PREF=1:john@example.com" crlf
                         "EMAIL;TYPE=\"home\":john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "tree-contacts-4_0.org"))
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
        (org-vcard-active-language "en")
        (org-vcard-active-version "3.0"))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:3.0" crlf
                         "FN:Joan Smith" crlf
                         "N:" crlf
                         "TEL;TYPE=voice:00 9999 9999" crlf
                         "TEL;TYPE=cell:0000 999 999" crlf
                         "EMAIL;TYPE=pref:joan@example.com" crlf
                         "EMAIL:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:3.0" crlf
                         "FN:John" crlf
                         "N:" crlf
                         "TEL;TYPE=voice:01 9999 9999" crlf
                         "TEL;TYPE=cell:0001 999 999" crlf
                         "EMAIL;TYPE=work,pref:john@example.com" crlf
                         "EMAIL;TYPE=home:john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "tree-contacts-3_0.org"))
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
        (org-vcard-active-language "en")
        (org-vcard-active-version "2.1"))

    (unwind-protect
        (should (string=
                 (concat "BEGIN:VCARD" crlf
                         "VERSION:2.1" crlf
                         "FN;CHARSET=US-ASCII:Joan Smith" crlf
                         "N;CHARSET=US-ASCII:" crlf
                         "TEL;VOICE;CHARSET=US-ASCII:00 9999 9999" crlf
                         "TEL;CELL;CHARSET=US-ASCII:0000 999 999" crlf
                         "EMAIL;PREF;CHARSET=US-ASCII:joan@example.com" crlf
                         "EMAIL;CHARSET=US-ASCII:joan.2@example.com" crlf
                         "END:VCARD" crlf
                         "BEGIN:VCARD" crlf
                         "VERSION:2.1" crlf
                         "FN;CHARSET=US-ASCII:John" crlf
                         "N;CHARSET=US-ASCII:" crlf
                         "TEL;VOICE;CHARSET=US-ASCII:01 9999 9999" crlf
                         "TEL;CELL;CHARSET=US-ASCII:0001 999 999" crlf
                         "EMAIL;WORK;PREF;CHARSET=US-ASCII:john@example.com" crlf
                         "EMAIL;HOME;CHARSET=US-ASCII:john.2@example.com" crlf
                         "END:VCARD" crlf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "tree-contacts-2_1.org"))
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
        (org-vcard-active-language "en")
        (org-vcard-active-version "4.0")
        (org-vcard-append-to-existing-import-buffer nil))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":KIND: individual" lf
                         ":FIELDTYPE: name" lf
                         ":END:" lf
                         "** joan.2@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":END:" lf
                         "** joan@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":PREFERRED:" lf
                         ":END:" lf
                         "** 0000 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 00 9999 9998" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: fax" lf
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
                         ":PREFERRED:" lf
                         ":END:" lf
                         "** 0001 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 01 9999 9998" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: fax-home" lf
                         ":END:" lf
                         "** 01 9999 9997" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: fax-work" lf
                         ":END:" lf
                         "** 01 9999 9999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: landline" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "tree-vcard-source-4_0.vcf"))
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
        (org-vcard-active-language "en")
        (org-vcard-active-version "3.0")
        (org-vcard-append-to-existing-import-buffer nil))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":KIND: individual" lf
                         ":FIELDTYPE: name" lf
                         ":END:" lf
                         "** joan.2@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":END:" lf
                         "** joan@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":PREFERRED:" lf
                         ":END:" lf
                         "** 0000 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 00 9999 9998" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: fax" lf
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
                         ":PREFERRED:" lf
                         ":END:" lf
                         "** 0001 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 01 9999 9998" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: fax-home" lf
                         ":END:" lf
                         "** 01 9999 9997" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: fax-work" lf
                         ":END:" lf
                         "** 01 9999 9999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: landline" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "tree-vcard-source-3_0.vcf"))
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
        (org-vcard-active-language "en")
        (org-vcard-active-version "2.1")
        (org-vcard-append-to-existing-import-buffer nil))

    (unwind-protect
        (should (string=
                 (concat "* Joan Smith" lf
                         ":PROPERTIES:" lf
                         ":KIND: individual" lf
                         ":FIELDTYPE: name" lf
                         ":END:" lf
                         "** joan.2@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":END:" lf
                         "** joan@example.com" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: email" lf
                         ":PREFERRED:" lf
                         ":END:" lf
                         "** 0000 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 00 9999 9998" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: fax" lf
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
                         ":PREFERRED:" lf
                         ":END:" lf
                         "** 0001 999 999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: cell" lf
                         ":END:" lf
                         "** 01 9999 9998" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: fax-home" lf
                         ":END:" lf
                         "** 01 9999 9997" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: fax-work" lf
                         ":END:" lf
                         "** 01 9999 9999" lf
                         ":PROPERTIES:" lf
                         ":FIELDTYPE: landline" lf
                         ":END:" lf)
                 (progn
                   (generate-new-buffer "*org-vcard-test*")
                   (insert-file-contents-literally (concat org-vcard-tests-data-dir
                                                           "tree-vcard-source-2_1.vcf"))
                   (org-vcard-import-to-tree "buffer" "buffer")
                   (switch-to-buffer "*org-vcard-import*")
                   (buffer-string))))
      (progn
        (if (get-buffer "*org-vcard-test*")
            (kill-buffer "*org-vcard-test*"))
        (if (get-buffer "*org-vcard-import*")
            (kill-buffer "*org-vcard-import*"))))))


;;; org-vcard-tests.el ends here.
