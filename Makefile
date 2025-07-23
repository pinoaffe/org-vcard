FILE = org-vcard
EMACS = emacs
INSTALL_INFO = install-info
ORG_DIR1 = -L $(word 1,$(wildcard $(HOME)/.emacs.d/elpa/org-9*))
ifeq ($(words $(ORG_DIR1)),1)
	ORG_DIR1 =
endif
ORG_LIB = -l org -l ox-texinfo
# This can be set on the command line to 't' to handle broken links in the Org file on export
# -- however, this for testing purposes - do try to cleanup broken links
ORG_B1B = nil
ORG_BOB = --eval="(setq org-export-with-broken-links $(ORG_B1B))"
ORG_PUB = --eval='(progn (find-file (expand-file-name "$(FILE).org")) (org-texinfo-export-to-texinfo))'
EMACS_ARGS = --batch $(ORG_DIR1) $(ORG_LIB) $(ORG_BOB) $(ORG_PUB)

all: info clean
	@echo all done!

# Run this to update the .info file and dir
info: $(FILE).org
	$(EMACS) $(EMACS_ARGS)
	makeinfo $(FILE).texi
	$(INSTALL_INFO) $(FILE).info dir

$(FILE).org: README.org
	cp README.org $(FILE).org

clean:
	rm -f $(FILE).texi $(FILE).org *~

distclean: clean
	rm -f dir $(FILE).info
