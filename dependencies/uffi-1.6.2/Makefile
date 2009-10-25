# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the uffi package
#  Programer:    Kevin M. Rosenberg, M.D.
#  Date Started: Mar 2002
#
#  CVS Id:   $Id$
#
# This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg

PKG:=uffi
DEBPKG=cl-uffi
SUBDIRS:= examples src benchmarks
DOCSUBDIRS:=doc

include Makefile.common


.PHONY: all
all: 


.PHONY: distclean
distclean: clean
	@$(MAKE) -C doc $@
#	./debian/rules clean


SOURCE_FILES=src doc examples Makefile uffi.system uffi.debian.system \
	benchmarks COPYRIGHT README TODO INSTALL ChangeLog NEWS \
	test-examples.cl set-logical.cl 

.PHONY: doc
doc:
	$(MAKE) -C doc

.PHONY: dist
dist: clean
	$(MAKE) -C doc $@

.PHONY: TAGS
TAGS:
	if [ -f TAGS ]; then mv -f TAGS TAGS~; fi
	find . -name \*.lisp -exec /usr/bin/etags -a \{\} \;
