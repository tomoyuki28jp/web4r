# FILE IDENTIFICATION
#
#  Name:         Makefile
#  Purpose:      Makefile for UFFI examples
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id$
#
# This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg

SUBDIRS=

include ../Makefile.common

base=uffi-c-test
source=$(base).c
object=$(base).o
shared_lib=$(base).so

.PHONY: all
all: $(shared_lib)

$(shared_lib): $(source) Makefile
	BASE=$(base) OBJECT=$(object) SOURCE=$(source) SHARED_LIB=$(shared_lib) sh make.sh
	rm $(object)

.PHONY: distclean
distclean: clean
	rm -f $(base).dylib $(base).dylib $(base).so $(base).o
