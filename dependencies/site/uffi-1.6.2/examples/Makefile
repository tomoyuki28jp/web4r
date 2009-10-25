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
#

SUBDIRS:=

include ../Makefile.common

.PHONY: distclean
distclean: clean


base=c-test-fns
source=$(base).c
object=$(base).o
shared_lib=$(base).so

.PHONY: all
all: $(shared_lib)

linux: $(source) Makefile
	gcc -fPIC -DPIC -c $(source) -o $(object)
	gcc -shared $(object) -o $(shared_lib)
	rm $(object)

mac:
	cc -dynamic -c $(source) -o $(object)
	ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -o $(base).dylib $(object)
	ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress /usr/lib/libz.dylib -o z.dylib

solaris:
	cc -KPIC -c $(source) -o $(object)
	cc -G $(object) -o $(shared_lib)

aix-acl:
	gcc -c -D_BSD -D_NO_PROTO -D_NONSTD_TYPES -D_MBI=void $(source)
	make_shared -o $(shared_lib) $(object)
