/*
;;;
;;; libsleepycat.c -- C wrappers for memory mgmt for BDB backend & others
;;; 
;;; Initial version 8/26/2004 by Ben Lee
;;; <blee@common-lisp.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; This program is released under the following license
;;; ("GPL").  For differenct licensing terms, contact the
;;; copyright holders.
;;;
;;; This program is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General
;;; Public License as published by the Free Software
;;; Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE. See the GNU General Public License
;;; for more details.
;;;
;;; The GNU General Public License can be found in the file
;;; LICENSE which should have been distributed with this
;;; code.  It can also be found at
;;;
;;; http://www.opensource.org/licenses/gpl-license.php
;;;
;;; You should have received a copy of the GNU General
;;; Public License along with this program; if not, write
;;; to the Free Software Foundation, Inc., 59 Temple Place,
;;; Suite 330, Boston, MA 02111-1307 USA
;;;
;;; Portions of this program (namely the C unicode string
;;; sorter) are derived from IBM's ICU:
;;;
;;; http://oss.software.ibm.com/icu/
;;;
;;; Copyright (c) 1995-2003 International Business Machines
;;; Corporation and others All rights reserved.
;;;
;;; ICU's copyright, license and warranty can be found at
;;;
;;; http://oss.software.ibm.com/cvs/icu/~checkout~/icu/license.html
;;;
;;; or in the file LICENSE.
;;;
*/

#include <string.h>
#include <wchar.h>
#include <stdint.h>

/* Pointer arithmetic utility functions */

/* NOTE: Byte order is on a per-machine basis, serialized streams using this
   library will not be compatable between little-endian and big-endian platforms */

/*------------------------------------------------------------------------------
  reader_and_writer

  Generates the following code: 

            double read_double(char *buf, int offset) {
              double d;
              memcpy(&d, buf+offset, sizeof(double));
              return d;
            }
            void write_double(char *buf, double num, int offset) {
              memcpy(buf+offset, &num, sizeof(double));
            }
  When called like this:
            reader_and_writer(double)
--------------------------------------------------------------------------------
*/

#define reader_and_writer( FNAME, DATATYPE )	\
DATATYPE read_##FNAME (char *buf, int offset) { \
  DATATYPE i; \
  memcpy(&i, buf+offset, sizeof( DATATYPE )); \
  return i; \
} \
void write_##FNAME (char *buf, DATATYPE num, int offset) { \
  memcpy(buf+offset, &num, sizeof( DATATYPE )); \
}

reader_and_writer(int32,int32_t)
reader_and_writer(uint32,uint32_t)
reader_and_writer(int64,int64_t)
reader_and_writer(uint64,uint64_t)
reader_and_writer(float,float)
reader_and_writer(double,double)

char *offset_charp(char *p, int offset) {
  return p + offset;
}

void copy_buf(char *dest, int dest_offset, char *src, int src_offset, 
	      int length) {
  memcpy(dest + dest_offset, src + src_offset, length);
}

