;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; gc.lisp - A wrapper around the migrate interface to support
;;;           stop-and-copy GC at the repository level
;;; 
;;; By Ian Eslick <ieslick at common-lisp.net>
;;;
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Portions Copyright (c) 2005-2007 by Robert Read and Ian Eslick
;;; <rread common-lisp net> <ieslick common-lisp net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :elephant)

(defgeneric stop-and-copy-gc (sc &key &allow-other-keys)
  (:documentation "Wrap all the migrate machinery in a
   simple top-level gc call.  This will copy all the data
   in the source store to the target store"))

(defmethod stop-and-copy-gc ((src store-controller) &key target mapspec replace-source delete-source)
  (when map-spec (set-oid-spec mapspec))
  (let ((target (gc-open-target sc target-spec))
	(src-spec (controller-spec src)))
    (migrate target src)
    (when map-spec 
      (set-oid-spec nil)
      (delete-spec mapspec))
    (when (or replace-source delete-source)
      (close-store src)
      (delete-spec src))
    (when replace-source
      (copy-spec source target))))

(defun delete-spec (spec)
  "Delete the storage associated with spec"
  )

(defun copy-spec (src targ)
  "Copy files associated with spec from src to targ"
  )


