;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: UFFI -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          os.lisp
;;;; Purpose:       Operating system interface for UFFI
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg.
;;;;
;;;; *************************************************************************

(in-package #:uffi)


(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (sys::getenv (string var))
  #+cmu (cdr (assoc (string var) ext:*environment-list* :test #'equalp
                    :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+(or openmcl digitool) (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var)
  #-(or allegro clisp cmu gcl lispworks lucid openmcl digitool sbcl)
  (error 'not-implemented :proc (list 'getenv var)))


;; modified from function ASDF -- Copyright Dan Barlow and Contributors

(defun run-shell-command (control-string  &rest args &key output)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *trace-output*.  Returns the shell's exit code."
  (unless output
    (setq output *trace-output*))

  (let ((command (apply #'format nil control-string args)))
    #+sbcl
    (sb-impl::process-exit-code
     (sb-ext:run-program
      "/bin/sh"
      (list "-c" command)
      :input nil :output output))

    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list "-c" command)
      :input nil :output output))

    #+allegro
    (excl:run-shell-command command :input nil :output output)

    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :output-stream output)

    #+clisp                             ;XXX not exactly *trace-output*, I know
    (ext:run-shell-command  command :output :terminal :wait t)

    #+openmcl
    (nth-value 1
               (ccl:external-process-status
                (ccl:run-program "/bin/sh" (list "-c" command)
                                 :input nil :output output
                                 :wait t)))

    #-(or openmcl clisp lispworks allegro scl cmu sbcl)
    (error "RUN-SHELL-PROGRAM not implemented for this Lisp.")
    ))
