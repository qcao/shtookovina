;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; This file is part of Shtookovina.
;;;
;;; Copyright (c) 2015 Mark Karpov
;;;
;;; Shtookovina is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; Shtookovina is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package #:shtookovina)

(defparameter *hooks* (make-hash-table)
  "This variable is bound to hash table that allows to lookup functions by
given keyword. The keyword corresponds to some predefined event.")

(defun register-hook (event function)
  "Register hook that will be invoked on specified EVENT. Supplied function
FUNCTION must be able to take appropriate number of arguments. Exact number
of arguments depends on EVENT, see documentation for full list of
events. Only one function can be registered for any given EVENT. That means
that new functions may overwrite old ones."
  (setf (gethash event *hooks*) function)
  (values))

(defun remove-hook (event)
  "Remove registered function that is called on EVENT. Returns T if there
was function associated with EVENT and NIL otherwise."
  (remhash event *hooks*))

(defun perform-hook (event &key args in-thread put-into-shell)
  "Call function associated with given EVENT with arguments ARGS (must be
list designator). If IN-THREAD is non-NIL, create separate thread for the
function (return no values in this case). If PUT-INTO-SHELL is non-NIL,
result of the function will be feed into system shell (the result must be a
string, then). Return value actual function evaluates to, or NIL if there is
no such function registered."
  (awhen (gethash event *hooks*)
    (flet ((call-hook ()
             (let ((result (apply it (ensure-list args))))
               (when put-into-shell
                 (trivial-shell:shell-command result))
               result)))
      (if in-thread
          (progn
            (bordeaux-threads:make-thread #'call-hook
                                          :name (symbol-name event))
            (values))
          (call-hook)))))
