;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; This file is part of Шτookωвiнα.
;;;
;;; Copyright (c) 2015 Mark Karpov
;;;
;;; Шτookωвiнα is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; Шτookωвiнα is distributed in the hope that it will be useful, but
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

(defmacro define-hook (event args &body body)
  "Register hook that will be invoked on specified EVENT. The function must
be able to take appropriate number of arguments. Exact number of arguments
depends on EVENT, see documentation for full list of events. Only one hook
can be registered for any given EVENT. That means that new hooks will
overwrite the old ones."
  `(progn
     (setf (gethash ,event *hooks*)
           (lambda ,args ,@body))
     ,event))

(defun remove-hook (event)
  "Remove hook that is called on EVENT. Returns T if there was a hook
associated with EVENT and NIL otherwise."
  (remhash event *hooks*))

(defun perform-hook (event &key args in-thread put-into-shell)
  "Call hook associated with given EVENT with arguments ARGS (must be a list
designator). If IN-THREAD is non-NIL, create separate thread for the
function (return no value in this case). If PUT-INTO-SHELL is non-NIL,
result of the function will be feed into the system shell (the result must
be a string). Return value actual function evaluates to, or NIL if there is
no such function registered."
  (awhen (gethash event *hooks*)
    (flet ((call-hook ()
             (let ((result (apply it (ensure-list args))))
               (when (and put-into-shell result)
                 (trivial-shell:shell-command result))
               result)))
      (if in-thread
          (progn
            (bordeaux-threads:make-thread #'call-hook
                                          :name (symbol-name event))
            (values))
          (call-hook)))))
