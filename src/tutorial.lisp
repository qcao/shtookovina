;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; This is built-in Шτookωвiнα tutorial. It's called when user first starts
;;; the program and attempts to introduce all key features of Шτookωвiнα.
;;;
;;; Copyright © 2015 Mark Karpov
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

(defvar *tutorial-on* nil
  "Whether tutorial should be displayed.")

(defvar *tutorial-stage* 0
  "Tutorial stage (or level). There are several levels in the
tutorial. Every level introduces some idea or feature.")

(defun tutorial ()
  "Main tutorial routine."
  (case *tutorial-stage*
    (0 (term:print (uie :tutorial-0) :args (name *language*))
       (incf *tutorial-stage*))
    (1 (if (last-command= "help" #'null)
           (progn (term:print (uie :tutorial-1))
                  (incf *tutorial-stage*))
           (term:print (uie :tutorial-try-again))))
    (2 (if (last-command= "help" (lambda (x) (gethash (car x) *commands*)))
           (progn (term:print (uie :tutorial-2))
                  (incf *tutorial-stage*))
           (term:print (uie :tutorial-try-again))))
    (3 (when (and (last-command= "add")
                  (plusp (dictionary-item-count)))
         (term:print (uie :tutorial-3))
         (incf *tutorial-stage*)))
    (4 (when (>= (dictionary-item-count) 12)
         (term:print (uie :tutorial-4))
         (incf *tutorial-stage*)))
    (5 (if (last-command= "train")
           (progn (term:print (uie :tutorial-5))
                  (incf *tutorial-stage*))
           (term:print (uie :tutorial-try-again))))
    (6 (if (last-command= "dict")
           (progn (term:print (uie :tutorial-6))
                  (setf *tutorial-on* nil))
           (term:print (uie :tutorial-try-again))))))
