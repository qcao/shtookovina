;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Shtookovina has support for multilingual user interface. This module
;;; defines means of registering new translations.
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

(defvar *ui-language* nil
  "This variable is normally bound to an instance of class UI-LANGUAGE that
is used to lookup phrases in current UI language to print them on the
screen.")

(defclass ui-language ()
  ((name
    :initarg :name
    :initform "?"
    :accessor name
    :documentation "name of the language")
   (vocabulary
    :initarg :vocabulary
    :initform (make-hash-table)
    :accessor vocabulary
    :documentation "hash table to loopup local translations of UI elements"))
   (:documentation "class to model local translation of user interface"))

(defmethod make-load-form ((self ui-language) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun set-ui-language (name &rest plist)
  "Set user interface language. NAME is name of the language and PLIST is a
plist of pairs 'keyword - string' that represent local translations of user
interface elements."
  (setf *ui-language*
        (make-instance 'ui-language
                       :name name
                       :vocabulary (plist-hash-table plist)))
  (values))

(defun add-ui-elements (&rest plist)
  "Add new user interface elements (translations). PLIST is a plist of pairs
'keyword - string' that represent local translations of user interface
elements."
  (do ((tail plist (cddr tail)))
      ((null tail) (values))
    (destructuring-bind (id string . rest) tail
      (declare (ignore rest))
      (setf (gethash id (vocabulary *ui-language*))
            string))))

(defun get-ui-locale ()
  "Returns name of active UI langauge."
  (name *ui-language*))

(defun uie (id)
  "Returns local translation of user interface element with id ID."
  (aif (gethash id (vocabulary *ui-language*))
       it
       "?"))

(defun uim (id &rest args)
  "Like UIE, but helps to render colorized string corresponding to ID,
inserting ARGS into it. Returns a list suitable for passing to
TERM:CAT-PRINT."
  (labels ((positions (char str &optional (start 0) acc)
             (aif (position char str :start start :test #'char=)
                  (positions char str (1+ it) (cons it acc))
                  (nreverse acc)))
           (malformed (item)
             (destructuring-bind (start . end) item
               (> start end)))
           (form-pairs (open close str)
             (remove-if #'malformed
                        (mapcar #'cons
                                (positions open str)
                                (positions close str))))
           (prepare (str start end)
             (aif (position #\~ str :start start :end end :test #'char=)
                  (concatenate 'string
                               (subseq str start it)
                               (format nil "~a" (pop args))
                               (subseq str (1+ it) end))
                  (subseq str start end))))
    (awhen (gethash id (vocabulary *ui-language*))
      (let ((brackets (form-pairs #\[ #\] it))
            (parens   (form-pairs #\( #\) it))
            (i        0)
            result)
        (dolist (item (mapcan (lambda (b)
                                (destructuring-bind (bs . be) b
                                  (awhen (find-if (lambda (x)
                                                    (= (car x) (1+ be)))
                                                  parens)
                                    (destructuring-bind (ps . pe) it
                                      (list (list (cons (1+ bs) be)
                                                  (cons (1+ ps) pe)))))))
                              brackets))
          (destructuring-bind ((bs . be) (ps . pe)) item
            (when (< i (1- bs))
              (push (prepare it i (1- bs)) result))
            (push (list (prepare it bs be)
                        (intern (string-upcase (subseq it ps pe)) "KEYWORD"))
                  result)
            (setf i (1+ pe))))
        (when (< i (length it))
          (push (prepare it i (length it)) result))
        (nreverse result)))))
