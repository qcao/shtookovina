;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; This module defines concept of language in Shtookovina. Later we'll be
;;; able to use it to define models of various natural languages.
;;;
;;; Copyright (c) 2014 Mark Karpov
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package #:shtk)

(defvar *shtk-langs* (make-hash-table)
  "This variable is bound to hash table of all supported languages.")

(defvar *shtk-actual-lang* :none
  "This variable should be bound to name of language that user wishes to
learn during Shtookovina session.")

(defclass lang ()
  ((name
    :initarg :name
    :initform "?"
    :accessor name)
   (lexemes
    :initarg :lexemes
    :initform (make-hash-table)
    :accessor lexemes)))

(defmethod make-load-form ((self lang) &optional env)
  "This method is needed to create language objects at compile time."
  (make-load-form-saving-slots self :environment env))

(defclass lexeme ()
  ((name
    :initarg :name
    :initform "?"
    :accessor name)
   (forms
    :initarg :forms
    :initform (make-array 0)
    :accessor forms)))

(defmethod make-load-form ((self lexeme) &optional env)
  "This method is needed to create lexemes at compile time."
  (make-load-form-saving-slots self :environment env))

(defmacro deflang (id name &rest lexemes)
  "This macro defines model of natural language."
  (labels ((build-forms (arg)
             (if arg
                 (destructuring-bind (spec . others) arg
                   (let ((lst (append spec
                                      (when others
                                        (apply #'map-product
                                               (curry #'concatenate 'string)
                                               others)))))
                     (make-array (length lst) :initial-contents lst)))
                  (make-array 1 :initial-contents '("default form"))))
           (mould (arg)
             (destructuring-bind (id name . forms) arg
               (values id
                       (make-instance 'lexeme
                                      :name name
                                      :forms (build-forms forms)))))
           (make-lexemes (arg)
             (let ((result (make-hash-table)))
               (dolist (x arg result)
                 (multiple-value-bind (id lexeme) (mould x)
                   (format t "~s ~s~%" id lexeme)
                   (setf (gethash id result) lexeme))))))
    `(setf (gethash ,id *shtk-langs*)
           (make-instance 'lang
                          :name ,name
                          :lexemes ,(make-lexemes lexemes)))))

(defun get-lang (lang)
  "This is a helper function, it tries to find requested language LANG and
if it succeeds, it returns it. Otherwise it returns NIL and prints a
message."
  (aif (gethash lang *shtk-langs*)
       it
       (format t "couldn't find definition of ~s;" lang)))

(defun print-lexemes (&optional (lang *shtk-actual-lang*))
  "Display lexemes of given language."
  (awhen (get-lang lang)
    (maphash (lambda (k v)
               (format t "~16s~a~%" k (name v)))
             (lexemes it))
    t))

(defun get-lexeme (lexeme &optional (lang *shtk-actual-lang*))
  "If lexeme LEXEME exists in language LANG, it is returned. Otherwise NIL
is returned."
  (awhen (get-lang lang)
    (or (gethash lexeme (lexemes it))
        (format t "language ~s doesn't contain lexeme ~s;" lang lexeme))))

(defun print-forms (lexeme &optional (lang *shtk-actual-lang*))
  "Display forms of given lexeme."
  (awhen (get-lexeme lexeme lang)
    (dotimes (x (length (forms it)))
      (format t "~16@<~d~>~a~%" x (aref (forms it) x)))))

(defun forms-number (lexeme &optional (lang *shtk-actual-lang*))
  "Returns number of forms that has LEXEME in LANG. If there is no such
lexeme of language, it returns NIL."
  (awhen (get-lexeme lexeme lang)
    (length (forms it))))

(defun form-name (lexeme form &optional (lang *shtk-actual-lang*))
  "Returns name of specified FORM of LEXEME."
  (awhen (get-lexeme lexeme lang)
    (aref (forms it) form)))
