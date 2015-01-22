;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Dictionary module manages user's dictionary and provides API that other
;;; parts of the program can use to select words and their forms that need
;;; training, change parameters of the words according to progress or
;;; regress made by the user, as revealed through practice session.
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

(defvar *dictionary* (make-hash-table :test #'equalp)
  "Hash table that allows to get dictionary item by its type and string
representation of the main form.")

(defvar *dictionary-weight-sums* nil
  "This is an alist that contains sums of all values in corresponding weight
vectors. It's used to pick up next pseudo-random word for training.")

(defparameter *initial-weight* 3
  "When the user gives right answer when training a word, the weight is
decreased, otherwise it's increased. This is upper limit of the weight.")

(defparameter *base-weight* (/ 1 *initial-weight*)
  "This is a number [0..1] that represents lower limit of weight of form.")

(defclass dictionary-item ()
  ((forms
    :initarg :forms
    :initform (make-array 1 :initial-element "")
    :accessor forms
    :documentation "vector of strings that represent forms of the item")
   (weights
    :initarg :weights
    :initform (make-array 1 :initial-element NIL)
    :accessor weights
    :documentation "alist of vectors of weights corresponding to items'
forms"))
  (:documentation "class that represents item in the dictionary"))

(defun default-dictionary ()
  "Resets dictionary state to its default.")

(defun load-dictionary (filename)
  "Try to load dictionary from file that has name FILENAME. This operation
sets values of *DICTIONARY* and *DICTIONARY-WEIGHT-SUMS* variables.")

(defun save-dictionary (filename)
  "Write current dictionary image to the file with the name FILENAME. This
mainly serializes contents of *DICTIONARY* variable.")

(defun add-dictionary-item (type default-form)
  "Add item to the dictionary. The item will have type TYPE and default form
DEFAULT-FORM. If the dictionary already contains this item, its weight will
be reset to *INITIAL-WEIGHT*.")

(defun rem-dictionary-item (type default-form)
  "Remove item from the dictionary.")

(defun edit-dictionary-item (type default-form form-index new-form)
  "Edit dictionary item changing one of its forms and possibly type of the
item. Target item is identified by TYPE and DEFAULT-FORM. Selected form at
FORM-INDEX will be replaced with NEW-FORM.")

(defun item-dec-weight (type default-form exc-type form-index)
  "Decrease weight of form at FORM-INDEX of item identified by TYPE and
DEFAULT-FORM in the dictionary.")

(defun item-inc-weight (type default-form exc-type form-index)
  "Increase weight of form at FORM-INDEX of item identified by TYPE and
DEFAULT-FORM in the dictionary.")

(defun get-next-form (exc-type)
  "Get type, default form, and index of randomly selected dictionary item
for exercises type EXC-TYPE.")
