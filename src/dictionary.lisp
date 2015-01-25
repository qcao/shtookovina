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

(defparameter *dictionary* (make-hash-table :test #'equalp)
  "Hash table that allows to get dictionary item by its type and string
representation of the main form (they should be CONSed to get key).")

(defparameter *initial-weight* 9
  "When the user gives right answer when training a word, the weight is
decreased, otherwise it's increased. This is upper limit of the weight.")

(defparameter *base-weight* 1
  "This is a number that represents lower limit of weight of form.")

(defparameter *weight-step* 3
  "Weight is changed discretely, *WEIGHT-STEP* is minimal difference between
consequent values of weight.")

(defvar +aspects-count+ 8
  "Number of 'aspects' (exercise types) per form.")

(defvar *weight-sums* (make-array +aspects-count+ :initial-element 0)
  "Vector of sums of all weights per aspect.")

(defclass dictionary-item ()
  ((forms
    :accessor forms
    :documentation "vector of strings that represent forms of the item")
   (weights
    :accessor weights
    :documentation "array of weights corresponding to items' forms")
   (item-weight
    :initform (make-array +aspects-count+ :initial-element 0)
    :accessor item-weight
    :documentation "vector of sums of all item weights"))
  (:documentation "class that represents an item in the dictionary"))

(defgeneric alter-weight (item form-index aspect-index amount)
  (:documentation "Intelligently change weight for form found at FORM-INDEX,
for aspect denoted by ASPECT-INDEX, and in AMOUNT. All alterations of weight
table should be done with this function."))

(defmethod alter-weight ((item dictionary-item) form-index aspect-index amount)
  (with-slots (weights item-weight) item
    (let* ((value  (aref weights form-index aspect-index))
           (sum    (+ value amount))
           (result (cond ((> sum *initial-weight*) *initial-weight*)
                         ((< sum *base-weight*) *base-weight*)
                         (t sum)))
           (diff   (- result value)))
      (setf (aref weights form-index aspect-index) result)
      (incf (aref item-weight aspect-index) diff)
      (incf (aref *weight-sums* aspect-index) diff)
      result)))

(defmethod initialize-instance :after
    ((item dictionary-item) &key type default-form)
  (with-slots (forms weights) item
    (let ((forms-number (forms-number type)))
      (check-type forms-number (integer 1))
      (setf forms (make-array forms-number
                              :element-type '(simple-array character)
                              :initial-element "")
            (aref forms 0) default-form
            weights (make-array (list forms-number
                                      +aspects-count+)
                                :element-type 'integer
                                :initial-element 0))
      (dotimes (form-index forms-number)
        (dotimes (aspect-index +aspects-count+)
          (alter-weight item form-index aspect-index *initial-weight*))))))

(defun save-dictionary (filename)
  "Write current dictionary image to the file with the name FILENAME. This
mainly serializes contents of *DICTIONARY* and *WEIGHT-SUMS* variables."
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (cl-store:store *dictionary* stream)
    (cl-store:store *weight-sums* stream)
    filename))

(defun load-dictionary (filename)
  "Try to load dictionary from file that has name FILENAME. This operation
sets values of *DICTIONARY* and *WEIGHT-SUMS* variables. The function
returns T on success and NIL on failure."
  (with-open-file (stream filename
                          :direction :input
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist nil)
    (when stream
      (setf *dictionary* (cl-store:restore stream)
            *weight-sums* (cl-store:restore stream))
      t)))

(defun rem-dictionary-item (type default-form)
  "Remove item from the dictionary. Return T if there was such an item and
NIL otherwise."
  (awhen (gethash (cons type default-form) *dictionary*)
    (let ((*initial-weight* 0))
      (dotimes (form-index (length (forms it)))
        (dotimes (aspect-index +aspects-count+)
          (alter-weight it form-index aspect-index 0))))
    (remhash (cons type default-form) *dictionary*)))

(defun add-dictionary-item (type default-form)
  "Add item to the dictionary. The item will have type TYPE (according to
definition of LANGUAGE) and default form DEFAULT-FORM. If the dictionary
already contains this item, the function has no effect."
  (unless (gethash (cons type default-form) *dictionary*)
    (setf (gethash (cons type default-form) *dictionary*)
          (make-instance 'dictionary-item
                         :type type
                         :default-form default-form))))

(defun clear-dictionary ()
  "Clear dictionary removing all its elements."
  (maphash (lambda (key value)
             (declare (ignore value))
             (destructuring-bind (type . default-form) key
               (rem-dictionary-item type default-form)))
           *dictionary*))

(defun edit-dictionary-item
    (type default-form new-form &optional (form-index 0))
  "Edit dictionary item changing one of its forms. Target item is identified
by TYPE and DEFAULT-FORM. Selected form at FORM-INDEX will be replaced with
NEW-FORM. Return NIL if there is no such item in the dictionary and NEW-FORM
otherwise."
  (check-type form-index integer)
  (check-type new-form (simple-array character))
  (awhen (gethash (cons type default-form) *dictionary*)
    (setf (aref (forms it) form-index) new-form)
    (when (zerop form-index)
      (remhash (cons type default-form) *dictionary*)
      (setf (gethash (cons type new-form) *dictionary*)
            it))
    new-form))

(defun item-dec-weight (type default-form form-index aspect-index)
  "Decrease weight of ASPECT-INDEX aspect of form at FORM-INDEX of item
identified by TYPE and DEFAULT-FORM in the dictionary."
  (awhen (gethash (cons type default-form) *dictionary*)
    (alter-weight it form-index aspect-index (- *weight-step*))))

(defun item-inc-weight (type default-form form-index aspect-index)
  "Decrease weight of ASPECT-INDEX aspect of form at FORM-INDEX of item
identified by TYPE and DEFAULT-FORM in the dictionary."
  (awhen (gethash (cons type default-form) *dictionary*)
    (alter-weight it form-index aspect-index *weight-step*)))

(defun get-next-form (aspect-index)
  "Get type, default form, and form index of randomly selected dictionary
item for ASPECT-INDEX."
  (let ((index (random (aref *weight-sums* aspect-index))))
    (when (plusp (hash-table-count *dictionary*))
      (block the-block
        (maphash (lambda (key value)
                   (if (<= index (aref (item-weight value) aspect-index))
                       (destructuring-bind (type . default-form) key
                         (return-from the-block
                           (values type
                                   default-form
                                   (do ((i 0 (1+ i)))
                                       ((<= index (aref (weights value)
                                                        i
                                                        aspect-index)) i)
                                     (decf index (aref (weights value)
                                                       i
                                                       aspect-index))))))
                       (decf index (aref (item-weight value) aspect-index))))
                 *dictionary*)))))

;;; --- tests ---

(load "/home/mark/projects/programs/shtookovina/git/langs/en.lisp")
