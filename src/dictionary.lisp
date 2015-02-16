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

(defvar *initial-weight* 9
  "When the user gives right answer when training a word, the weight is
decreased, otherwise it's increased. This is upper limit of the weight.")

(defvar *base-weight* 1
  "This is a number that represents lower limit of weight of form.")

(defvar *weight-step* 3
  "Weight is changed discretely, *WEIGHT-STEP* is minimal difference between
consequent values of weight.")

(defvar +aspects-count+ 3
  "Number of 'aspects' (exercise types) per form. In this version of
Shtookovina we have three aspects: translation, writing, and audition.")

(defparameter *weight-sums* (make-array +aspects-count+ :initial-element 0)
  "Vector of sums of all weights per aspect.")

(defparameter *unknown-form* "?"
  "When some form of a dictionary item is not yet specified by the user, we
use this symbolic string for it.")

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
    :documentation "vector of sums of all item weights")
   (translation
    :initarg :translation
    :initform "?"
    :accessor translation
    :documentation "translation of the dictionary item"))
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
                              :initial-element *unknown-form*)
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

(defun add-dictionary-item (type default-form translation)
  "Add item to the dictionary. The item will have type TYPE (according to
definition of LANGUAGE) and default form DEFAULT-FORM. If the dictionary
already contains this item, the function has no effect. Return NIL value if
there was no such item in the dictionary and non-NIL value otherwise."
  (unless (gethash (cons type default-form) *dictionary*)
    (setf (gethash (cons type default-form) *dictionary*)
          (make-instance 'dictionary-item
                         :type type
                         :default-form default-form
                         :translation translation))))

(defun clear-dictionary ()
  "Clear dictionary removing all its elements. Return number of removed
dictionary items."
  (let ((total (hash-table-count *dictionary*)))
    (maphash-keys
     (lambda (key)
       (destructuring-bind (type . default-form) key
         (rem-dictionary-item type default-form)))
     *dictionary*)
    total))

(defun edit-dictionary-item-form
    (type default-form new-form &optional (form-index 0))
  "Edit dictionary item changing one of its forms. Target item is identified
by TYPE and DEFAULT-FORM. Selected form at FORM-INDEX will be replaced with
NEW-FORM. Return NIL if there is no such item in the dictionary and NEW-FORM
otherwise."
  (awhen (gethash (cons type default-form) *dictionary*)
    (setf (aref (forms it) form-index) new-form)
    (when (zerop form-index)
      (remhash (cons type default-form) *dictionary*)
      (setf (gethash (cons type new-form) *dictionary*)
            it))
    new-form))

(defun edit-dictionary-item-translation (type default-form new-translation)
  "Edit dictionary item changing its translation. Target item is identified
by TYPE and DEFAULT-FORM. Return NIL if there is no such item in the
dictionary and NEW-TRANSLATION otherwise."
  (awhen (gethash (cons type default-form) *dictionary*)
    (setf (translation it) new-translation)))

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

(defun item-reset-progress (type default-form)
  "Set all weights for all forms to *INITIAL-WEIGHT* value (= 0 %
learned). Return NIL if there is no such dictionary item and T otherwise."
  (awhen (gethash (cons type default-form) *dictionary*)
    (dotimes (form-index (length (forms it)))
      (dotimes (aspect-index +aspects-count+)
        (alter-weight it form-index aspect-index *initial-weight*)))
    t))

(defun item-mark-as-learned (type default-form)
  "Set all weights for all forms to *BASE-WEIGHT* value (= 100 %
learned). Return NIL if there is no such dictionary item and T otherwise."
  (awhen (gethash (cons type default-form) *dictionary*)
    (dotimes (form-index (length (forms it)))
      (dotimes (aspect-index +aspects-count+)
        (alter-weight it form-index aspect-index (- *initial-weight*))))
    t))

(defun dictionary-item-count ()
  "Returns total number of dictionary items in the dictionary."
  (hash-table-count *dictionary*))

(defun get-next-form (aspect-index)
  "Get type, default form, and form index of randomly selected dictionary
item for ASPECT-INDEX."
  (let ((index (random (aref *weight-sums* aspect-index))))
    (when (plusp (dictionary-item-count))
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

(defun get-some-forms (aspect-index number)
  "Return list of lists of form (type default-form form-index). NUMBER
specifies how many elements returned list should have. If there is not
enough forms in the dictionary, i.e. NUMBER < number of forms in the
dictionary, return NIL. Otherwise return NUMBER randomly selected items as
with GET-NEXT-FORM. Big values of NUMBER are not recommended."
  (when (block the-block
          (let ((total 0))
            (maphash-values
             (lambda (item)
               (incf total (length (forms item)))
               (when (>= total number)
                 (return-from the-block t)))
             *dictionary*)))
    (do (result
         (total 0))
        ((= total number)
         result)
      (let ((form (multiple-value-list (get-next-form aspect-index))))
        (unless (find form result :test #'equal)
          (push form result)
          (incf total))))))

(defun form (item form-index)
  "Return form of given item ITEM at FORM-INDEX."
  (aref (forms item) form-index))

(defun (setf form) (new-form item form-index)
  "Set form of given item ITEM at FORM-INDEX."
  (setf (aref (forms item) form-index)
        new-form))

(defun item-form (type default-form form-index)
  "The same as FORM, but takes TYPE and DEFAULT-FORM instead of item."
  (form (gethash (cons type default-form) *dictionary*)
        form-index))

(defun (setf item-form) (new-form type default-form form-index)
  "Set form at FORM-INDEX of given item identified by TYPE and DEFAULT-FORM."
  (setf (form (gethash (cons type default-form) *dictionary*)
              form-index)
        new-form))

(defun unknown-form-p (type default-form form-index)
  "Check if specified form of dictionary item is unknown."
  (string= (item-form type default-form form-index)
           *unknown-form*))

(defun item-form-progress (item form-index &optional aspect-index)
  "Calculate progress in percents for form at FORM-INDEX in dictionary item
ITEM. If ASPECT-INDEX is supplied and it's not NIL, calculate progress for
this aspect."
  (let* ((weights (weights item))
         (size    (array-dimension weights 1))
         (total   (* (- *initial-weight*
                        *base-weight*)
                     (if aspect-index 1 size))))
    (floor
     (* 100
        (- total
           (if aspect-index
               (- (aref weights form-index aspect-index)
                  *base-weight*)
               (do ((i 0 (1+ i))
                    (sum 0))
                   ((= i size) sum)
                 (incf sum (- (aref weights form-index i)
                              *base-weight*))))))
        total)))

(defun item-progress (item &optional aspect-index)
  "Calculate progress in percents for given dictionary item ITEM."
  (do ((i 0 (1+ i))
       (size (array-dimension (weights item) 0))
       (sum 0))
      ((= i size)
       (floor sum size))
    (incf sum (item-form-progress item i aspect-index))))

(defun dictionary-progress (&optional aspect-index)
  "Calculate average progress through the dictionary."
  (round (/ (let ((total 0))
              (maphash-values
               (lambda (item)
                 (incf total (item-progress item aspect-index)))
               *dictionary*)
              total)
            (let ((count (dictionary-item-count)))
              (if (zerop count) 1 count)))))
