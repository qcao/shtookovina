;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; This is Shtookovina API to define new user functions and the REPL
;;; itself.
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

(defparameter *commands* (make-hash-table :test #'equalp)
  "Hash table of objects that represent commands that user can type in
REPL.")

(defparameter *command-list* nil
  "List of strings that contains all defined command names. Used for
autocompletion.")

(defparameter *arg-parsers* (make-hash-table)
  "Parsers that are used to parse various types of command arguments in
Shtookovina REPL.")

(defparameter *command-counter* 0
  "Command counter. It's part of Shtookovina prompt. It's incremented every
time READ-COMMAND gets some input successfully.")

(defparameter *session-prompt* "[~a]> "
  "This is a control string that's passed to FORMAT function along with the
value of *COMMAND-COUNTER* to produce Shtookovina session prompt.")

(defparameter *session-terminated* nil
  "If this variable is bound to non-NIL value, Shtookovina REPL will be
terminated.")

(defclass command ()
  ((types
    :initarg :types
    :accessor types
    :documentation "list of types of arguments for this command")
   (fnc
    :initarg :fnc
    :accessor fnc
    :documentation "function that is called to process the command")
   (short-desc-id
    :initarg :short-desc-id
    :accessor short-desc-id
    :documentation "this is id of short description of the command")
   (long-desc-id
    :initarg :long-desc-id
    :accessor long-desc-id
    :documentation "this is id of detailed description of the command")
   (lambda-list
    :initarg :lambda-list
    :accessor lambda-list
    :documentation "lambda list of FNC, it's used for automatic docs"))
  (:documentation "internal representation of a command"))

(defmethod make-load-form ((self command) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmacro define-command (name args (short-desc-id long-desc-id) &body body)
  "Define new user command. It will available in Shtookovina REPL. NAME must
be a string designator. It's the name that user will be able to use to
execute the command. ARGS must be a list describing arguments of the
function where every element is of form (SYMBOL TYPE), where SYMBOL is the
name of the variable and TYPE is its type. Symbol &OPTIONAL is allowed, it
has the same meaning as in usual lambda list. BODY is an implicit PROGN."
  (flet ((make-arg (types arg)
           (cond ((listp arg)
                  (if types (cdr arg) (list (car arg))))
                 ((find arg lambda-list-keywords)
                  (unless types (list arg)))
                 (t (error "illegal element in argument list")))))
    (let ((lambda-list (mapcan (curry #'make-arg nil) args))
          (name (string-downcase name)))
      `(progn
         (setf (gethash ,name *commands*)
               (make-instance 'command
                              :types ',(mapcan (curry #'make-arg t) args)
                              :fnc (lambda ,lambda-list
                                     ,@body)
                              :short-desc-id ,short-desc-id
                              :long-desc-id ,long-desc-id
                              :lambda-list ',lambda-list))
         (pushnew ,name *command-list* :test #'string-equal)
         (setf *command-list* (sort *command-list* #'string<))
         ',name))))

(defun custom-complete (text start end)
  "Custom completion for Shtookovina REPL. First word is completed to a word
from *COMMAND-LIST*, other words are currently not completed."
  (declare (ignore end))
  (labels ((common-prefix (items)
             (subseq
              (car items) 0
              (position
               nil
               (mapcar
                (lambda (i)
                  (every (lambda (x)
                           (char= (char (car items) i)
                                  (char x           i)))
                         (cdr items)))
                (iota (reduce #'min (mapcar #'length items)))))))
           (select-completions (list)
             (let ((els (remove-if-not
                         (lambda (x)
                           (starts-with-subseq text
                                               x
                                               :test #'char-equal))
                         list)))
               (if (cdr els)
                   (cons (common-prefix els) els)
                   els))))
    (when (or (zerop start)
              (every (curry #'char= #\space)
                     (subseq rl:*line-buffer* 0 start)))
      (select-completions *command-list*))))

(rl:register-function :complete #'custom-complete)

(defun string-to-list (str &optional (del #\space))
  "Transform string STR to list of words. Words are separated by DEL in
STR."
  (remove-if #'emptyp (split-sequence del str :test #'char=)))

(defun novelty-check (a-line r-line)
  "Check if actual line A-LINE is different from most recent line in the
history R-LINE."
  (not (equalp (string-to-list a-line)
               (string-to-list r-line))))

(defun read-command ()
  "Read command from user with editing, parse it and return a list of
strings."
  (awhen (rl:readline :prompt (format nil
                                      *session-prompt*
                                      *command-counter*)
                      :add-history t
                      :novelty-check #'novelty-check)
    (incf *command-counter*)
    (let (result)
      (do ((i 0 (1+ i))
           (tail (string-to-list it #\")
                 (cdr tail)))
          ((null tail) result)
        (nconcf result
                (if (evenp i)
                    (string-to-list (car tail))
                    (list (car tail))))))))

(defun correct-command (command)
  "Check if COMMAND is a known command, otherwise perform :UNKNOWN-COMMAND
hook, its non-NIL return value will be used as replacement."
  (if (find (car command) *command-list* :test #'string-equal)
      command
      (perform-hook :unknown-command
                    :args (list command))))

(defmacro define-arg-parser (target-type args &body body)
  "Defined new argument parser for TARGET-TYPE. ARGS must be lambda-list of
arguments that parsing function can take. It should be able to take at least
one argument - string that represents an argument to parse. BODY is an
implicit PROGN."
  `(setf (gethash ',target-type *arg-parsers*)
         (lambda ,args
           ,@body)))

(define-condition arg-parser-failed (simple-error)
  ((raw-arg
    :initarg :raw-arg
    :reader raw-arg)
   (target-type
    :initarg :target-type
    :reader target-type))
  (:report (lambda (condition stream)
             (term:cat-print
              (uim :arg-parser-failed
                   (raw-arg condition)
                   (target-type condition))
              :stream stream)))
  (:documentation "The condition is signaled when argument parser cannot
fulfill its task: when returned value is not of target type or when the
parser signals any condition itself. In both cases, condition of this type
is signaled."))

(defun coerce-arg (target-type arg)
  "This is special version of COERCE to convert strings into Lisp
objects. It converts STRING into object of type OUTPUT-TYPE."
  (awhen (gethash target-type *arg-parsers*)
    (handler-case
        (let ((result (funcall it arg)))
          (assert (typep result target-type))
          result)
      (error (condition)
        (declare (ignore condition))
        (error 'arg-parser-failed
               :raw-arg arg
               :target-type target-type)))))

(defun perform-command (command)
  "Cast arguments of the COMMAND to necessary types and call function that
will perform any necessary processing."
  (destructuring-bind (name . args) command
    (handler-case
        (awhen (gethash name *commands*)
          (apply (fnc it) (mapcar #'coerce-arg (types it) args)))
      (arg-parser-failed (condition)
        (princ condition)
        (term:cat-print
         (uim :help-command-reminder name)))
      (error (condition)
        (declare (ignore condition))
        (term:cat-print
         (uim :command-invalid-call name))
        (term:cat-print
         (uim :help-command-reminder name))))))

(defun session ()
  "This is Shtookovina REPL."
  (perform-hook :session-start)
  (do () (*session-terminated*)
    (awhen (correct-command (read-command))
      (perform-command it)))
  (perform-hook :session-end)
  (values))

(defun print-command-description (command)
  (flet ((args-annotation (args)
           (let (optionals
                 result)
             (dolist (arg args (nreverse result))
               (cond ((eql arg '&optional) (setf optionals t))
                     (optionals
                      (push " [" result)
                      (push (list arg :arg) result)
                      (push "]" result))
                     (t
                      (push " " result)
                      (push (list arg :arg) result))))))
         (type-annotation (args types)
           (let ((last (lastcar args)))
             (mapcan (lambda (a p)
                       (list (list a :arg)
                             " - "
                             (list p :typ)
                             (if (eql a last) "" ", ")))
                     args
                     types))))
    (awhen (gethash command *commands*)
      (term:cat-print
       (cons (list command :cmd)
             (args-annotation (lambda-list it))))
      (when (types it)
        (term:cat-print
         (cons (uie :where)
               (type-annotation (remove '&optional
                                        (lambda-list it)
                                        :count 1)
                                (types it)))))
      (term:cat-print (uim (long-desc-id it))
                      :margin 2))
    (values)))

;; default parsers

(define-arg-parser string (arg)
  arg)

(define-arg-parser integer (arg)
  (parse-integer arg))

(define-arg-parser keyword (arg)
  (intern (string-upcase arg) "KEYWORD"))
