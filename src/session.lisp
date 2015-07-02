;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; This is Шτookωвiнα API to define new user functions and the REPL
;;; itself.
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

(defparameter *commands* (make-hash-table :test #'equalp)
  "Hash table of objects that represent commands that user can type in
REPL.")

(defparameter *command-list* nil
  "List of strings that contains all defined command names. Used for
autocompletion.")

(defparameter *arg-parsers* (make-hash-table)
  "Parsers that are used to parse various types of command arguments in
Шτookωвiнα REPL.")

(defparameter *command-counter* 0
  "Command counter. It's part of Шτookωвiнα prompt. It's incremented every
time `read-command' returns some input successfully.")

(defparameter +session-prompt+ "[~a]> "
  "This is a control string that's passed to FORMAT function along with the
value of `*command-counter*' to produce Шτookωвiнα session prompt.")

(defparameter *session-terminated* nil
  "If this variable is bound to non-NIL value, Шτookωвiнα REPL will be
terminated.")

(defparameter *command-recognition-threshold* 1
  "Maximal Damerau-Levenshtein distance between entered command and existing
command that may be presented to user to choose from when there is no exact
match.")

(defparameter *session-history* nil
  "List of all commands executed in current session (from newest to
oldest).")

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
    :documentation "id of short description of the command")
   (long-desc-id
    :initarg :long-desc-id
    :accessor long-desc-id
    :documentation "id of detailed description of the command")
   (lambda-list
    :initarg :lambda-list
    :accessor lambda-list
    :documentation "lambda list of FNC, it's used for automatic docs"))
  (:documentation "internal representation of a command"))

(defmethod make-load-form ((self command) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmacro define-command (name args (short-desc-id long-desc-id) &body body)
  "Define new user command. It will available in Шτookωвiнα REPL. NAME must
be a string designator. It's the name that user will be able to use to
execute the command. ARGS must be a list describing arguments of the
function where every element is of form (SYMBOL TYPE), where SYMBOL is the
name of the variable and TYPE is its type. Symbol &OPTIONAL is allowed, it
has the same meaning as in usual lambda list. BODY is an implicit progn."
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

(defun session-std-complete (text start end)
  "Custom completion for Шτookωвiнα REPL. First word is completed to a word
from `*command-list*', other words are currently not completed."
  (declare (ignore start end))
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
    (select-completions *command-list*)))

(defun repeat-audio-query (arg key)
  "Call `audio-query' silently with the same text as in its last call."
  (declare (ignore arg key))
  (when *last-audio-query*
    (audio-query *last-audio-query*
                 :silent-success t
                 :silent-failure t)))

(defun string-to-list (str &optional (del #\space))
  "Transform string STR to list of words. Words are separated by DEL in
STR."
  (remove-if #'emptyp (split-sequence del str :test #'char=)))

(defun novelty-check (a-line r-line)
  "Check if actual line A-LINE is different from most recent line in the
history R-LINE."
  (not (equalp (string-to-list a-line)
               (string-to-list r-line))))

(defun readline (prompt &key add-history (num-chars 0))
  "Generalized version of readline. It checks if current input stream is
interactive. If it's interactive we use real GNU Readline library here,
otherwise we just read a line of text after printing given prompt. PROMPT is
prompt to print, ADD-HISTORY, if given, specifies whether to add current
input to the history, NUM-CHARS is the number of characters to read (zero
means unlimited)."
  (if (interactive-stream-p *standard-input*)
      (rl:readline :prompt prompt
                   :add-history add-history
                   :num-chars num-chars
                   :novelty-check #'novelty-check)
      (progn
        (princ prompt)
        (read-line))))

(defun read-command ()
  "Read command from user with editing, parse it and return a list of
strings."
  (awhen (readline (format nil +session-prompt+ *command-counter*)
                   :add-history t)
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
  "Defined new argument parser for TARGET-TYPE. ARGS must be lambda list of
arguments that parsing function can take. It should be able to take at least
one argument — string that represents an argument to parse. BODY is an
implicit progn."
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
             (term:print (uie :arg-parser-failed)
                         :args (list (raw-arg condition)
                                     (target-type condition))
                         :stream stream)))
  (:documentation "The condition is signaled when argument parser cannot
fulfill its task: when returned value is not of target type or when the
parser itself signals a condition."))

(defun coerce-arg (target-type arg)
  "This is special version of `coerce' to convert strings into Lisp
objects. It converts string ARG into object of type TARGET-TYPE (unless ARG
is already of desired type)."
  (if (typep arg target-type)
      arg
      (awhen (gethash target-type *arg-parsers*)
        (handler-case
            (let ((result (funcall it arg)))
              (assert (typep result target-type))
              result)
          (condition (condition)
            (declare (ignore condition))
            (error 'arg-parser-failed
                   :raw-arg arg
                   :target-type target-type))))))

(defun perform-command (command &rest args)
  "Cast arguments of the COMMAND to necessary types and call function that
will perform any necessary processing."
  (let ((command (string-downcase command)))
    (handler-case
        (awhen (gethash command *commands*)
          (apply (fnc it) (mapcar #'coerce-arg (types it) args)))
      (arg-parser-failed (condition)
        (princ condition)
        (term:print (uie :help-command-reminder)
                    :args command))
      (condition (condition)
        (declare (ignore condition))
        (term:print (uie :command-invalid-call)
                    :args command)
        (term:print (uie :help-command-reminder)
                    :args command)))))

(defmacro with-exit-on-sigint (global &body body)
  "Immediately exit BODY if SIGINT is sent. If GLOBAL is not NIL, set
variable of `*session-terminated*' to T."
  `(handler-case
       (progn ,@body)
     (sb-sys:interactive-interrupt (condition) ; fix: implementation dependent
       (declare (ignore condition))
       (terpri)
       (finish-output)
       ,(when global
          '(setf *session-terminated* t)))))

(defun session ()
  "This is Шτookωвiнα REPL."
  (setf *session-terminated* nil)
  (perform-hook :session-start)
  (do (input)
      (*session-terminated*)
    (with-exit-on-sigint t
      (perform-hook :tutorial-hook)
      (setf input (read-command))
      (awhen (and (not (emptyp input))
                  (correct-command input))
        (push it *session-history*)
        (apply #'perform-command it)
        (incf *command-counter*)
        (perform-hook :after-command
                      :args it))))
  (perform-hook :session-end)
  (values))

(defun last-command= (command &optional arg-predicate)
  "Check if last (newest) command in session history equal to given command
COMMAND (must be a string designator). If ARG-PREDICATE is supplied it must
be predicate to check arguments of the command. Only if the predicate
returns non-NIL value `last-command=' will return T."
  (awhen (car *session-history*)
    (destructuring-bind (c . a) it
      (and (string-equal c command)
           (or (not arg-predicate)
               (funcall arg-predicate a))))))

(defun int-yes-or-no (default-yes)
  "Interactively ask user to answer «yes» or «no». If DEFAULT-YES is not
NIL, consider empty input «yes», otherwise «no»."
  (awhen (readline
          (format nil +session-prompt+
                  (if default-yes "Yn" "Ny"))
          :num-chars 1)
    (if (emptyp it)
        default-yes
        (char-equal (char it 0) #\y))))

(defun int-select-option (options &optional (item-style :default))
  "Interactively select an option from OPTIONS. Return the option on success
and NIL on failure (canceled)."
  (term:o-list options
               :index       :letter
               :index-style :arg
               :item-style  item-style)
  (let ((input (readline
                (format nil +session-prompt+
                        (coerce (mapcar #'code-char
                                        (iota (length options)
                                              :start (char-code #\a)))
                                'string))
                :num-chars 1)))
    (when (and input
               (not (emptyp input))
               (alpha-char-p (char input 0)))
      (nth (- (char-code (char input 0))
              (char-code #\a))
           options))))

(defun int-correct-command (command)
  "Interactively correct given command COMMAND. Return corrected command on
success or NIL on failure."
  (let* ((command (string-downcase command))
         (options (remove-if (lambda (x)
                               (> (mksm:damerau-levenshtein command x)
                                  *command-recognition-threshold*))
                             *command-list*)))
    (if options
        (progn
          (term:print (uie :possible-corrections)
                      :args command)
          (int-select-option options :cmd))
        (term:print (uie :uncorrectable-command)
                    :args command))))

(defun print-command-description (command)
  "Print full description of given command COMMAND. The information includes
synopsis, list of arguments (with types), and long description of the
command itself."
  (flet ((args-annotation (args)
           (let (optionals
                 result)
             (dolist (arg args (nreverse result))
               (cond ((eql arg '&optional) (setf optionals t))
                     (optionals
                      (push " [" result)
                      (if (consp arg)
                          (progn
                            (push (list (car arg) :arg) result)
                            (push " → " result)
                            (push (list (cadr arg) :arg) result))
                          (push (list arg :arg) result))
                      (push "]" result))
                     (t
                      (push " " result)
                      (push (list arg :arg) result))))))
         (type-annotation (args types)
           (let ((last (lastcar args)))
             (mapcan (lambda (a p)
                       (list (list (if (consp a) (car a) a) :arg)
                             " — "
                             (list p :typ)
                             (if (eql a last) "" ", ")))
                     args
                     types))))
    (when-let* ((command (if (gethash command *commands*)
                             (string-downcase command)
                             (int-correct-command command)))
                (it      (gethash command *commands*)))
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
      (term:print (uie (long-desc-id it))
                  :margin 2))
    (values)))

;; Default Parsers

(define-arg-parser string (arg)
  (string arg))

(define-arg-parser integer (arg)
  (parse-integer arg))

(define-arg-parser keyword (arg)
  (car (find arg (get-lexemes) :test #'string-equal :key #'cadr)))
