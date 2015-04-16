;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; All user-friendly commands live here. It includes commands to start some
;;; exercise, get information or statistics, open external resource (such as
;;; generated URL in your favorite web-browser, so you can check an online
;;; dictionary), and more. See how the commands are defined, and you will be
;;; able to add your own commands easily.
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

(define-command quit ()
    (:cmd-quit-s :cmd-quit-l)
  (setf *session-terminated* t))

(define-command help (&optional (command string))
    (:cmd-help-s :cmd-help-l)
  (if command
      (print-command-description command)
      (progn
        (term:print (uie :available-commands)
                    :base-style :hdr)
        (terpri)
        (term:table (cons (list (uie :command) (uie :description))
                          (mapcar (lambda (x)
                                    (list x (uie (short-desc-id
                                                  (gethash x *commands*)))))
                                  *command-list*))
                    :cell-style '(:cmd :default)
                    :header-style :hdr
                    :border-style nil
                    :column-width 10))))

(define-command lang ()
    (:cmd-lang-s :cmd-lang-l)
  (term:print (uie :current-language)
              :args (name *language*)))

(define-command ui-lang ()
    (:cmd-ui-lang-s :cmd-ui-lang-l)
  (term:print (uie :current-ui-language)
              :args (get-ui-locale)))

(define-command lexemes ()
    (:cmd-lexemes-s :cmd-lexemes-l)
  (term:table (cons (uie :lexemes)
                    (mapcar #'cadr (get-lexemes)))
              :cell-style :typ
              :header-style :hdr
              :border-style nil
              :column-width 15))

(define-command forms ((lexeme keyword))
    (:cmd-forms-s :cmd-forms-l)
  (if (get-lexeme lexeme)
      (progn
        (term:print (uie :lexeme-forms)
                    :base-style :hdr)
        (terpri)
        (term:table (cons (list (uie :index) (uie :name))
                          (mapcar (lambda (i form)
                                    (list i (if (emptyp form)
                                                (uie :default-form)
                                                form)))
                                  (iota (forms-number lexeme))
                                  (get-forms lexeme)))
                    :cell-style '(:arg :default)
                    :header-style :hdr
                    :border-style nil
                    :column-width 10))
      (term:print (uie :no-such-lexeme)
                  :args lexeme)))

(define-command audio ((text string))
    (:cmd-audio-s :cmd-audio-l)
  (audio-query text))

(define-command add ((type keyword)
                     (default-form string)
                     (translation string))
    (:cmd-add-s :cmd-add-l)
  (if (add-item type default-form translation)
      (term:print (uie :dict-item-added)
                  :args (list (lexeme-name type) default-form))
      (term:print (uie :dict-item-already-exists)
                  :args (list (lexeme-name type) default-form))))

(define-command rem ((type keyword)
                     (default-form string))
    (:cmd-rem-s :cmd-rem-l)
  (if (rem-item type default-form)
      (term:print (uie :dict-item-removed)
                  :args (list (lexeme-name type) default-form))
      (term:print (uie :dict-no-such-item)
                  :args (list (lexeme-name type) default-form))))

(define-command clear ()
    (:cmd-clear-s :cmd-clear-l)
  (term:print (uie :dict-cleared)
              :args (clear-dictionary)))

(define-command learned ((type keyword)
                         (default-form string))
    (:cmd-learned-s :cmd-learned-l)
  (if (item-mark-as-learned type default-form)
      (term:print (uie :dict-item-learned)
                  :args (list (lexeme-name type) default-form))
      (term:print (uie :dict-no-such-item)
                  :args (list (lexeme-name type) default-form))))

(define-command reset ((type keyword)
                       (default-form string))
    (:cmd-reset-s :cmd-reset-l)
  (if (item-reset-progress type default-form)
      (term:print (uie :dict-item-reset)
                  :args (list (lexeme-name type) default-form))
      (term:print (uie :dict-no-such-item)
                  :args (list (lexeme-name type) default-form))))

(define-command eform ((type keyword)
                       (default-form string)
                       (new-form string)
                       &optional
                       ((form-index 0) integer))
    (:cmd-eform-s :cmd-eform-l)
  (check-type form-index (integer 0))
  (if (edit-item-form type
                      default-form
                      new-form
                      form-index)
      (term:print (uie :dict-form-changed)
                  :args (list (lexeme-name type)
                              default-form
                              (form-name type form-index)))
      (term:print (uie :dict-no-such-item)
                  :args (list (lexeme-name type) default-form))))

(define-command etrans ((type keyword)
                        (default-form string)
                        (new-translation string))
    (:cmd-etrans-s :cmd-etrans-l)
  (if (edit-item-translation type
                             default-form
                             new-translation)
      (term:print (uie :dict-trans-changed)
                  :args (list (lexeme-name type) default-form))
      (term:print (uie :dict-no-such-item)
                  :args (list (lexeme-name type) default-form))))

(define-command dict (&optional (prefix string))
    (:cmd-dict-s :cmd-dict-l)
  (if prefix
      (flet ((print-item (type item)
               (term:print (uie :dict-entry-header)
                           :args (list (form item 0)
                                       (lexeme-name type)
                                       (translation item)
                                       (item-progress item)))
               (terpri)
               (term:table (cons (list (uie :index)
                                       (uie :name)
                                       (uie :value)
                                       (uie :progress))
                                 (mapcar (lambda (i form)
                                           (list i
                                                 (if (emptyp form)
                                                     (uie :default-form)
                                                     form)
                                                 (form item i)
                                                 (item-form-progress item i)))
                                         (iota (forms-number type))
                                         (get-forms type)))
                           :cell-style '(:arg :default :default :arg)
                           :header-style :hdr
                           :border-style nil
                           :column-width '(10 30 30 10))
               (terpri)))
        (maphash (lambda (key item)
                   (destructuring-bind (type . default-form) key
                     (when (starts-with-subseq prefix default-form
                                               :test #'char-equal)
                       (print-item type item))))
                 *dictionary*))
      (progn
        (term:print (uie :dict-general)
                    :args (list (dictionary-item-count)
                                (dictionary-progress)))
        (terpri)
        (term:table (cons (list (uie :aspect)
                                (uie :progress))
                          (mapcar (lambda (uie-id index)
                                    (list (uie uie-id)
                                          (dictionary-progress index)))
                                  '(:aspect-translation
                                    :aspect-writing
                                    :aspect-listening)
                                  (iota +aspects-count+)))
                    :cell-style '(:default :arg)
                    :header-style :hdr
                    :border-style nil
                    :column-width '(20 20)))))

(define-command query ((word string))
    (:cmd-query-s :cmd-query-l)
  (perform-hook :query-ext
                :args word
                :in-thread t
                :put-into-shell t))

(define-command conj ((verb string))
    (:cmd-conj-s :cmd-conj-l)
  (perform-hook :conj-ext
                :args verb
                :in-thread t
                :put-into-shell t))

(define-command history (&optional ((items 10) integer))
    (:cmd-history-s :cmd-history-l)
  (check-type items (integer 0))
  (flet ((auto-quote (str)
           (if (find #\space str :test #'char=)
               (format nil "\"~a\"" str)
               str)))
    (do ((i *command-counter* (1- i))
         (h *session-history* (cdr h)))
        ((or (null h)
             (= (- *command-counter* i)
                items)))
      (let ((item (car h)))
        (term:cat-print
         (append (list (format nil "~4d" i) ": " (list (car item) :cmd))
                 (mapcan (lambda (x)
                           (list " " (list (auto-quote x) :arg)))
                         (cdr item))))))))

;; Before we write definitions of exercises, we need to define some helpers.

(defun pick-forms (aspect-index quantity)
  "Return forms for exercises. If some forms are unknown, ask user to fill
them out. If there is not enough forms in the dictionary, return NIL."
  (let ((forms (pick-some-forms aspect-index quantity)))
    (dolist (form forms forms)
      (when (apply #'unknown-form-p form)
        (destructuring-bind (type default-form form-index) form
          (term:print (uie :unknown-form-query)
                      :args (list (lexeme-name type)
                                  default-form
                                  (form-name type form-index)))
          (setf (item-form type default-form form-index)
                (readline (format nil +session-prompt+ "?")))
          (term:print (uie :dict-form-changed)
                      :args (list (lexeme-name type)
                                  default-form
                                  (form-name type form-index))))))))

(defmacro exercise ((aspect-index
                     progress
                     target-forms
                     helper-forms
                     description-uie)
                    &body body)
  "This macro provides useful logic that can be used when programming an
exercise. In order to enter the exercise user must have at least
TARGET-FORMS + HELPER-FORMS forms in his/her dictionary. The exercise will
be executed in a loop until PROGRESS percents of overall progress is done
for ASPECT-INDEX aspect index (unless overall progress is already
sufficiently high so that amount of extra progress is impossible to
achieve). TARGET-WORDS words will be generated and bound to symbol TARGETS,
HELPER-WORDS words will be generated and bound to symbol HELPERS. Return
value of BODY affects weights of words specified in TARGETS variable, if
BODY evaluates to NIL, weights will be increased, otherwise decreased."
  (with-gensyms (executed-once not-enough init-progress forms)
    `(do (,executed-once
          ,not-enough
          (,init-progress (dictionary-progress ,aspect-index)))
         ((or ,not-enough
              (and ,executed-once
                   (>= (dictionary-progress ,aspect-index)
                       (min (+ ,init-progress
                               ,progress)
                            100)))))
       (let ((,forms (pick-forms ,aspect-index
                                 ,(+ target-forms helper-forms))))
         (if ,forms
             (let ((targets (subseq ,forms 0 ,target-forms))
                   (helpers (nthcdr ,target-forms ,forms)))
               (declare (ignorable targets helpers))
               (unless ,executed-once
                 (term:print (uie ,description-uie)))
               (let ((result (progn ,@body)))
                 (dolist (form targets)
                   (destructuring-bind (type default-form form-index) form
                     (funcall (if result
                                  #'item-dec-weight
                                  #'item-inc-weight)
                              type
                              default-form
                              form-index
                              ,aspect-index)))
                 (term:print (uie (if result :correct :incorrect)))
                 (setf ,executed-once t)))
             (progn
               (setf ,not-enough t)
               (term:print (uie :not-enough-forms))))))))

(define-command trans (&optional ((progress 20) integer))
    (:cmd-trans-s :cmd-trans-l)
  (check-type progress (integer 1))
  (exercise (0 progress 1 3 :exercise-translation)
    (destructuring-bind (type default-form form-index) (car targets)
      (let* ((trans-word (zerop (random 2)))
             (prepare (lambda (x)
                        (if trans-word
                            (apply #'item-form x)
                            (destructuring-bind (type default-form form-index)
                                x
                              (format nil "~a ~a"
                                      (item-translation type default-form)
                                      (form-name type form-index))))))
             (target-item (funcall prepare (car targets))))
        (if trans-word
            (term:cat-print
             (list (list (lexeme-name type) :typ)
                   " "
                   (item-translation type default-form)
                   " "
                   (form-name type form-index)))
            (term:cat-print
             (list (list (lexeme-name type) :typ)
                   " "
                   (list (apply #'item-form (car targets)) :arg))))
        (string= target-item
                 (int-select-option
                  (shuffle
                   (cons target-item
                         (mapcar prepare helpers)))))))))

(define-command const (&optional ((progress 20) integer))
    (:cmd-const-s :cmd-const-l)
  (check-type progress (integer 1))
  (exercise (1 progress 1 0 :exercise-constructor)
    (destructuring-bind (type default-form form-index) (car targets)
      (let ((target-item (item-form type default-form form-index)))
        (term:cat-print
         (list (list (lexeme-name type) :typ)
               " "
               (item-translation type default-form)
               " "
               (form-name type form-index)))
        (do ((parts (shuffle (copy-sequence 'string target-item)))
             (acc   (make-array 0
                                :element-type 'character
                                :adjustable t
                                :fill-pointer 0)))
            ((or (= (length acc) (length target-item))
                 (not (starts-with-subseq acc
                                          target-item
                                          :test #'char-equal)))
             (progn
               (audio-query target-item)
               (string-equal acc target-item)))
          (awhen (readline
                  (concatenate 'string
                               (format nil +session-prompt+ parts)
                               acc)
                  :num-chars 1)
            (let ((it (if (emptyp it) #\newline (char it 0))))
              (vector-push-extend it acc)
              (update parts (curry #'remove it)
                      :test #'char-equal
                      :count 1))))))))

(define-command listen (&optional ((progress 20) integer))
    (:cmd-listen-s :cmd-listen-l)
  (check-type progress (integer 1))
  (exercise (2 progress 1 0 :exercise-listening)
    (destructuring-bind (type default-form form-index) (car targets)
      (let ((target-item (item-form type default-form form-index)))
        (audio-query target-item :silent-success t)
        (awhen (readline (format nil +session-prompt+ "?"))
          (string-equal target-item it))))))

(define-command crosswd (&optional ((words 12) integer))
    (:cmd-crosswd-s :cmd-crosswd-l)
  (check-type words (integer 1))
  (let* ((helpers
          (remove-duplicates
           (pick-forms 0 words)
           :test (lambda (x y)
                   (string-equal
                    (apply #'item-form x)
                    (apply #'item-form y)))))
         (words (make-hash-table :test #'equal))
         (prompt (format nil +session-prompt+ "?"))
         (filler (substitute-if #\space (constantly t) prompt)))
    (term:print (uie (if helpers
                         :exercise-crossword
                         :not-enough-forms)))
    (dolist (item helpers)
      (let ((target-form (apply #'item-form item)))
        (setf (gethash target-form words)
              (list (substitute-if #\nul (constantly t) target-form)))))
    (flet ((crossable (word i str)
             (let ((crossings (cdr (gethash str words))))
               (awhen (position (char word i) str :test #'char-equal)
                 (not (or (find word crossings :test #'string= :key #'car)
                          (find i crossings :test #'= :key #'cdr)))))))
      (do ((word-list (hash-table-keys words)))
          ((null word-list))
        (let* ((word (pop word-list))
               (indexes (set-difference
                         (iota (length word))
                         (mapcar #'cdr (cdr (gethash word words))))))
          (dolist (i indexes)
            (awhen (find-if (curry #'crossable word i) word-list)
              (push (cons it (position (char word i) it :test #'char-equal))
                    (cdr (gethash word words)))
              (push (cons word i)
                    (cdr (gethash it words))))))))
    (do ((tail (apply #'circular-list helpers) (cdr tail)))
        ((notany (lambda (x) (find #\nul x :test #'char=))
                 (mapcar #'car (cdr (hash-table-values words)))))
      (let* ((item (car tail))
             (word (apply #'item-form item)))
        (destructuring-bind (type default-form form-index) item
          (destructuring-bind (revealed . crossings)
              (gethash word words)
            (when (find #\nul revealed :test #'char=)
              (term:cat-print
               (list
                filler
                (list (substitute #\· #\nul revealed :test #'char=) :arg)
                " — "
                (list (lexeme-name type) :typ)
                " "
                (item-translation type default-form)
                " "
                (form-name type form-index)))
              (let ((input (readline prompt)))
                (unless (emptyp input)
                  (if (string-equal input word)
                      (progn
                        (setf (car (gethash word words)) word)
                        (dolist (crossing crossings)
                          (destructuring-bind (target . index) crossing
                            (setf (char (car (gethash target words)) index)
                                  (char target index))))
                        (audio-query word)
                        (term:print (uie :correct)))
                      (term:print (uie :incorrect))))))))))))

(define-command train ()
    (:cmd-train-s :cmd-train-l)
  (mapc #'perform-command '(trans const listen crosswd)))
