;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; All user-friendly commands live here. It includes commands to start some
;;; exercise, get information or statistics, open external resource (such as
;;; generated URL in your favorite web-browser, so you can check an online
;;; dictionary), and more. See how the commands are defined, and you will be
;;; able to add your own commands easily.
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

(defvar +table-cell-width+ 10
  "Cell width that is used when Shtookovina needs to print some table.")

(defvar +dict-table-width+ '(10 30 30 10)
  "Cell width that is used for tables printed by DICT command.")

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
                    :column-width +table-cell-width+))))

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
  (term:print (uie :lexemes)
              :base-style :hdr)
  (terpri)
  (term:table (cons (list (uie :id) (uie :name))
                    (get-lexemes))
              :cell-style '(:arg :default)
              :header-style :hdr
              :border-style nil
              :column-width +table-cell-width+))

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
                    :column-width +table-cell-width+))
      (term:print (uie :no-such-lexeme)
                  :args lexeme)))

(define-command audio ((text string))
    (:cmd-audio-s :cmd-audio-l)
  (audio-query text))

(define-command add ((type keyword)
                     (default-form string)
                     (translation string))
    (:cmd-add-s :cmd-add-l)
  (if (add-dictionary-item type default-form translation)
      (term:print (uie :dict-item-added)
                  :args (list type default-form))
      (term:print (uie :dict-item-already-exists)
                  :args (list type default-form))))

(define-command rem ((type keyword)
                     (default-form string))
    (:cmd-rem-s :cmd-rem-l)
  (if (rem-dictionary-item type default-form)
      (term:print (uie :dict-item-removed)
                  :args (list type default-form))
      (term:print (uie :dict-no-such-item)
                  :args (list type default-form))))

(define-command clear ()
    (:cmd-clear-s :cmd-clear-l)
  (term:print (uie :dict-cleared)
              :args (clear-dictionary)))

(define-command learned ((type keyword)
                         (default-form string))
    (:cmd-learned-s :cmd-learned-l)
  (if (item-mark-as-learned type default-form)
      (term:print (uie :dict-item-learned)
                  :args (list type default-form))
      (term:print (uie :dict-no-such-item)
                  :args (list type default-form))))

(define-command reset ((type keyword)
                       (default-form string))
    (:cmd-reset-s :cmd-reset-l)
  (if (item-reset-progress type default-form)
      (term:print (uie :dict-item-reset)
                  :args (list type default-form))
      (term:print (uie :dict-no-such-item)
                  :args (list type default-form))))

(define-command eform ((type keyword)
                       (default-form string)
                       (new-form string)
                       &optional
                       (form-index integer))
    (:cmd-eform-s :cmd-eform-l)
  (let ((form-index (or form-index 0)))
    (if (edit-dictionary-item-form type
                                   default-form
                                   new-form
                                   form-index)
        (term:print (uie :dict-form-changed)
                    :args (list type default-form form-index))
        (term:print (uie :dict-no-such-item)
                    :args (list type default-form)))))

(define-command etrans ((type keyword)
                        (default-form string)
                        (new-translation string))
    (:cmd-etrans-s :cmd-etrans-l)
  (if (edit-dictionary-item-translation type
                                        default-form
                                        new-translation)
      (term:print (uie :dict-trans-changed)
                  :args (list type default-form))
      (term:print (uie :dict-no-such-item)
                  :args (list type default-form))))

(define-command dict (&optional (prefix string))
    (:cmd-dict-s :cmd-dict-l)
  (if prefix
      (flet ((print-item (type item)
               (term:print (uie :dict-entry-header)
                           :args (list (aref (forms item) 0)
                                       type
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
                                                 (aref (forms item) i)
                                                 (item-form-progress item i)))
                                         (iota (forms-number type))
                                         (get-forms type)))
                           :cell-style '(:arg :default :default :arg)
                           :header-style :hdr
                           :border-style nil
                           :column-width +dict-table-width+)
               (terpri)))
        (maphash (lambda (key item)
                   (destructuring-bind (type . default-form) key
                     (when (starts-with-subseq prefix default-form
                                               :test #'char-equal)
                       (print-item type item))))
                 *dictionary*))
      (term:print (uie :dict-general)
                  :args (list (dictionary-item-count)
                              (dictionary-progress)))))

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
