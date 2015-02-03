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

(defvar +table-cell-width+ 20
  "Cell width that is used when Shtookovina needs to print some table.")

(define-command quit ()
    (:cmd-quit-s :cmd-quit-l)
  (setf *session-terminated* t))

(define-command help (&optional (command string))
    (:cmd-help-s :cmd-help-l)
  (if command
      (print-command-description command)
      (progn
        (term:cat-print (uie :available-commands)
                        :base-style :hdr)
        (terpri)
        (term:table (cons (list (uie :command) (uie :description))
                          (mapcar (lambda (x)
                                    (list x (uie (short-desc-id
                                                  (gethash x *commands*)))))
                                  *command-list*))
                    :header-style :hdr
                    :border-style nil
                    :column-width +table-cell-width+))))

(define-command lang ()
    (:cmd-lang-s :cmd-lang-l)
  (term:cat-print (uim :current-language
                       (name *language*))))

(define-command ui-lang ()
    (:cmd-ui-lang-s :cmd-ui-lang-l)
  (term:cat-print (uim :current-ui-language
                       (get-ui-locale))))

(define-command lexemes ()
    (:cmd-lexemes-s :cmd-lexemes-l)
  (term:cat-print (uie :lexemes)
                  :base-style :hdr)
  (terpri)
  (term:table (cons (list (uie :id) (uie :name))
                    (get-lexemes))
              :header-style :hdr
              :border-style nil
              :column-width +table-cell-width+))

(define-command forms ((lexeme keyword))
    (:cmd-forms-s :cmd-forms-l)
  (if (get-lexeme lexeme)
      (progn
        (term:cat-print (uie :lexeme-forms)
                        :base-style :hdr)
        (terpri)
        (term:table (cons (list (uie :index) (uie :name))
                          (mapcar (lambda (i form)
                                    (list i (if (emptyp form)
                                                (uie :default-form)
                                                form)))
                                  (iota (forms-number lexeme))
                                  (get-forms lexeme)))
                    :header-style :hdr
                    :border-style nil
                    :column-width +table-cell-width+))
      (term:cat-print (uim :no-such-lexeme lexeme))))

(define-command audio ((text string))
    (:cmd-audio-s :cmd-audio-l)
  (audio-query text))

;; (define-command rem ((type keyword) (default-form string))
;;     (:cmd-rem-s :cmd-rem-l)
;;   )

;; (define-command add ((type keyword) (default-form string))
;;     (:cmd-add-s :cmd-add-l)
;;   )

;; (define-command clear ()
;;     (:cmd-clear-s :cmd-clear-l)
;;   )

;; (define-command edit ((type keyword)
;;                       (default-form string)
;;                       (new-form string)
;;                       &optional
;;                       (form-index integer))
;;     (:cmd-edit-s :cmd-edit-l)
;;   )

(define-command dict ((word string))
    (:cmd-dict-s :cmd-dict-l)
  (perform-hook :external-dict
                :args word
                :in-thread t
                :put-into-shell t))

(define-command conj ((verb string))
    (:cmd-conj-s :cmd-conj-l)
  (perform-hook :external-conj
                :args verb
                :in-thread t
                :put-into-shell t))
