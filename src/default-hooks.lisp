;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Default hooks used in Shtookovina, user is welcome to register his/her
;;; own functions.
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

(defparameter *command-recognition-threshold* 1
  "Maximal Damerau-Levenshtein distance between entered command and existing
command that may be presented to user to choose from when there is no exact
match.")

(define-hook :audio-query (filename)
  (term:cat-print (uim :define-audio-query filename)))

(define-hook :successful-audio-query (text)
  (term:cat-print (uim :proposed-audio text)))

(define-hook :failed-audio-query (text)
  (term:cat-print (uim :failed-audio-query text)))

(define-hook :unknown-command (command)
  (flet ((gen-prompt (num)
           (format nil *session-prompt*
                   (coerce (mapcar #'code-char
                                   (iota num :start (char-code #\a)))
                           'string))))
    (let* ((carcmd (car command))
           (options (remove-if (lambda (x)
                                 (> (mksm:damerau-levenshtein
                                     (string-downcase carcmd) x)
                                    *command-recognition-threshold*))
                               *command-list*)))
      (if options
          (progn
            (term:cat-print (uim :possible-corrections carcmd))
            (term:o-list options
                         :index :letter
                         :index-style :arg
                         :item-style :cmd)
            (when-let* ((out (rl:readline :prompt (gen-prompt (length options))
                                          :num-chars 1))
                        (nem (not (emptyp out))))
              (cons (nth (- (char-code (char out 0))
                            (char-code #\a))
                         options)
                    (cdr command))))
          (term:cat-print (uim :uncorrectable-command carcmd))))))
