;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Wizard helps to set up parameters of Шτookωвiнα generating user
;;; configuration file by asking simple questions.
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

(defun run-and-print (sexp stream)
  "Eval given s expression SEXP and print it to stream STREAM."
  (eval sexp)
  (print sexp stream))

(defun wizard-ui-lang (stream)
  "Ask user about his/her preferred user interface language and write
necessary code to the user's configuration file in order to set this
language on every start."
  (term:print (uie :wizard-ui-lang))
  (do (input)
      ((and input (use-ui-language input))
       (run-and-print `(use-ui-language ,input) stream))
    (when input
      (term:print (uie :wizard-ui-lang-bad)
                  :args input))
    (setf input (readline (format nil +session-prompt+ "?"))))
  (term:print (uie :wizard-ui-lang-ok)
              :args (get-ui-locale)))

(defun wizard-shtooka-dirs (stream)
  "Ask user about directory containing Shtooka audio databases downloaded
from Shtooka website, traverse supplied directory and add code that will
bind special variable *SHTOOKA-DIRS* on every start."
  (flet ((get-dirs (path)
           (remove-if-not #'fad:directory-exists-p
                          (fad:list-directory path))))
    (term:print (uie :wizard-shtooka-dirs))
    (let (dirs)
      (do (input)
          (dirs (run-and-print `(setf *shtooka-dirs* ',dirs) stream))
        (when input
          (term:print (uie :wizard-shtooka-dirs-bad)
                      :args input))
        (setf input (or (readline (format nil +session-prompt+ "?")) "")
              dirs  (get-dirs input)))
      (term:table (cons (uie :wizard-shtooka-dirs-ok) dirs)
                  :border-style nil
                  :margin 2
                  :header-style :hdr
                  :cell-style :typ))))

(defun wizard-audio-query (stream)
  "Determine how to play audio files. Print definition of :AUDIO-QUERY hook
to STREAM."
  (let ((options '("flac -cd ~s | aplay"
                   "mplayer ~s")))
    (term:print (uie :wizard-audio-query))
    (aif (int-select-option options :arg)
         (run-and-print `(define-hook :audio-query (filename)
                           (format nil ,it filename))
                        stream)
         (term:print (uie :wizard-audio-query-manually)))))

(defun wizard-query-ext (stream)
  "Generate example of :QUERY-EXT hook, write it to STREAM, print a message
to *STANDARD-OUTPUT*."
  (run-and-print '(define-hook :query-ext (word)
                   (format nil "icecat -new-tab \"localhost/~a\""
                    (hexify-string word)))
                 stream)
  (term:print (uie :wizard-query-ext)))

(defun wizard-conj-ext (stream)
  "Generate example of :CONJ-EXT hook, write it to STREAM, print a message
to *STANDARD-OUTPUT*."
  (run-and-print '(define-hook :conj-ext (verb)
                   (format nil "icecat -new-tab \"localhost/~a\""
                    (hexify-string verb)))
                 stream)
  (term:print (uie :wizard-conj-ext)))

(defun wizard (stream)
  "Generate user configuration file by asking simple questions. Contents of
the generated file should be sent to OUT (stream)."
  (dolist (fnc (list #'wizard-ui-lang
                     #'wizard-shtooka-dirs
                     #'wizard-audio-query
                     #'wizard-query-ext
                     #'wizard-conj-ext))
    (funcall fnc stream)))
