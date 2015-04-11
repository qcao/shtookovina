;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Default hooks used in Шτookωвiнα, user is welcome to register his/her
;;; own functions.
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

(define-hook :successful-audio-query (text)
  (term:print (uie :proposed-audio)
              :args text))

(define-hook :failed-audio-query (text)
  (term:print (uie :failed-audio-query)
              :args text))

(define-hook :unknown-command (command)
  (awhen (int-correct-command (car command))
    (cons it (cdr command))))

(define-hook :tutorial-hook ()
  (awhen *tutorial-on*
    (tutorial)))
