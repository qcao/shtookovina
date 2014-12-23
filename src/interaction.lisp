;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Basic interaction for Shtookovina. Currently it's OK to use the program
;;; directly from REPL just calling necessary functions.
;;;
;;; Copyright (c) 2014 Mark Karpov
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package #:shtookovina)

(defmacro define-language (lang-name) ; we should think about concept
  nil)

(defmacro select-lang (lang-name)
  nil)

(defmacro interface-lang (lang-name)
  nil)

(defun add-item (type form)
  nil)

(defun rem-item (type form)
  nil)

(defun play-item (form)
  nil)

(defun train ()
  nil)

(defun load-dict ()
  nil)

(defun save-dict ()
  nil)

(defun forms-of (word)
  nil)

;; basic machinery to start the program??
