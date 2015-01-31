;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; ASDF file for Shtookovina.
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

(asdf:defsystem :shtookovina
  :version      "0.1.0"
  :description  "Lisp based program for language learning"
  :author       "Mark Karpov"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "package")
                 (:file "hooks")
                 (:file "language")
                 (:file "shtooka-db")
                 (:file "dictionary")
                 (:file "ui-language")
                 (:file "session")
                 (:file "ui-commands")
                 (:file "default-hooks")
                 (:file "default-style")
                 (:file "main"))
  :depends-on   (#:alexandria
                 #:anaphora
                 #:trivial-shell
                 #:bordeaux-threads
                 #:split-sequence
                 #:cl-store
                 #:mk-string-metrics
                 #:cl-readline
                 #:cl-ansi-term))
