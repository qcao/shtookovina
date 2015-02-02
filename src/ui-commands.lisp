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

(define-command quit ()
    (:quit-short-desc :quit-long-desc)
  (setf *session-terminated* t))

(define-command help (&optional (command string))
    (:help-short-desc :help-long-desc)
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
                    :column-width 37))))

(define-command lang ()
    (:lang-short-desc :lang-long-desc)
  (term:cat-print (uim :current-language
                       (name *language*))))
