;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Some utilities that user may find useful when writing hooks and
;;; customization in his/her init-file.
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

;; some utilities

(defvar +url-encoding-table+
  (let ((table (make-array 256 :initial-element nil))
        (other "0123456789-_.~"))
    (dotimes (i 26)
      (setf (aref table (+ 65 i)) (code-char (+ 65 i))
            (aref table (+ 97 i)) (code-char (+ 97 i))))
    (dotimes (i (length other))
      (setf (aref table (char-code (char other i)))
            (char other i)))
    (dotimes (i 256 table)
      (unless (aref table i)
        (setf (aref table i) (format nil "%~2,'0x" i)))))
  "Vector translating bytes to URI-encoded percent-sequences.")

(defun hexify-string (string)
  "URI-encode URL and return the result. If URL is multibyte, it is first
converted to a utf-8 byte string. Each byte corresponding to an allowed
character is left as-is, while all other bytes are converted to a
three-character string (percent-encoding): \"%\" followed by two upper-case
hex digits."
  (with-output-to-string (stream)
    (let ((utf-8 (babel:string-to-octets string :encoding :utf-8)))
      (dotimes (i (length utf-8))
        (princ (aref +url-encoding-table+
                     (aref utf-8 i))
               stream)))))
