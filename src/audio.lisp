;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; This module defines functions to play audio recordings of given words.
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

(in-package #:shtk)

(defvar *shtk-audio-dir* nil
  "Path to directory that contains data base of audio recordings. This value
should be set during initialization of Shtookovina session.")

(defvar *shtk-audio-files* nil
  "We need this hash table to get file name of particular recording by
associated text.")

(defun extract-db (index-file) ; well, some corrections here are necessary
  "Helper function for INIT-AUDIO-DB."
  (with-open-file (s index-file :direction :input)
    (flet ((next-line ()
             (aif (read-line s nil)
                  (string-trim " " it)))
           (headerp (str)
             (and (not (emptyp str))
                  (char= (first-elt str) #\[)
                  (char= (last-elt str) #\])
                  (subseq str 1 (1- (length str)))))
           (parse-tag (str)
             (when (find #\= str :test #'char=)
               (split-sequence #\= str))))
      (setf *shtk-audio-files* (make-hash-table :test #'equalp))
      (do ((line (next-line)
                 (next-line))
           actual-file
           alphaidx)
          ((null line))
        (aif (headerp line)
             (progn
               (when (and actual-file alphaidx)
                 (push actual-file
                       (gethash alphaidx *shtk-audio-files*)))
               (setf actual-file it
                     alphaidx nil))
             (awhen (parse-tag line)
               (destructuring-bind (k v) it
                 (when (string= "SWAC_ALPHAIDX" k)
                   (setf alphaidx v)))))))))

(defun init-audio-db ()
  "Sets contents of *SHTK-AUDIO-FILES. This function must be called before
first call of PLAY-ITEM."
  (let ((index-file (merge-pathnames *shtk-audio-dir* "index.tags.txt")))
    (if *shtk-audio-dir*
        (if (probe-file index-file)
            (extract-db index-file)
            (format t "couldn't find file ~s;" index-file))
        (format t "I don't know where you store the audio!"))))

(defun play-item (str)
  "Plays audio file corresponding to given text. If data base contains
several files corresponding to the same text, one of them will be randomly
selected. This function returns given text on success and NIL on failure."
  (awhen (gethash str *shtk-audio-files*)
    (trivial-shell:shell-command
     (format nil "flac -cd \"~a\" | aplay"
             (merge-pathnames *shtk-audio-dir*
                              (random-elt it))))
    str))
