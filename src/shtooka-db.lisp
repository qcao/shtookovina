;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; This module defines functions to read and parse Shtooka database. It
;;; also provides functions to get file name of relevant audio recording
;;; according to given word of interest.
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

(defvar *shtooka-dir* nil
  "Path to directory that contains data base of audio recordings.")

(defparameter *shtooka-index* "index.tags.txt"
  "Name of index file in directory that contains Shtooka database.")

(defvar *audio-index* nil
  "This hash table allows to lookup names of audio files relevant to given
word (not necessary in its base form). INIT-SHTOOKA-DB reads Shtooka
database index and fills this hash table.")

(defvar *text-index* nil
  "This hash table allows to lookup text corresponding to given filename in
data base.")

(defparameter *target-tag* "SWAC_TEXT"
  "This tag will be used to fill out *AUDIO-INDEX*.")

(defparameter *uniqueness-threshold* 1/100
  "A word is considered auxiliary in current language, if it appears in
this (or greater) part of all texts in data base. Auxiliary words are
deleted from *AUDIO-INDEX* to speed up playback.")

(defun extract-words (str)
  "Split text into words, according to Sthookovina concept of word."
  (split-sequence-if-not #'alpha-char-p str))

(defun init-shtooka-db ()
  "Generate contents of *AUDIO-INDEX* and *TEXT-INDEX*. This function must
be called before first call of PLAY-ITEM."
  (labels ((headerp (str)
             (and (not (emptyp str))
                  (char= (first-elt str) #\[)
                  (char= (last-elt str) #\])
                  (subseq str 1 (1- (length str)))))
           (parse-tag (str)
             (when (find #\= str :test #'char=)
               (split-sequence #\= str)))
           (parse-db (filename)
             (let ((total-num 0))
               (with-open-file (stream filename :direction :input)
                 (setf *audio-index* (make-hash-table :test #'equalp)
                       *text-index*  (make-hash-table :test #'equal))
                 (do ((line (read-line stream nil)
                            (read-line stream nil))
                      actual-file
                      target-tag)
                     ((null line))
                   (aif (headerp line)
                        (progn
                          (when (and actual-file target-tag)
                            (dolist (item (extract-words target-tag))
                              (push actual-file
                                    (gethash item *audio-index*)))
                            (setf (gethash actual-file *text-index*)
                                  target-tag))
                          (setf actual-file it
                                total-num   (1+ total-num)
                                target-tag  nil))
                        (awhen (parse-tag line)
                          (destructuring-bind (k v) it
                            (when (string= *target-tag* k)
                              (setf target-tag v))))))
                 (maphash (lambda (key value)
                            (when (>= (length value)
                                      (* *uniqueness-threshold*
                                         total-num))
                              (remhash key *audio-index*)))
                          *audio-index*)))))
    (if *shtooka-dir*
        (let ((index-file (merge-pathnames *shtooka-dir*
                                           *shtooka-index*)))
          (if (probe-file index-file)
              (parse-db   index-file)
              (perform-hook :could-not-find-shtooka-index
                            :args index-file))
          (perform-hook :shtooka-dir-not-set)))))

(defun play-item (text)
  "Invokes :PLAY-ITEM hook with file name of audio file corresponding to
given TEXT as first argument. TEXT may be word or phrase. If data base
contains several files corresponding to the same text, one of them will be
randomly selected. This function returns text of selected audio file on
success and NIL on failure."
  (flet ((quote-filename (filename)
           (if (find #\space filename :test #'char=)
               (format nil "~s" filename)
               filename))
         (count-items (items)
           (let ((counts (make-hash-table)))
             (hash-table-alist
              (dolist (item items counts)
                (if (gethash item counts)
                    (incf (gethash item counts))
                    (setf (gethash item counts) 1)))))))
    (when-let* ((counts (count-items
                         (mappend (lambda (x) (gethash x *audio-index*))
                                  (extract-words text))))
                (extremum (cdr (extremum counts #'> :key #'cdr)))
                (pretenders (remove-if (curry #'> extremum)
                                       counts
                                       :key #'cdr))
                (selected (car (random-elt pretenders))))
      (perform-hook :play-item
                    :args (merge-pathnames *shtooka-dir*
                                           (quote-filename selected))
                    :in-thread t
                    :put-into-shell t)
      (gethash selected *text-index*))))

;;; --- testing ---

(setf *shtooka-dir* "/home/mark/Downloads/fr/flac/")

(register-hook :play-item
               (lambda (str)
                 (format nil "flac -cd ~a | aplay" str)))

;; todo:
;; * restarts in some cases (?)
;; * some guessing when system cannot find some words (damerau-levenshtein)
;; * how to play auxiliary words?
