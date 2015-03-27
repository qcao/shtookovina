;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; This is the place where we put it all together.
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

(defvar +shtookovina-version+ "[Shtookovina](hdr) [0.1.0](arg)"
  "Version of the program.")

(defvar +shtookovina-ui-langs+
  (make-pathname :directory '(:absolute "usr" "share" "shtookovina" "ui-langs"))
  "Where to search for definitions of user interface languages.")

(defvar +shtookovina-langs+
  (make-pathname :directory '(:absolute "usr" "share" "shtookovina" "langs"))
  "Where to search for definitions of target languages.")

(defvar +shtookovina-local+
  (merge-pathnames (make-pathname :directory '(:relative ".shtookovina"))
                   (user-homedir-pathname))
  "Local directory where user's personal data is stored.")

(defvar +config-pathname+ (make-pathname :name "config" :type "lisp")
  "Path name of configuration file.")

(defvar +dict-pathname+ (make-pathname :name "dict" :type "bin")
  "Path name of dictionary file.")

(defvar +gnu-glp-notice+
  "[Shtookovina](hdr) - program to help learn natural languages

Copyright (c) 2015 Mark Karpov

Shtookovina is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Shtookovina is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see [<http://www.gnu.org/licenses/>](typ)."
  "A notice about license of the program (GNU GLP, version 3).")

(defparameter *virgin-user* nil
  "This variable is bound to non-NIL value if the current user runs the
program for the first time (at least with specified target language).")

(opts:define-opts
  (:name :target
   :description "Set target language LANG. This option is a must."
   :short #\t
   :long "target"
   :arg-parser #'identity
   :meta-var "LANG")
  (:name :version
   :description "Print version of the program."
   :long "version")
  (:name :license
   :description "Print license of the program."
   :long "license")
  (:name :help
   :description "Print description of command line options."
   :long "help"))

(defun unknown-option (condition)
  "What to do if user has passed unknown command line option. CONDITION is
raised condition."
  (term:print "[Warning](err): \"~\" option is unknown"
              :args (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun local-target-pathname (target-lang)
  "Return local target pathname -- combination of shtookovina local
directory and directory named after target language."
  (merge-pathnames (make-pathname :directory (list :relative target-lang))
                   +shtookovina-local+))

(defun load-lisp (filename)
  "Load given file FILENAME, evaluating it in SHTOOKOVINA package."
  (let ((*package* (find-package 'shtookovina)))
    (load filename
          :if-does-not-exist nil
          :verbose nil)))

(defun use-ui-language (lang)
  "Load file corresponding to specified language LANG, changing language of
user interface. Return T on success and NIL on failure."
  (load-lisp
   (make-pathname :name lang
                  :type "lisp"
                  :defaults +shtookovina-ui-langs+)))

(defun load-target-lang (lang)
  "Load file corresponding to specified language LANG, setting target
language for training. Retrun T on success and NIL on failure."
  (load-lisp
   (make-pathname :name lang
                  :type "lisp"
                  :defaults +shtookovina-langs+)))

(defun load-config (local-target)
  "Load configuration file located in LOCAL-TARGET directory."
  (load-lisp (merge-pathnames +config-pathname+ local-target)))

(defun load-dict (local-target)
  "Load dictionary file located in LOCAL-TARGET directory."
  (load-dictionary (merge-pathnames +dict-pathname+ local-target)))

(defun ask-and-save-dict (local-target)
  "Ask if user wants to save changes in his/her dictionary and save them."
  (term:print (uie :ask-save-dict))
  (when (int-yes-or-no t)
    (save-dictionary (merge-pathnames +dict-pathname+ local-target))))

(defun main (&rest rest)
  "Entry point of Shtookovina program."
  (declare (ignore rest))
  (handler-case
      (multiple-value-bind (options free-args)
          (handler-case
              (handler-bind ((opts:unknown-option #'unknown-option))
                (opts:get-opts))
            (opts:missing-arg (condition)
              (error (format nil "option ~s needs an argument"
                             (opts:option condition))))
            (opts:arg-parser-failed (condition)
              (error (format nil "cannot parse ~s as argument of ~s"
                             (opts:raw-arg condition)
                             (opts:option  condition)))))
        (declare (ignore free-args))
        (setf *random-state* (make-random-state t))
        (unless (use-ui-language "en")
          (error "couldn't load default user language"))
        (when (getf options :version)
          (term:print +shtookovina-version+)
          (return-from main))
        (when (getf options :license)
          (term:print +gnu-glp-notice+)
          (return-from main))
        (when (getf options :help)
          (term:print (opts:describe
                       :prefix "shtk [options]"))
          (return-from main))
        (let ((target-lang (getf options :target)))
          (unless target-lang
            (error "you must specify target language, for example \"en\""))
          (unless (load-target-lang target-lang)
            (error (format nil "cannot find definition for ~s"
                           target-lang)))
          (let ((local-target (local-target-pathname target-lang)))
            (ensure-directories-exist local-target)
            (load-config local-target)
            (load-dict local-target)
            (init-shtooka-db)
            (rl:register-function :complete #'session-std-complete)
            (rl:bind-keyseq "\\C-o" #'repeat-audio-query)
            (session)
            (ask-and-save-dict local-target))))
    (error (condition)
      (term:print "[Fatal](err): ~" :args (princ-to-string condition)))))
