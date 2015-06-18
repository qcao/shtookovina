;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; This is the place where we put it all together.
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

(defvar +shtookovina-version+ "[Шτookωвiнα](hdr) [0.1.0](arg)"
  "Version of the program.")

(defvar +shtookovina-ui-langs+
  (make-pathname :directory '(:absolute "usr" "share" "shtookovina" "ui-langs"))
  "Where to search for definitions of user interface languages.")

(defvar +shtookovina-langs+
  (make-pathname :directory '(:absolute "usr" "share" "shtookovina" "langs"))
  "Where to search for definitions of target languages.")

(defvar +shtookovina-local+
  (make-pathname :directory '(:relative ".shtookovina"))
  "Local directory where user's personal data is stored. Use
`shtookovina-local' function to get absolute path at run time.")

(defvar +config-pathname+ (make-pathname :name "config" :type "lisp")
  "Path name of configuration file.")

(defvar +dict-pathname+ (make-pathname :name "dict" :type "bin")
  "Path name of dictionary file.")

(defvar +gnu-glp-notice+
  "[Шτookωвiнα](hdr) - program to help learn natural languages

Copyright © 2015 Mark Karpov

Шτookωвiнα is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

Шτookωвiнα is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see [<http://www.gnu.org/licenses/>](typ)."
  "A notice about license of the program (GNU GLP, version 3).")

(opts:define-opts
  (:name :help
   :description "Print description of command line options."
   :long "help")
  (:name :license
   :description "Print license of the program."
   :long "license")
  (:name :version
   :description "Print version of the program."
   :long "version")
  (:name :target
   :description "Set target language LANG. This option is a must."
   :short #\t
   :long "target"
   :arg-parser #'identity
   :meta-var "LANG")
  (:name :no-wizard
   :description "Don't invoke wizard."
   :long "no-wizard")
  (:name :no-tutorial
   :description "Omit tutorial."
   :long "no-tutorial"))

(defun unknown-option (condition)
  "What to do if user has passed unknown command line option. CONDITION is
the raised condition."
  (term:print "[Warning](err): \"~\" option is unknown"
              :args (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun shtookovina-local ()
  "Return local directory where user's personal data is stored."
  (merge-pathnames +shtookovina-local+ (user-homedir-pathname)))

(defun local-target-pathname (target-lang)
  "Return local target pathname — combination of shtookovina local directory
and directory named after target language TARGET-LANG."
  (merge-pathnames (make-pathname :directory (list :relative target-lang))
                   (shtookovina-local)))

(defun load-lisp (filename)
  "Load given file FILENAME, evaluating it in `shtookovina' package. Return
T on success and NIL on failure."
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

(defun load-config (local-target &optional no-wizard)
  "Load configuration file located in LOCAL-TARGET directory. Retrun T on
success and NIL on failure. If NO-WIZARD is not NIL, don't start wizard even
if the specified configuration file does not exist."
  (let ((filename (merge-pathnames +config-pathname+ local-target)))
    (unless (or (load-lisp filename) no-wizard)
      (with-open-file (stream filename
                              :direction :output
                              :if-exists :rename)
        (wizard stream)))))

(defun load-dict (local-target)
  "Load dictionary file located in LOCAL-TARGET directory. Retrun T on
success and NIL on failure."
  (load-dictionary (merge-pathnames +dict-pathname+ local-target)))

(defun ask-and-save-dict (local-target)
  "Ask if user wants to save changes in his dictionary and save them."
  (term:print (uie :ask-save-dict))
  (when (int-yes-or-no t)
    (save-dictionary (merge-pathnames +dict-pathname+ local-target))))

(defun main (&rest rest)
  "Entry point of Шτookωвiнα program."
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
          (term:print
           (opts:describe
            :prefix "Шτookωвiнα — program to help learn natural languages"
            :usage-of "shtk"))
          (return-from main))
        (let ((target-lang (getf options :target)))
          (unless target-lang
            (error "you must specify target language, for example \"en\""))
          (unless (load-target-lang target-lang)
            (error (format nil "cannot find definition for ~s"
                           target-lang)))
          (let ((local-target (local-target-pathname target-lang)))
            (ensure-directories-exist local-target)
            (term:print +shtookovina-version+)
            (load-config local-target (getf options :no-wizard))
            (setf *tutorial-on* (not (or (load-dict local-target)
                                         (getf options :no-tutorial))))
            (init-shtooka-db)
            (rl:register-function :complete #'session-std-complete)
            (rl:bind-keyseq "\\C-o" #'repeat-audio-query)
            (session)
            (ask-and-save-dict local-target))))
    (error (condition)
      (term:print "[Fatal](err): ~" :args (princ-to-string condition)))))
