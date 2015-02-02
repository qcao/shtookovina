;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; English translation of user interface.
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

;; This is English translation of the user interface. Other translations can
;; be easily written by substitution of corresponding English words and
;; phrases. Some strings are used as plain text (via UIE function), others
;; also contain information about graphic rendition and tildes that will be
;; replaced with arguments of function UIM. To apply some style (see
;; 'default-style.lisp' for list of default styles) enclose text in
;; brackets, and immediately after closing bracket ']', put name of desired
;; style in parenthesis.  For example: [my text](cmd).

(set-ui-language
 "English"
 :arg-parser-failed
 "[Cannot](err) parse [\"~\"](inc) as [~](typ)"
 :available-commands
 "Available Commands:"
 :command
 "Command"
 :command-invalid-call
 "[Cannot](err) call [~](cmd) with these arguments"
 :current-language
 "You are learning [~](arg)"
 :define-audio-query
 "[Cannot](err) play [\"~\"](arg), please define [AUDIO-QUERY](cmd) hook"
 :description
 "Description"
 :failed-audio-query
 "[Cannot](err) find any relevant audio for [\"~\"](arg)"
 :help-command-reminder
 "For information about the command use: [help](cmd) [~](arg)"
 :help-long-desc
 "When invoked without arguments, it prints information about all available
commands. When [COMMAND](arg) argument given, it prints detailed description
of the specific command."
 :help-short-desc
 "Print info about commands."
 :lang-long-desc
 "The [lang](cmd) command prints name of the language that you're learning
in the current Shtookovina session. It's useful for troubleshooting."
 :lang-short-desc
 "Print name of the language you're learning."
 :possible-corrections
 "Possible corrections for [~](cmd):"
 :proposed-audio
 "Proposed audio: [\"~\"](arg)"
 :quit-long-desc
 "Quit the Shtookovina REPL."
 :quit-short-desc
 "Quit the Shtookovina REPL."
 :uncorrectable-command
 "[Cannot](err) perform or correct command [~](cmd)"
 :where "where ")
