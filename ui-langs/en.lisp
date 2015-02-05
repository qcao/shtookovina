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
;; phrases. To apply some style (see documentation of CL-ANSI-TERM library)
;; enclose text in brackets, and immediately after closing bracket ']', put
;; name of desired style in parenthesis. For example: [my text](cmd).

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
 :cmd-audio-l
 "Search Shtooka databases, pick and play some audio file that's relevant to
given [TEXT](arg)."
 :cmd-audio-s
 "Play audio that's relevant to given text."
 :cmd-conj-l
 "Show conjugation of given [VERB](arg), possibly with help of an external
program."
 :cmd-conj-s
 "Show conjugation of given verb."
 :cmd-dict-l
 "Show information about (and translation of) given [WORD](arg)."
 :cmd-dict-s
 "Show translation of given word."
 :cmd-forms-l
 "Print table with indexed forms of given [LEXEME](arg). You can get full
list of all defined lexemes with [lexemes](cmd) command."
 :cmd-forms-s
 "Print forms of given lexeme."
 :cmd-help-l
 "When invoked without arguments, it prints information about all available
commands. When [COMMAND](arg) argument given, it prints detailed description
of the specific command."
 :cmd-help-s
 "Print info about commands."
 :cmd-lang-l
 "The [lang](cmd) command prints name of the language that you're learning
in the current Shtookovina session. It's useful for troubleshooting."
 :cmd-lang-s
 "Print name of the language you're learning."
 :cmd-lexemes-l
 "Print a table of all lexemes defined in the actual language."
 :cmd-lexemes-s
 "Print all lexemes and their IDs."
 :cmd-quit-l
 "Quit the Shtookovina REPL."
 :cmd-quit-s
 "Quit the Shtookovina REPL."
 :cmd-ui-lang-l
 "Print name of user interface language."
 :cmd-ui-lang-s
 "Print name of user interface language."
 :current-language
 "You are learning [~](arg)"
 :current-ui-language
 "User interface is in [~](arg)"
 :default-form
 "default form"
 :description
 "Description"
 :failed-audio-query
 "[Cannot](err) find any relevant audio for [\"~\"](arg)"
 :help-command-reminder
 "For information about the command use: [help](cmd) [~](arg)"
 :id
 "ID"
 :index
 "Index"
 :lexemes
 "Defined Lexemes"
 :lexeme-forms
 "Lexeme Forms"
 :name
 "Name"
 :no-such-lexeme
 "[Cannot](err) find definition of the [~](arg) lexeme"
 :possible-corrections
 "Possible corrections for [~](cmd):"
 :proposed-audio
 "Proposed audio: [\"~\"](arg)"
 :uncorrectable-command
 "[Cannot](err) correct command [~](cmd)"
 :where "where ")
