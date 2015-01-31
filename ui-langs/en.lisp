;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Definition of English translation of user interface.
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

(set-ui-language
 "English"
 :where
 "where "
 :arg-parser-failed
 "[Cannot](err) parse [\"~\"](inc) as [~](typ)"
 :cmd-invalid-call
 "[Cannot](err) call [~](cmd) with these arguments"
 :help-cmd-reminder
 "For information about the command use: [help](cmd) [~](arg)"
 :define-audio-query
 "[Cannot](err) play [\"~\"](arg), please define [AUDIO-QUERY](cmd) hook"
 :proposed-audio
 "Proposed audio: [\"~\"](arg)"
 :failed-audio-query
 "[Cannot](err) find any relevant audio for [\"~\"](arg)"
 :quit-short-desc
 "Quit the Shtookovina REPL."
 :quit-long-desc
 "Quit the Shtookovina REPL."
 :help-short-desc
 "Print info about commands."
 :help-long-desc
 "When invoked without arguments, it prints information about all available
commands. When [COMMAND](arg) argument given, it prints detailed description
of the specific command."
 :available-cmds
 "Available Commands:"
 :command
 "Command"
 :description
 "Description"
 :possible-corrections
 "Possible corrections for [~](cmd):"
 :uncorrectable-cmd
 "[Cannot](err) perform or correct command [~](cmd)")
