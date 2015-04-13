;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Definition of English language for Шτookωвiнα.
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

(set-language
 "English"
 '((:adjective    "adjective")
   (:adverb       "adverb")
   (:article      "article")
   (:conjunction  "conjunction")
   (:interjection "interjection")
   (:noun         "noun")
   (:numeral      "numeral"
    :ss-forms     ("(cardinal)" "(ordinal)"))
   (:phrase       "phrase")
   (:preposition  "preposition")
   (:pronoun      "pronoun")
   (:verb         "verb"
    :ss-forms     ("Infinitive" "Past" "Past Participle"))))
