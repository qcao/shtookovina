;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Definition of English language for Shtookovina.
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

(define-language :en "English"
  (:adj "adjective")
  (:adv "adverb")
  (:art "article")
  (:cnj "conjunction")
  (:int "interjection")
  (:nom "noun")
  (:phr "phrase")
  (:prp "preposition")
  (:prn "pronoun")
  (:vrb "verb"
        :ss-forms
        ("Infinitive" "Past Participle")
        :form-aspects
        (("(I) " "(you) " "(it) " "(we) " "(they) ")
         ("Present Simple"
          "Future Simple"
          "Past Simple"
          "Present Continuous"
          "Future Continuous"
          "Past Continuous"
          "Present Perfect"
          "Future Perfect"
          "Past Perfect"))))
