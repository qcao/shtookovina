;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; This module contains definitions of models of various natural languages.
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

;;; français

(deflang :fr "français"
  (:adj "adjectif"
        ()
        ("m" "f")
        ("" " pl"))
  (:adv "adverbe")
  (:art "article")
  (:cnj "conjonction")
  (:int "interjection")
  (:nom "nom"
        ("m" "f"))
  (:phr "phrase")
  (:prp "préposition")
  (:prn "pronom")
  (:vrb "verbe"
        ("Infinitif" "Participe Passe" "Gérondif") 
        ("(je)" "(tu)" "(il, elle)" "(nous)" "(vous)" "(ils, elles)")
        (" Present de l'indicatif"
         " Futur simple"
         " Imparfait"
         " Present du subjonctif"
         " Imperatif"
         " Conditionnel présent")))

;;; English

(deflang :en "English"
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
        ("Infinitive" "Past Participle")
        ("(I)" "(you)" "(he, she, it)" "(we)" "(they)")
        ("Present Simple"
         "Future Simple"
         "Past Simple"
         "Present Continuous"
         "Future Continuous"
         "Past Continuous"
         "Present Perfect"
         "Future Perfect"
         "Past Perfect")))
