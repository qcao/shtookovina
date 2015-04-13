;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Definition of Russian language for Шτookωвiнα.
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
 "Русский"
 '((:глагол          "глагол"
    :ss-forms        ("инфинитив")
    :form-aspects
    (("И.п." "Р.п." "Д.п." "В.п." "Т.п." "П.п.")
     (" м." " ж." " ср." " мн.")))
   (:междометие      "междометие")
   (:местоимение     "местоимение"
    :form-aspects
    (("И.п." "Р.п." "Д.п." "В.п." "Т.п." "П.п.")
     (" м." " ж." " ср." " мн.")))
   (:наречие         "наречие")
   (:предлог         "предлог")
   (:прилагательное  "прилагательное"
    :form-aspects
    (("И.п." "Р.п." "Д.п." "В.п." "Т.п." "П.п.")
     (" м." " ж." " ср." " мн.")))
   (:союз            "союз")
   (:существительное "существительное"
    :form-aspects
    (("И.п." "Р.п." "Д.п." "В.п." "Т.п." "П.п.")
     (" м." " ж." " ср." " мн.")))
   (:фраза           "фраза")
   (:частица         "частица")
   (:числительное    "числительное"
    :ss-forms        ("количественное" "порядковое"))))

;; This definition may be too difficult to use in practice, since
;; adjectives, nouns, pronouns, and verbs have way too many forms. Probably,
;; you should just learn some basic rules to be able to get these
;; forms. However, not all words are regular in this respect. Edit this file
;; to adjust to your learning strategy.
