;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Definition of French language for Шτookωвiнα.
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
 "Français"
 '((:adjectif     "adjectif"
    :form-aspects (("m" "f") ("" " pl")))
   (:adverbe      "adverbe")
   (:article      "article")
   (:conjonction  "conjonction")
   (:interjection "interjection")
   (:nom          "nom"
    :form-aspects (("" " pl")))
   (:phrase       "phrase")
   (:pronom       "pronom")
   (:préposition  "préposition")
   (:verbe        "verbe"
    :ss-forms
    ("Infinitif" "Participe Passe" "Gérondif")
    :form-aspects
    (("Présent de l'indicatif"
      "Futur simple"
      "Imparfait"
      "Présent du subjonctif")
     (" (je)" " (tu)" " (il)" " (nous)" " (vous)" " (ils/elles)")))))
