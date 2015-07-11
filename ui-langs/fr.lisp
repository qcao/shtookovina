;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; French translation of user interface.
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

(set-ui-language
 "Français"
 :arg-parser-failed
 "[Impossible](err) de analyser [« ~ »](inc) comme [~](typ)"
 :ask-save-dict
 "Voudriez-vous enregistrer les modifications de votre dictionnaire faites
dans cette session ?"
 :aspect
 "Aspect"
 :aspect-listening
 "Écoute"
 :aspect-translation
 "Traduction"
 :aspect-writing
 "Écriture"
 :available-commands
 "Commandes disponibles :"
 :command
 "Commande"
 :command-invalid-call
 "[Impossible](err) de appeler [~](cmd) avec ces arguments"
 :correct
 "[Bonne réponse !](crc)"
 :cmd-add-l
 "Ajouter nouvel élément de type [TYPE](arg) (voir
[lexemes](cmd)). L'élément aura forme principale [DEFAULT-FORM](arg). Si
l'élément existe déjà dans le dictionnaire, cette commande n'aura pas
d'effet."
 :cmd-add-s
 "Ajouter nouvel élément au dictionnaire."
 :cmd-audio-l
 "Rechercher base de données de Shtooka projet et jouer un fichier audio qui
est pertinent au texte donné [TEXT](arg). Notez que vous pouvez utiliser
[Ctrl-o](typ) à répéter le dernier requête audio."
 :cmd-audio-s
 "Jouter audio qui est pertinent au texte donné."
 :cmd-clear-l
 "Effacer le dictionnaire supprimant tous ses éléments."
 :cmd-clear-s
 "Effacer le dictionnaire."
 :cmd-conj-l
 "Présenter conjugaison du verbe donné [VERB](arg), possiblement avec aide
d'un programme externe."
 :cmd-conj-s
 "Présenter conjugaison de verbe donné."
 :cmd-const-l
 "Le constructeur de mots. Vous obtenez une traduction et lettres d'un mot,
mais elles sont mélangées. Vous devez entrer correctement le mot. L'argument
[PROGRESS](arg), désigne le progrès que vous voudriez faire (comme nombre de
réponses correctes)."
 :cmd-const-s
 "Le constructeur de mots."
 :cmd-crosswd-l
 "Exercice « Mots croisés ». Dans cet exercice, vous ne voyez pas
représentation visuelle de mots croisés. Peu importe que telle
représentation peut être dessinée ou non. Vous faites défiler divers mots et
essayez les entrer par leurs désignations. Si vous ne savez pas un mot vous
pouvez le passer. Lorsque vous entrez un mot correctement, certains lettres
d'autres mots coïncidant avec les lettres du mot entré sont révélées. Cet
exercice utilise [WORDS](arg) mots de votre dictionnaire."
 :cmd-crosswd-s
 "Exercice « Mots croisés »."
 :cmd-dict-l
 "Imprimer information concernant le dictionnaire. Si l'argument
[PREFIX](arg) est donné, présenter information détaillée concernant chaque
élément dans le dictionnaire qui a forme principale commençant avec le
préfixe donné. Autrement, imprimer information générale autour du
dictionnaire complet."
 :cmd-dict-s
 "Imprimer information concernant le dictionnaire."
 :cmd-eform-l
 "Changer forme d'élément spécifié dans le dictionnaire. L'élément est
identifié par son [TYPE](arg) et [DEFAULT-FORM](arg) (forme principale). La
forme choisie à [FORM-INDEX](arg) sera remplacée par [NEW-FORM](arg). Pour
obtenir information concernant les indexes, essayez [forms](cmd) commande."
 :cmd-eform-s
 "Changer forme d'élément spécifié dans le dictionnaire."
 :cmd-etrans-l
 "Changer traduction d'élément dans le dictionnaire. L'élément est identifié
par son [TYPE](arg) et [DEFAULT-FORM](arg) (forme principale)."
 :cmd-etrans-s
 "Changer traduction d'élément dans le dictionnaire."
 :cmd-forms-l
 "Imprimer formes du lexème donné [LEXEME](arg). Vous pouvez obtenir la
liste de tous lexèmes définis avec [lexemes](cmd) commande."
 :cmd-forms-s
 "Imprimer formes de lexème donné."
 :cmd-help-l
 "Lorsque cette commande est appelée sans des arguments, imprimer
information concernant toutes commandes interactives disponibles. Lorsque
l'argument [COMMAND](arg) est donné, imprimer description détaillé de la
commande spécifiée."
 :cmd-help-s
 "Imprimer information autour des commandes interactives."
 :cmd-history-l
 "Imprimer l'histoire de la session. L'argument [ITEMS](arg) désigne combien
éléments de l'histoire doivent être imprimés."
 :cmd-history-s
 "Imprimer l'histoire de la session."
 :cmd-lang-l
 "Imprimer le nom de la langue que vous apprenez."
 :cmd-lang-s
 "Imprimer le nom de la langue que vous apprenez."
 :cmd-learned-l
 "Marquer élément spécifié comme appris. L'élément est identifié par son
[TYPE](arg) et [DEFAULT-FORM](arg) (forme principale)."
 :cmd-learned-s
 "Marquer élément spécifié comme appris."
 :cmd-lexemes-l
 "Imprimer un tableau de tous lexèmes définis dans la langue ciblée."
 :cmd-lexemes-s
 "Imprimer un tableau de tous lexèmes."
 :cmd-listen-l
 "Exercice « Écoute ». Certain audio pertinent est joué, vous devez
reconnaître entrée de dictionnaire et l'entrer correctement. L'argument
[PROGRESS](arg), désigne le progrès que vous voudriez faire (comme nombre de
réponses correctes)."
 :cmd-listen-s
 "Exercice « Écoute »."
 :cmd-query-l
 "Présenter information concernant le mot donné [WORD](arg)."
 :cmd-query-s
 "Présenter information concernant le mot donné."
 :cmd-quit-l
 "Quitter Шτookωвiнα."
 :cmd-quit-s
 "Quitter Шτookωвiнα."
 :cmd-rem-l
 "Supprimer élément spécifié. L'élément est identifié par son [TYPE](arg) et
[DEFAULT-FORM](arg) (forme principale)."
 :cmd-rem-s
 "Supprimer élément spécifié."
 :cmd-reset-l
 "Remettre le progrès de élément spécifie. L'élément est identifié par son
[TYPE](arg) et [DEFAULT-FORM](arg) (forme principale)."
 :cmd-reset-s
 "Remettre le progrès de élément spécifie."
 :cmd-train-l
 "Formation complète. It includes all sorts of exercises : translation,
writing, and audition in right order. It's recommended to use this command
for all training."
 :cmd-train-s
 "Formation complète."
 :cmd-trans-l
 "Exercice « Traduction ». Dans cet exercice, vous obtenez un mot (en la
langue de l'interface utilisateur ou ce que vous apprenez) et quatre
traductions possibles. Choisissez la meilleure traduction. L'argument
[PROGRESS](arg), désigne le progrès que vous voudriez faire (comme nombre de
réponses correctes)."
 :cmd-trans-s
 "Exercice « Traduction »."
 :cmd-ui-lang-l
 "Imprimer le nom de la langue d'interface utilisateur."
 :cmd-ui-lang-s
 "Imprimer le nom de la langue d'interface utilisateur."
 :current-language
 "Vous apprenez [~](arg)"
 :current-ui-language
 "La langue d'interface utilisateur est [~](arg)"
 :default-form
 "forme principale"
 :description
 "Description"
 :dict-cleared
 "Le dictionnaire est dégagé, [~](arg) élément(s) supprimé(s)"
 :dict-entry-header
 "[~](arg), [~](typ) — ~ [[~](arg) %]"
 :dict-general
 "[~](arg) mot(s) dans le dictionnaire, progrès total [~](arg) %"
 :dict-form-changed
 "[~](typ) [~](arg) ~ a été changé"
 :dict-item-added
 "[~](typ) [~](arg) a été ajouté à votre dictionnaire"
 :dict-item-already-exists
 "[Impossible](err) de ajouter [~](typ) [~](arg), parce qu'il existe déjà
dans votre dictionnaire"
 :dict-item-learned
 "[~](typ) [~](arg) a été marqué comme appris"
 :dict-item-removed
 "[~](typ) [~](arg) a été supprimé"
 :dict-item-reset
 "Progrès de [~](typ) [~](arg) a été réinitialisé"
 :dict-no-such-item
 "[Impossible](err) de trouver [~](typ) [~](arg), il n'y a pas de tel élément"
 :dict-trans-changed
 "Traduction de [~](typ) [~](arg) a été changée"
 :exercise-constructor
 "Entrez les lettres du mot (on vous donne les lettres du mot, mais elles
sont mélangées) :"
 :exercise-crossword
 "Faites défiler divers mots et essayez les entrer par leurs
désignations. Si vous ne savez pas un mot vous pouvez le passer (appuyez sur
[Entrée](typ) entrant une ligne vide). Lorsque vous entrez un mot
correctement, certains lettres d'autres mots coïncidant avec les lettres du
mot entré sont révélées. L'exercice continue jusqu'à vous entriez tous les
mots correctement."
 :exercise-listening
 "Certain audio pertinent est joué, vous devez reconnaître entrée de
dictionnaire et l'entrer correctement (appuyez sur [Ctrl-o](typ) pour
écouter audio à nouveau) :"
 :exercise-translation
 "Choisissez bonne traduction du élément donné :"
 :failed-audio-query
 "[Impossible](err) de trouver audio pertinent pour [« ~ »](arg)"
 :help-command-reminder
 "Pour information concernant la commande, essayez : [help](cmd) [~](arg)"
 :incorrect
 "[Mauvaise réponse.](inc)"
 :index
 "Index"
 :lexemes
 "Lexèmes définis"
 :lexeme-forms
 "Les formes du lexème"
 :name
 "Nom"
 :no-such-lexeme
 "[Impossible](err) de trouver la définition du lexème [~](arg)"
 :not-enough-forms
 "Il [n'y a pas assez de mots](err) dans le dictionnaire"
 :possible-corrections
 "Corrections possibles pour [~](cmd) :"
 :progress
 "%"
 :proposed-audio
 "Audio proposé : [« ~ »](arg)"
 :tutorial-0 ;; NEXT
 "[Welcome to Шτookωвiнα](hdr), program that will help you to learn
[~](arg). In Шτookωвiнα you enter commands and their arguments to make
various things happen. This way you can add words to your dictionary and
edit them, run various exercises with these words, and other interesting
things. Every command is thoroughly documented, so the first thing you
should learn about this environment is how to discover new commands and get
description of command you're interested in. [help](cmd) command comes in
handy here. Try to call it without arguments now."
 :tutorial-1
 "[Great!](hdr) As you can see there are quite a few commands available in
Шτookωвiнα. Pick any of them and call [help](cmd) command with name of the
command as an argument, for example [help](cmd) [help](arg)."
 :tutorial-2
 "[Fantastic!](hdr) Now, that you know how to explore the environment, you
can continue on your own, but we would like to show you how to add words to
your dictionary first. Use command [add](cmd) to do so. If you check it with
help of [help](cmd) [add](arg) command, you will see that it takes four
arguments! Understand what to pass to [add](cmd) command and add first word
to your dictionary."
 :tutorial-3
 "[Fine!](hdr) Now add [11](arg) more words of different types. You should
have [12](arg) words in your dictionary to continue with this tutorial. Note
that you can get some statistic with help of [dict](cmd) command. This
command can be used to display statistic about some specific words that
share common prefix. Pass that prefix as first argument to [dict](cmd)
command to use this functionality."
 :tutorial-4
 "[12 words should be enough](hdr) to start our training. Шτookωвiнα has
several exercises to train different aspects of word recognition :
[translation](typ), [writing](typ), and [listening](typ). It even has some
sort of crossword game! You can use specific commands to start any of these
exercises, but we advise you use [train](cmd) command that tries to provide
comprehensive training experience using all these exercise in the right
order. Call [train](cmd) command without arguments."
 :tutorial-5
 "[So, that's how Шτookωвiнα can help you remember words.](hdr) There are
statistic per word that helps Шτookωвiнα determine which words need more
training. Call [dict](cmd) command and see for yourself that you've made
some progress!"
 :tutorial-6
 "[Not bad at all!](hdr) Шτookωвiнα supports other functionality too. You
can teach it how to lookup definition and/or translation of given word in
online dictionaries. You can also reprogram Шτookωвiнα putting some code
into your configuration file. You can even define new commands, etc. This
tutorial has shown you very basic commands and I fear it's come to its
end. To exit the interactive environment use [quit](cmd). Good luck!"
 :tutorial-try-again ;; OKAY ↓
 "[Dommage.](inc) Essayez de nouveau."
 :uncorrectable-command
 "[Impossible](err) de corriger la commande [~](cmd)"
 :unknown-form-query
 "[~](typ) [~](arg) ~ est [inconnu](err), replissez-le pour continuer :"
 :value
 "Valeur"
 :where
 "où "
 :wizard-audio-query
 "Choisissez comment Шτookωвiнα doit jouer fichier FLAC, s'il vous plaît :"
 :wizard-audio-query-manually
 "Bon, définissez le hameçon [:audio-query](typ) vous-même dans votre
fichier de configuration."
 :wizard-conj-ext
 "Éditer l'exemple de le crochet [:conj-ext](typ) dans votre fichier de
configuration pour activer la commande [conj](cmd)."
 :wizard-query-ext
 "Éditer l'exemple de le crochet [:query-ext](typ) dans votre fichier de
configuration pour activer la commande [query](cmd)."
 :wizard-shtooka-dirs
 "Шτookωвiнα doit savoir où enregistrements sonores sont situés dans votre
système. Vous pouvez obtenir la base de données ici
[<http://download.shtooka.net/>](typ). Décompressez les archives et les
placez dans seul répertoire, alors entrez le chemin d'accès au répertoire
ici :"
 :wizard-shtooka-dirs-bad
 "Le répertoire [« ~ »](arg) n'existe pas ou elle est vide, essayez de
nouveau, s'il vous plaît."
 :wizard-shtooka-dirs-ok
 "Les répertoires ajoutés"
 :wizard-ui-lang
 "Choisissez la langue d'interface utilisateur, s'il vous plaît. Entrez deux
lettres, [« en »](arg) par exemple (pour choisir anglaise)."
 :wizard-ui-lang-bad
 "Votre saisie [« ~ »](arg) ne désigne pas quelque langue définie, essayez
de nouveau."
 :wizard-ui-lang-ok
 "Bon, utilisant [~](arg) comme la langue d'interface utilisateur.")
