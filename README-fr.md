# Шτookωвiнα

*Шτookωвiнα est en phase d'évaluation (bêta). Essayez-la, s'il vous plaît !*

Ce programme est destiné à être utile pour des apprenants de langues
étrangères. Voici quelques caractéristiques intéressantes de Шτookωвiнα pour
un démarrage rapide :

* le programme utilise enregistrements sonores de
  [Shtooka Project](http://shtooka.net/), ils ont été enregistrés par
  locuteurs bénévoles, plutôt que des robots ;

* ce programme a interface de ligne de commande, mais il essaye d'être
  convival avec l'aide de
  [cl-readline](https://github.com/mrkkrp/cl-readline) et de
  [cl-ansi-term](https://github.com/mrkkrp/cl-ansi-term) ;

* il y a différentes exercices qui vous aident à travailler sur divers
  aspects d'un mot : traduction, écriture et écoute ;

* le programme a aussi «mots croisés» exercice ;

* c'est bien modifiable par l'intermédiaire d'un fichier de configuration en
  Common Lisp, le utilisateur peut définir nouvelles commandes et changer
  absolument tout dans Шτookωвiнα ;

* le programme ne fait pas des hypothèses sur la langue étudiée et ceci
  permet d'ajouter très facilement définitions de nouvelles langues ;

* on peut aussi choisir langue d'interface utilisateur, nouvelles
  traductions sont écrites par duplication de définition actuelle de
  certaine langue d'interface utilisateur et replaçant de liste de chaînes ;

* pour des utilisateurs qui ne connaissent pas Common Lisp, il y a un
  assistant qui peut configurer la plupart des paramètres;

* il y a un tutoriel interactif intégré.

## Compilation et installation

Le processus d'installation est très simple :

1. Installez [SBCL](http://www.sbcl.org/), Шτookωвiнα est écrite en Common
   Lisp standard et elle ne mise pas sur caractéristiques spécifiques de
   SBCL, mais seule version compilée avec SBCL est testée ;

2. Installez [Quicklisp](http://www.quicklisp.org/) pour obtenir
   automatiquement toutes les dépendances, bref, voici comment procéder :

   ```
   $ curl -O http://beta.quicklisp.org/quicklisp.lisp
   $ sbcl --load quicklisp.lisp
   * (quicklisp-quickstart:install)
   * (quit)
   ```

3. Installez [Buildapp](http://www.xach.com/lisp/buildapp/), préférez la
   version la plus récente, s'il vous plaît :

   ```
   $ git clone https://github.com/xach/buildapp.git
   $ cd buildapp
   # make install
   ```

4. Téléchargez ou clonez dépôt de Шτookωвiнα :

   ```
   $ git clone https://github.com/mrkkrp/shtookovina.git
   ```

5. `cd` dans le répertoire et compilez le programme avec `make` :

   ```
   $ cd shtookovina
   $ make
   ```

6. Vous devez maintenant avoir fichier exécutable de Шτookωвiнα dans
   `build/shtk`, vous pouvez l'installer de cette façon :

   ```
   # bash install.sh
   ```

7. C'est fini (vous pouvez utiliser `uninstall.sh` pour désinstaller le
   programme).

## Comment exécuter Шτookωвiнα ?

On peut utiliser Шτookωвiнα pour apprendre plusieurs langues en même
temps. Ainsi, elle doit maintenir dictionnaires distincts et fichiers de
configuration pour chaque langue étudiée. Vous spécifiez la langue avec la
option `-t` ou `--target` :

```
$ shtk -t en # si vous voulez apprendre anglaise
```

Cette option est obligatoire. Lorsque vous voulez ajouter nouvelles langues,
veuillez vous reporter à
[ISO 639-2](http://www.loc.gov/standards/iso639-2/php/code_list.php) pour
choisir leurs codes à deux caractères, s'il vous plaît.

## Assistant et audio

Now, you hopefully have installed Шτookωвiнα. However, it should be
configured before you can use it. Initially Шτookωвiнα was planned as a
hardcore program for Lisp geeks, but later we decided to make it more
user-friendly, so more people could use it. To make Шτookωвiнα work you're
supposed to write some Lisp in your configuration file setting some
variables and defining some hooks. Fortunately, we have wizard that can do
this automatically.

![Шτookωвiнα Wizard](img/wizard.png)

When you start Шτookωвiнα for the first time, wizard will be called (unless
you start it with `--no-wizard` option). First it asks about interface
language in English (it's default language, Шτookωвiнα will use your
preferred language as soon as it knows which one it is).

Second, Шτookωвiнα needs to know where audio databases are located in your
system. Шτookωвiнα uses databases from Shtooka project and for speed they
should be downloaded and placed in one directory. You can download the
databases from
[http://download.shtooka.net/](http://download.shtooka.net/). Don't forget
to uncompress the archives.

Third, the wizard will ask your how you would like to play audio files
(Shtooka project uses FLAC files). You should choose between listed options.

Finally, to use `query` and `conj` commands (open web-page with description
of given word or conjugation of given verb) you currently need to manually
edit relevant hooks in your configuration file, since we cannot know which
web-service your prefer to use for this task, and thus we cannot know how to
transform the word into URL.

## Tutorial

Шτookωвiнα has built-in interactive tutorial that shows basic commands:

![Шτookωвiнα Tutorial](img/tutorial.png)

The following topics are covered:

* How to find new commands and explore features of the program?

* How to get full description of any command?

* How to add words and phrases to user's dictionary?

* What sort of exercises can be used to remember these words?

* How to display information about the whole dictionary and detailed
  information about some specific entries?

## Exercises

Currently there are four different exercises:

### Translation

![Translation Exercise](img/translation.png)

In this exercise you are given a word (either in interface language or
target language) and four possible translations. You should select correct
one.

### Word Constructor (writing)

![Word Constructor Exercise](img/constructor.png)

You are given translation and letters of the word, but they are
shuffled. You need to enter the word correctly, letter by letter.

### Listening

![Listening Exercise](img/listening.png)

Some relevant audio is played to you, you should recognize dictionary item
and enter it correctly.

### Crossword

![Crossword Exercise](img/crossword.png)

This crossword doesn't show you visual representation of the crossing
words. It doesn't really matter if such representation can be drawn or
not. You cycle through various words and try to enter them correctly by
their description. If you don't know a word, you can skip it. If you enter a
word correctly, some letters of other words that coincide with the letters
of the entered word are revealed.

## How to contribute?

We need your help. There are many ways to help the project:

* define your language if it's missing, see `langs` directory for
  instructions and examples;

* translate user interface, see `ui-langs` directory for instructions and
  examples;

* translate this `README.md` file, append two-letter code of your language
  to the filename like this `README-fr.md` (for French);

* if you have encountered a bug, please
  [open an issue](https://github.com/mrkkrp/shtookovina/issues).

## License

Droit d'auteur © 2015 Mark Karpov

Distribué sous GNU GPL, version 3.
