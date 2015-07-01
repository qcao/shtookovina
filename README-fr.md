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
  Common Lisp, l'utilisateur peut définir nouvelles commandes et changer
  absolument tout dans Шτookωвiнα ;

* le programme ne fait pas des hypothèses sur la langue étudiée et ceci
  permet d'ajouter très facilement définitions de nouvelles langues ;

* on peut aussi choisir langue d'interface utilisateur, nouvelles
  traductions sont écrites par duplication de définition actuelle de
  certaine langue d'interface utilisateur et remplacement de liste de
  chaînes ;

* pour des utilisateurs qui ne connaissent pas Common Lisp, il y a un
  assistant qui peut configurer la plupart des paramètres ;

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

6. Vous devez maintenant avoir fichier exécutable de Шτookωвiнα dans la
   répertoire `build/`, vous pouvez l'installer de cette façon :

   ```
   # bash install.sh
   ```

7. C'est fini (vous pouvez utiliser `uninstall.sh` pour désinstaller le
   programme).

## Comment exécuter Шτookωвiнα ?

On peut utiliser Шτookωвiнα pour apprendre plusieurs langues en même
temps. Ainsi, elle doit maintenir dictionnaires distincts et fichiers de
configuration pour chaque langue étudiée. Vous spécifiez la langue avec
l'option `-t` ou `--target` :

```
$ shtk -t en # si vous voulez apprendre anglaise
```

Cette option est obligatoire. Lorsque vous voulez ajouter nouvelles langues,
veuillez vous reporter à
[ISO 639-2](http://www.loc.gov/standards/iso639-2/php/code_list.php) pour
choisir leurs codes à deux caractères, s'il vous plaît.

## Assistant et audio

Maintenant, vous devriez avoir installé Шτookωвiнα. Toutefois, elle doit
être configurée avant de pouvoir l'utiliser. Pour le faire fonctionner, vous
êtes censés écrire code en Common Lisp dans votre fichier de
configuration. Heureusement, nous avons l'assistant qui peut le faire
automatiquement.

![L'assistant](img/wizard.png)

Lorsque vous exécutez Шτookωвiнα pour la première fois, l'assistant est
appelé (sauf si vous exécutez le programme avec l'option
`--no-wizard`). D'abord il vous demande concernant langue d'interface
utilisateur en anglaise (c'est la langue par défaut, Шτookωвiнα utilisera
votre langue préférée, dès que elle la sait).

Deuxièmement, Шτookωвiнα doit savoir où enregistrements sonores sont situés
dans votre système. Le programme utilise base de données de Shtooka Project
et pour la vitesse ils doivent être téléchargés et placés dans seul
répertoire. Vous pouvez obtenir la base de données ici
[http://download.shtooka.net/](http://download.shtooka.net/). N'oubliez pas
de décompresser les archives.

Troisièmement, l'assistant vous demandera comment vous voudriez jouer
audio. Vous devez choisir entre options cotées.

Finalement, pour utiliser `query` et `conj` commandes, vous devez éditer
manuellement certain hameçons dans votre fichier de configuration, puisque
nous ne pouvons pas savoir quel service web vous préférez, et donc nous ne
pouvons pas savoir comment composer URL pour requête.

## Le tutoriel

Шτookωвiнα a un tutoriel interactif intégré qui présente quelques ordres de
base :

![Le tutoriel](img/tutorial.png)

Les sujets suivantes sont couverts :

* Comment chercher nouvelles commandes et explorer caractéristiques du
  programme ?

* Comment obtenir description complète de toutes commandes ?

* Comment ajouter des mots et phrases à votre dictionnaire ?

* Quel type d'exercices puet être utilisé pour apprendre les mots ?

* Comment voir information concernant tout le dictionnaire et information
  détaillée autour de certaines entrées particulières ?

## Exercices

Actuellement, il y a quatre différents exercices :

### Traduction

![Traduction](img/translation.png)

Dans cet exercice, vous obtenez un mot (en la langue de l'interface
utilisateur ou ce que vous apprenez) et quatre traductions
possibles. Choisissez la meilleure traduction.

### Le constructeur de mots (écriture)

![Le constructeur de mots](img/constructor.png)

Vous obtenez une traduction et lettres de un mot, mais elles sont
mélangées. Vous devez entrer correctement le mot.

### Écoute

![Écoute](img/listening.png)

Certain audio pertinent est joué, vous devez reconnaître entrée de
dictionnaire et l'entrer correctement.

### Mots croisés

![Mots croisés](img/crossword.png)

Dans cet exercice, vous ne voyez pas représentation visuelle de mots
croisés. Peu importe que telle représentation peut être dessinée ou
non. Vous faites défiler divers mots et essayez les entrer par leurs
désignations. Si vous ne savez pas un mot vous pouvez le passer. Lorsque
vous entrez un mot correctement, certains lettres d'autres mots coïncidant
avec les lettres du mot entré sont révélées.

## Comment contribuer ?

Nous avons besoin de votre aide. Il y a plusieurs moyens aider le projet :

* définissez votre langue si elle est manquante, voir la répertoire `langs`
  pour obtenir des instructions et des exemples (c'est en anglais
  seulement);

* traduisez l'interface utilisateur, voir la répertoire `ui-langs` pour
  obtenir des instructions et des exemples ;

* traduisez ce fichier `README.md`, ajoutez code à deux caractères de votre
  langue au nom du fichier, comme celui-ci : `README-es.md` (pour espagnol)
  ;

* si vous avez rencontré un bogue,
  [rapporter-le](https://github.com/mrkkrp/shtookovina/issues), s'il vous
  plaît.

## Licence

Droit d'auteur © 2015 Mark Karpov

Distribué sous GNU GPL, version 3.
