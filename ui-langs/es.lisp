;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Spanish translation of user interface.
;;;
;;; Copyright © 2015 Mark Karpov, Nicolás Bonader
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
 "Español"
 :arg-parser-failed
 "[No se pudo](err) parsear [\"~\"](inc) como [~](typ)"
 :ask-save-dict
 "¿Quieres guardar los cambios hechos en tu diccionario durante esta
sesión?"
 :aspect
 "Aspecto"
 :aspect-listening
 "Escucha"
 :aspect-translation
 "Traducción"
 :aspect-writing
 "Escritura"
 :available-commands
 "Comandos disponibles:"
 :command
 "Comando"
 :command-invalid-call
 "[No se pudo](err) llamar a [~](cmd) con estos argumentos"
 :correct
 "[¡Respuesta Correcta!](crc)"
 :cmd-add-l
 "Añade un nuevo ítem al diccionario del tipo [TYPE](arg) (ver [lexemes](cmd)).
El ítem tendrá por defecto la forma [DEFAULT-FORM](arg). Si el ítem ya
existe en el diccionario, este comando no tiene efectos."
 :cmd-add-s
 "Añadir un nuevo ítem al diccionario"
 :cmd-audio-l
 "Busca bases de datos Shtooka, elige y reproduce algún archivo de audio
relevante para [TEXT](arg). Puedes usar [Ctrl-o](typ) para repetir la última
consulta de audio."
 :cmd-audio-s
 "Reproduce audio relevante para un texto dado"
 :cmd-clear-l
 "Vacía el diccionario borrando todos sus ítems."
 :cmd-clear-s
 "Vacía el diccionario"
 :cmd-conj-l
 "Muestra la conjugación de [VERB](arg), posiblemente con ayuda de un
programa externo."
 :cmd-conj-s
 "Mostrar la conjugación de un verbo dado"
 :cmd-const-l
 "Ejercicio constructor de palabra. Tienes la traducción y las letras de la
palabra, pero éstas estan desordenadas. Debes ingresar la palabra
correctamete, letra por letra. El argumento [PROGRESS](arg) muestra el
progreso que te gustaría alcanzar."
 :cmd-const-s
 "Ejercicio constructor de palabra"
 :cmd-crosswd-l
 "Ejercicio de crucigrama. Este crucigrama no te muestra una representación
visual de las palabras. En realidad no importa si tal representación puede
ser dibujada o no. Recorres varias palabras e intentas ingresarlas
correctamente según su descripción. Si no conoces una palabra, puedes
saltearla. Si ingresas una palabra correctamente, algunas letras de otras
palabras que coincidan con las letras de la palabra ingresada serán
reveladas. Este ejercicio toma [WORDS](arg) palabras de tu diccionario."
 :cmd-crosswd-s
 "Ejercicio de crucigrama"
 :cmd-dict-l
 "Imprime información sobre el diccionario. Si el argumento [PREFIX](arg) es
dado, muestra información detallada acerca de cada ítem del diccionario cuya
forma (indizada en cero) por defecto comience con el prefijo dado. En caso
contrario imprime información acerca de todo el diccionario."
 :cmd-dict-s
 "Imprime información acerca del diccionario"
 :cmd-eform-l
 "Edita un ítem del diccionario cambiando una de sus formas. El ítem a
cambiar es identificado por [TYPE](arg) y [DEFAULT-FORM](arg). La forma
seleccionada en [FORM-INDEX](arg) es reemplazada por [NEW-FORM](arg). Para
obtener información acerca de la forma de los índices escriba [forms](cmd)
comando."
 :cmd-eform-s
 "Cambiar la forma de un ítem del diccionario"
 :cmd-etrans-l
 "Edita un ítem del diccionario cambiando su traducción. El ítem a cambiar
es es identificado por [TYPE](arg) y [DEFAULT-FORM](arg)."
 :cmd-etrans-s
 "Cambiar la traducción de un ítem del diccionario"
 :cmd-forms-l
 "Imprime una tabla con las formas indexadas de un [LEXEME](arg)
dado. También puedes obtener una lista de todos los lexemas con el comando
[lexemes](cmd)."
 :cmd-forms-s
 "Imprimir formas de un lexema"
 :cmd-help-l
 "Cuando es invocado sin argumentos, imprime información sobre todos los
comandos disponibles. Cuando el argumento [COMMAND](arg) es dado, imprime
una descripción detallada del comando en particular."
 :cmd-help-s
 "Imprimir información sobre comandos"
 :cmd-history-l
 "Imprime el historial de sesión actual. El argumento indica cuántos ítems
del historial deben ser impresos."
 :cmd-history-s
 "Imprimir historial"
 :cmd-lang-l
 "El comando [lang](cmd) imprime el nombre del idioma que estás aprendiendo
el la sesión actual de Шτookωвiнα. Es útil para resolver problemas."
 :cmd-lang-s
 "Imprimir el nombre del idioma que estás aprendiendo"
 :cmd-learned-l
 "Marcar como aprendido un ítem en particular del diccionario. El ítem del
diccionario es identificado por su [TYPE](arg) y [DEFAULT-FORM](arg)."
 :cmd-learned-s
 "Marcar un ítem en particular como aprendido"
 :cmd-lexemes-l
 "Imprime una tabla de todos los lexemas definidos del idioma."
 :cmd-lexemes-s
 "Imprimir todos los lexemas"
 :cmd-listen-l
 "Ejercicio de comprensión auditiva. Algunos audios se reproducen,
debes reconocer un ítem del diccionario e ingresarlo correctamente. El
argumento [PROGRESS](arg) indica el progreso que te gustaría alcanzar."
 :cmd-listen-s
 "Ejercicio de comprensión auditiva"
 :cmd-query-l
 "Muestra información (y su traducción) acerca de una palabra [WORD](arg)
dada."
 :cmd-query-s
 "Mostrar traducción de una palabra"
 :cmd-quit-l
 "Sale de Шτookωвiнα REPL."
 :cmd-quit-s
 "Salr de Шτookωвiнα REPL"
 :cmd-rem-l
 "Borra un ítem en particular del diccionario. El ítem a borrar es
identificado por su [TYPE](arg) y [DEFAULT-FORM](arg)."
 :cmd-rem-s
 "Borrar un ítem en particular del diccionario"
 :cmd-reset-l
 "Reestablece el progreso de un ítem en particular del diccionario. El ítem
es identificado por sus [TYPE](arg) y [DEFAULT-FORM](arg)."
 :cmd-reset-s
 "Reestablecer el progreso de un ítem en particular del diccionario"
 :cmd-train-l
 "Entrenamiento integral. Incluye toda clase de ejercicios: traducción,
escritura y escucha en el orden correcto. Se recomienda usar este comando
para todo entrenamiento."
 :cmd-train-s
 "Entrenamiento integral"
 :cmd-trans-l
 "Éste es un ejercicio de traducción. En este ejercicio tienes una
palabra (en el idioma de la interfaz o el que estás aprendiendo) y cuatro
posible traducciones. Debes seleccionar la correcta. El argumento
[PROGRESS](arg) indica qué progreso te gustaría alcanzar."
 :cmd-trans-s
 "Ejercicio de traducción"
 :cmd-ui-lang-l
 "Imprime el idioma de la interfaz de usuario."
 :cmd-ui-lang-s
 "Imprimir el idioma de la interfaz de usuario"
 :current-language
 "Estás aprendiendo [~](arg)"
 :current-ui-language
 "La interfaz de usuario está en [~](arg)"
 :default-form
 "forma por defecto"
 :description
 "Descripción"
 :dict-cleared
 "El diccionario se ha vaciado, [~](arg) ítem(s) borrados"
 :dict-entry-header
 "[~](arg), [~](typ) — ~ [[~](arg) %]"
 :dict-general
 "[~](arg) palabra(s) en el diccionario, progreso general de [~](arg) %"
 :dict-form-changed
 "Cambiado [~](typ) [~](arg) ~"
 :dict-item-added
 "[~](typ) [~](arg) añadido a tu diccionario."
 :dict-item-already-exists
 "[No se pudo](err) añadir [~](typ) [~](arg), ya está presente en tu
diccionario."
 :dict-item-learned
 "[~](typ) [~](arg) han sido marcadas como totalmente aprendidas."
 :dict-item-removed
 "[~](typ) [~](arg) borrados de tu diccionario."
 :dict-item-reset
 "El progreso de [~](typ) [~](arg) ha sido reestablecido."
 :dict-no-such-item
 "[No se pudo](err) encontrar [~](typ) [~](arg), no existe tal ítem"
 :dict-trans-changed
 "Traducción de [~](typ) [~](arg) cambiada"
 :exercise-constructor
 "Ingrese la palabra letra por letra (pista: tienes todas las letras de la
palabra en la línea de comandos, pero están desordenadas):"
 :exercise-crossword
 "Recorre a través de varias palabras e intenta ingresarlas correctamente
según su descripción. Si no conoces una de las palabras, puedes saltearla
(ingresa una línea en blanco presionando [Enter](typ). Si ingresas una
palabra correctamente, algunas letras de otras palabras que coincidan con
las letras de la palabra ingresada serán reveladas. El ejercicio continúa
hasta que ingreses todas las palabras correctamente."
 :exercise-listening
 "Escucha algunas grabaciones, reconoce la palabra e ingrésala (presiona
[Ctrl-o](typ) para escuchar la grabación nuevamente."
 :exercise-translation
 "Selecciona la traducción correcta de un ítem del diccionario."
 :failed-audio-query
 "[No se pudo](err) encontrar ningún audio relevante para [\"~\"](arg)"
 :help-command-reminder
 "Para información sobre el comando escriba: [help](cmd) [~](arg)"
 :incorrect
 "[Respuesta incorrecta.](inc)"
 :index
 "Index"
 :lexemes
 "Lexemas Definidos"
 :lexeme-forms
 "Lexeme Forms"
 :name
 "Name"
 :no-such-lexeme
 "[No se pudo](err) encontrar la definición del lexema [~](arg)"
 :not-enough-forms
 "No hay [not enough](err) palabras en el diccionario."
 :possible-corrections
 "Posible correcciones para [~](cmd):"
 :progress
 "%"
 :proposed-audio
 "Audio propuesto: [\"~\"](arg)"
 :tutorial-0
 "[Bienvenido a Шτookωвiнα](hdr), el programa que te ayudará a aprender
[~](arg). En Шτookωвiнα ingresas comandos y sus argumentos para hacer que
varias cosas sucedan. De esta forma puedes añadir palabras a tu diccionario
y editarlas, realizar varios ejercicios con estas palabras, y otras cosas
interesantes. Cada comando está minuciosamente documentado, así que la
primera cosa que deberías aprender acerca de este entorno es cómo descubrir
nuevos comandos y obtener descripción del comando en el que estás
interesado. El comando [help](cmd) viene bien aquí. Intenta escribirlo ahora
sin argumentos."
 :tutorial-1
 "[Genial!](hdr) Como puedes ver hay bastantes comandos disponibles en
Шτookωвiнα. Elije alguno y escribe el comando [help](cmd) con el nombre del
comando como argumento, por ejemplo [help](cmd) [help](arg)."
 :tutorial-2
 "[Fantástico!](hdr) Ahora que sabes cómo explorar el entorno, puedes
continuar por tu cuenta, pero nos gustaría mostrarte primero cómo añadir
palabras a tu diccionario. Usa el comando [add](cmd) para esto. Si lo
revisas con ayuda del comando [help](cmd) [add](arg), ¡notarás que toma
cuatro argumentos! Piensa qué darle al comando [add](cmd) y añade la primera
palabra a tu diccionario."
 :tutorial-3
 "[Bien!](hdr) Ahora añade [11](arg) más palabras de diferentes tipos.
Deberías tener [12](arg) palabras en tu diccionario para continuar con este
tutorial. Fíjate que puedes obtener algunas estadísticas con ayuda del
comando [dict](cmd). Este comando puede ser usado para mostrar estadísticas
sobre ciertas palabras en particular que tienen un prefijo en común. Dale
ese prefijo como primer argumento al comando [dict](cmd) para usar esta
funcionalidad."
 :tutorial-4
 "[12 palabras deberían bastar](hdr) para comenzar nuestro entrenamiento.
Шτookωвiнα tiene algunos ejercicios para practicar diferentes aspectos del
reconocimiento de palabras: [traducción](typ), [escritura](typ), y
[escucha](typ) ¡Incluso tiene un tipo de crucigrama! Puedes escribir
comandos para comenzar con algunos de estos ejercicios, pero te recomendamos
usar el comando [train](cmd) que provee un entrenamiento integral usando
todos estos ejercicios en el orden correcto. Llama al comando [train](cmd)
sin argumentos."
 :tutorial-5
 "[Bueno, así es como Шτookωвiнα puede ayudarte a recordar palabras.](hdr)
Hay más estadísticas por palabra que ayudan a Шτookωвiнα a determinar qué
palabras necesitan más entrenamiento. Escribe el comando [dict](cmd) y ¡ve
por ti mismo que has hecho progreso!"
 :tutorial-6
 "[Not bad at all!](hdr) Шτookωвiнα acepta otras funcionalidades también.
Puedes enseñarle cómo buscar la definición y/o traducción de una palabra en
diccionarios en línea. Puedes también reprogramar Шτookωвiнα añadiendo algo
de código en tu archivo de configuración. Incluso puedes definir nuevos
comandos, etc. Este tutorial te ha mostrado muy básicos comandos y temo que
va a terminar. Para salir del entorno interactivo escribe
[quit](cmd). ¡Buena suerte!"
 :tutorial-try-again
 "[Qué pena.](inc) Inténtalo de nuevo."
 :uncorrectable-command
 "[No se pudo](err) corregir el comando [~](cmd)"
 :unknown-form-query
 "[~](typ) [~](arg) ~ es [desconocido](err), rellénalo para continuar:"
 :value
 "Valor"
 :where
 "where "
 :wizard-audio-query
 "Selecciona cómo Шτookωвiнα debería reproducir archivos FLAC:"
 :wizard-audio-query-manually
 "OK, define hook [:audio-query](typ) yourself in your configuration file."
 :wizard-conj-ext
 "Edit example of [:conj-ext](typ) hook in your configuration file to
activate [conj](cmd) command."
 :wizard-query-ext
 "Edit example of [:query-ext](typ) hook in your configuration file to
 activate [query](cmd) command."
 :wizard-shtooka-dirs
 "Шτookωвiнα necesita saber dónde están las fuentes de audio de Shtooka en
tu sistema. Puedes descargar grabaciones de audio en tu idioma desde el
sitio oficial del Proyect Shtooka [<http://download.shtooka.net/>](typ).
Desarchívalas y ubícalas en un directorio, luego ingresa la ruta del
directorio aquí:"
 :wizard-shtooka-dirs-bad
 "El directorio [\"~\"](arg) no existe o está vacío. Intenta de nuevo."
 :wizard-shtooka-dirs-ok
 "Diccionarios añadidos"
 :wizard-ui-lang
 "Selecciona el idioma que será usado para la interfaz de usuario. Ingresa
dos letras, por ejemplo [\"en\"](arg) para Inglés."
 :wizard-ui-lang-bad
 "El input [\"~\"](arg) no representa ningún idioma definido, intenta de
nuevo."
 :wizard-ui-lang-ok
 "OK, usando [~](arg) para la interfaz de usuario.")
