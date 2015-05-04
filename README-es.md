# Шτookωвiнα

*Шτookωвiнα está en fase beta. Dale una oportunidad.*

Este programa está diseñado para serle útil a estudiantes de
idiomas. Éstas son algunas características de Шτookωвiнα:

* utiliza grabaciones del [Proyecto Shtooka](http://shtooka.net/),
  estas grabaciones son hechas por hablantes nativos voluntarios, en vez
  de robots;

* tiene una interfaz de línea de comandos minimalista que incluso intenta
  ser amigable con ayuda de [cl-readline](https://github.com/mrkkrp/cl-readline)
  y de [cl-ansi-term](https://github.com/mrkkrp/cl-ansi-term);

* tiene varios ejercicios que le ayudarán al usuario a trabajar diferentes
  aspectos a la hora de recordar una palabra: traducción, escritura y escucha;

* tiene también ejercicios de «crucigrama»;

* es totalmente hackeable a través del script de configuración escrito
  en Common Lisp, el usuario puede definir nuevos comandos y cambiar
  absolutamente todo en Шτookωвiнα;

* este programa no hace ninguna suposición sobre el idioma a aprender,
  esto permite añadir definiciones de nuevos idiomas muy fácilmente;

* el idioma de la interfaz de usuario también puede ser especificado,
  las nuevas traducciones se hacen copiando existentes definiciones
  de idiomas de la interfaz de usuario y editando listas de cadenas de
  caracteres;

* para los usuarios que no conozcan Common Lisp existe Wizard que puede
  configurar la mayoría de los parámetros;

* existe también un tutorial interactivo incluido.

## Compilación e Instalación

El proceso de instalación es muy sencillo:

1. Instale [SBCL](http://www.sbcl.org/), Шτookωвiнα está escrito en estándar
   Common Lisp y no necesita ninguna característica en particular de SBCL,
   sin embargo solo la versión compilada con SBCL es testeada.

2. Instale [Quicklisp](http://www.quicklisp.org/) para automaticamente
   obtener todas las dependencias, en definitiva, esto es lo que hay que hacer:

   ```
   $ curl -O http://beta.quicklisp.org/quicklisp.lisp
   $ sbcl --load quicklisp.lisp
   * (quicklisp-quickstart:install)
   * (quit)
   ```

3. Instale [Buildapp](http://www.xach.com/lisp/buildapp/), particularmente
   la última versión:

   ```
   $ git clone https://github.com/xach/buildapp.git
   $ cd buildapp
   # make install
   ```

4. Descargue o clone el repositorio de Шτookωвiнα:

   ```
   $ git clone https://github.com/mrkkrp/shtookovina.git
   ```

5. Ejecute `cd` con el directorio resultante y compile el programa con `make`:

   ```
   $ cd shtookovina
   $ make
   ```

6. Ahora debería tener el binario de Шτookωвiнα en `build/shtk`, puede
   instalarlo de esta manera:

   ```
   # bash install.sh
   ```

7. Listo (puede ejecutar `uninstall.sh` para desinstalar el programa).

## ¿Cómo comenzar con Шτookωвiнα?

Шτookωвiнα puede ser usado para aprender varios idiomas al mismo tiempo.
En consecuencia debe mantener distintos diccionarios y archivos de
configuración para cada idioma a aprender. Debe especificar el idioma
con la opción `-t` o `--target`:

```
$ shtk -t en # si desea aprender Inglés
```

Esta opción es obligatorio. Para añadir nuevos idiomas utilice 
[ISO 639-2](http://www.loc.gov/standards/iso639-2/php/code_list.php) al
elegir el código de dos letras.

## Wizard y el Audio

Ahora con suerte ha instalado Шτookωвiнα. Sin embargo, debería configurarlo
antes de usarlo. Inicialmente Шτookωвiнα fue ideado como un programa para
geeks de Lisp, pero luego decidimos hacerlo más amigable con el usuario,
para que más gente pudiera usarlo. Para hacer funcionar a Шτookωвiнα usted
debería escribir algo de código Lisp en su archivo de configuración
definiendo algunas vriables y atajos. Afortunadamente, tenemos a Wizard
que puede hacer esto automaticamente.

![Шτookωвiнα Wizard](img/wizard.png)

Cuando usted inicia Шτookωвiнα for primera vez, Wizard será llamado (al
menos que lo inicie con la opción `--no-wizard`). Primero le pregunta
sobre el idioma de la interfaz de usuario, en inglés (éste es el idioma
por defecto, Шτookωвiнα usará su idioma preferido tan pronto como sepa
cuál es).

Segundo, Шτookωвiнα necesita saber dónde están las bases de datos de
audio en su sistema. Шτookωвiнα usa bases de datos del proyecto Shtooka
y por una cuestión de rapidez, deberían ser descargadas en un directorio.
Puede descargar las bases de datos desde
[http://download.shtooka.net/](http://download.shtooka.net/). Recuerde
descomprimir los archivos.

Tercero, Wizard le preguntará cómo le gustaría reproducir los archivos
de audio (el proyecto Shtooka usa archivos FLAC). Deberá elegir entre
las opciones que se listarán.

Finalmente, para usar los comandos `query` y `conj` (abrir una página web
con la descripción de una palabra o la conjugación de algún verbo) necesita
actualmente editar los atajos correspondientes en su archivo de
configuración, ya que no podemos saber qué servicio web usted prefiere
usar para este propósito, y por lo tanto no podemos saber cómo transformar
la palabra en cuestión a una URL en particular.

## Tutorial

Шτookωвiнα tiene un tutorial interactivo incluído, que muestra algunos
comandos básicos:

![Шτookωвiнα Tutorial](img/tutorial.png)

Son cubiertos los siguientes temas:

* ¿Cómo encontrar nuevos comandos y explorar características del programa?

* ¿Cómo obtener la descripción completa de cualquier comando?

* ¿Cómo añadir palabras y frases al diccionario de usuario?

* ¿Qué tipo de ejercicios pueden ser usados para recordar estas palabras?

* ¿Cómo encontrar información acerca del diccionario entero e información
  detallada acerca de entradas en particular?

## Ejercicios

Actualmente hay cuatro ejercicios diferentes:

### Traduccion

![Ejercicio de Traducción](img/translation.png)

En este ejercicio se le da una palabra (en el idioma de la interfaz
o el que está aprendiendo) y cuatro posible traducciones. Debe seleccionar
una correctamente.

### Constructor de Palabras (escritura)

![Ejercicio constructor de palabras](img/constructor.png)

Se le da la traducción y las letras de la palabra, pero éstas están
desordenadas. Tiene que ingresar la palabra correctamente, letra
por letra

### Escucha

![Ejercicio de escucha](img/listening.png)

Algunos audios se reproducen, debe reconocer un ítem del diccionario
e ingresarlo correctamente.

### Crucigrama

![Ejercicio de crucigrama](img/crossword.png)

Este crucigrama no le muestra una representación visual de las palabras.
En realidad no importa si tal representación puede ser dibujada o no.
Recorre varias palabras e intenta ingresarlas correctamente según su
descripción. Si no conoce una palabra, puede saltearla. Si ingresa una
palabra correctamente, algunas letras de otras palabras que coincidan
con las letras de la palabra ingresada serán reveladas.

## ¿Cómo contribuir?

Necesitamos su ayuda. Hay muchas formas de ayudar al proyecto:

* defina su idioma si no se encuentra, busque el directorio `langs`
  por instrucciones y ejemplos;

* traduzca la interfaz de usuario, busque el directorio `ui-langs` por
  instrucciones y ejemplos;

* traduzca este archivo `README.md`, agrege el código de dos letras
  de su idioma al nombre de archivo, así `README-fr.md` (para francés);

* si ha encontrado algún error, 
  [abra un hilo de discusión](https://github.com/mrkkrp/shtookovina/issues).

## Licencia

Copyright © 2015 Mark Karpov

Distribuído bajo GNU GPL, version 3.
