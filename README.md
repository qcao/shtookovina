# Шτookωвiнα

This will be some Lispy stuff for language learning. It will be based on
recordings from [Shtooka Project](http://shtooka.net/).

*Note: this is beginning to be usable, you can install it and try. However,
Шτookωвiнα is not very user-friendly at the moment.*

## Installation

Installation process is pretty straightforward:

1. Install [SBCL](http://www.sbcl.org/), Шτookωвiнα is written in standard
   Common Lisp and it doesn't rely on any specific features of SBCL, however
   only version compiled with SBCL is tested and guaranteed to work;

2. Install [Quicklisp](http://www.quicklisp.org/) to automatically get all
   dependencies, in short, here is what to do:

   ```
   $ curl -O http://beta.quicklisp.org/quicklisp.lisp
   $ sbcl --load quicklisp.lisp
   * (quicklisp-quickstart:install)
   * (quit)
   ```

3. Install [Buildapp](http://www.xach.com/lisp/buildapp/);

4. Download or clone Шτookωвiнα's repo:

   ```
   $ git clone https://github.com/mrkkrp/shtookovina.git master
   ```

5. `cd` into the directory and `make` the program:

   ```
   $ cd shtookovina
   $ make
   ```

6. Now you should have Шτookωвiнα's binary `build/shtk`, you can install it
   this way:

   ```
   # sh install.sh
   ```

7. Done (you can use `uninstall.sh` to uninstall the program).

## Wizard and Audio

Now, you hopefully has installed Шτookωвiнα. However, it should be
configured before you can use it. Initially Шτookωвiнα was planned as a
hardcore program for Lisp geeks, but later I decided to make it more
user-friendly, so more people could use it. To make Шτookωвiнα work you're
supposed to write some Lisp in your configuration file setting some
variables and defining some hooks. Now we have wizard that can do this
automatically.

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

Third, the wizard will ask your which way you would like to play audio files
(Shtooka project uses FLAC files). You should choose between listed options.

Finally, to use `query` and `conj` commands (open web-page with description
of given word or conjugation of given verb) you currently need to manually
edit relevant hooks in your configuration files, since we cannot know which
web-service your prefer to use for this task, and thus we cannot know how to
transform the word to URL.

## License

Copyright (c) 2015 Mark Karpov

Distributed under GNU GPL, version 3.
