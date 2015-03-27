![Шτookωвiнα Logo](https://cdn.rawgit.com/mrkkrp/shtookovina/master/doc/logo.svg)

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

4. Download or clone Sthookovina's repo:

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

## License

Copyright (c) 2015 Mark Karpov

Distributed under GNU GPL, version 3.
