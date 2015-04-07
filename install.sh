#!/bin/sh
#
# Шτookωвiнα Installation Script
#
# Copyright © 2015 Mark Karpov
#
# Шτookωвiнα Installation Script is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Шτookωвiнα Installation Script is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

### constants

I_DIRS="/usr/share/{shtookovina/{ui-langs,langs},licenses/shtookovina}"

### functions

bad_exit() # prints error message and exits the program
{
    echo "failed" 1>&2
    exit 1
}

### main

echo 'Шτookωвiнα installation has been started;'

# 1. check if actual user is root (must be root to install the software)

echo -n 'actual user must be root...'
test $(id -u) -gt 0 && bad_exit
echo 'ok'

# 2. check if there is compiled executable

echo -n 'searching for executable...'
test -f build/shtk || bad_exit
echo 'ok'

# 3. creating directories

echo 'creating directories...'
eval install -vdm755 $I_DIRS
if test $? -eq 0
then echo 'creating directories: ok'
else bad_exit
fi

# 4. copying new files

echo 'copying new files...'
install -vDm755 build/shtk    /usr/bin/
install -vDm644 langs/*       /usr/share/shtookovina/langs/
install -vDm644 ui-langs/*    /usr/share/shtookovina/ui-langs/
install -vDm644 LICENSE.md    /usr/share/licenses/shtookovina/
install -vDm644 doc/shtk.1.gz /usr/share/man/man1/
echo 'copying new files: ok'

# 5. done

echo 'done.'
