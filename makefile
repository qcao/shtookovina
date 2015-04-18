#
# Шτookωвiнα Makefile
#
# Copyright © 2015 Mark Karpov
#
# Шτookωвiнα Makefile is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.
#
# Шτookωвiнα Makefile is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.

.PHONY : install-deps clear

build/shtk : $(wildcard src/*.lisp src/shtookovina.asd) install-deps
	buildapp --output build/shtk --manifest-file build/manifest.txt \
	--load-system shtookovina --entry shtookovina:main

install-deps : src/shtookovina.asd
	mkdir -vp build
	sbcl --non-interactive \
	--load src/shtookovina.asd --eval "(ql:quickload :shtookovina)" \
	--eval '(ql:write-asdf-manifest-file "build/manifest.txt")'
	echo $(abspath ./src/shtookovina.asd) >> build/manifest.txt

clear :
	rm -vr build
