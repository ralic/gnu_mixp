# Makefile.am
#
# Copyright (C) 2011 Thien-Thi Nguyen
#
# This file is part of Mixp.
#
# Mixp is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3, or
# (at your option) any later version.
#
# Mixp is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with Mixp; see the file COPYING.  If not,
# write to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA  02110-1301  USA

TESTS_ENVIRONMENT = sh runtest
TESTS = x001

check_DATA = .fake

submake = $(MAKE) $(AM_MAKEFLAGS)

## This hair is so that we can do, e.g.:
##  touch ../src/expat.c && make check
.PHONY: ../src/stamp-all
../src/stamp-all:
	@cd ../src && $(submake) stamp-all

stamp-all: ../src/stamp-all
	@touch $@ -r $<

.fake: stamp-all
	top=`pwd`/.fake ;			\
	rm -rf $$top ;				\
	cd ../src ;				\
	$(submake) install			\
	  FOR_MAKE_CHECK=1			\
	  prefix="$$top"

clean-local:
	-rm -rf .fake

EXTRA_DIST = \
  REC-xml-19980210.xml \
  sub.inc \
  test.dtd \
  $(TESTS)

# Makefile.am ends here
