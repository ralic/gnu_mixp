#!/bin/sh

[ -f configure.ac ] || {
  echo "autogen.sh: run this command only at the top of a source tree."
  exit 1
}

libtoolize --automake
aclocal -I aclocal
autoheader
autoconf
automake --add-missing
