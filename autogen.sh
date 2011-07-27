#!/bin/sh
# Usage: ./autogen.sh

[ -f configure.ac ] || {
  echo "autogen.sh: run this command only at the top of a source tree."
  exit 1
}

set -e

test -d build-aux || mkdir build-aux

gnulib-tool --copy-file doc/INSTALL.UTF-8 INSTALL

autoreconf --verbose --force --install --symlink --warnings=all

# autogen.sh ends here
