#!/bin/sh
# Usage: ./autogen.sh

[ -f configure.ac ] || {
  echo "autogen.sh: run this command only at the top of a source tree."
  exit 1
}

set -e

guile-baux-tool import \
    re-prefixed-site-dirs \
    c2x \
    gen-scheme-wrapper \
    punify \
    gbaux-do

gnulib-tool --copy-file doc/INSTALL.UTF-8 INSTALL

autoreconf --verbose --force --install --symlink --warnings=all

# autogen.sh ends here
