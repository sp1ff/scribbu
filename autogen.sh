#!/bin/sh

# Use this script to bootstrap your build AFTER checking it out form
# source control. You should not have to use it for anything else.

set -e

# I could pass 'foreign' to AC_INIT_AUTOMAKE, but I don't want to
# lose checking for *all* GNU compliance.
echo "symlinking README"
test -L README || ln -s README.md README

# Runs autoconf, autoheader, aclocal, automake, autopoint, libtoolize
echo "Regenerating autotools files"
autoreconf -vfi

echo "Now run configure, make & make install"
