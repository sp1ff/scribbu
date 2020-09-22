SUBDIRS = scribbu src scheme test doc
ACLOCAL_AMFLAGS = -I macros
EXTRA_DIST = examples

dist-hook:
	chmod -R a+w $(distdir)/examples
	rm -rf `find $(distdir)/examples -type f -name *~`

# On Linux, "make install" suffers from a fairly severe problem, laid
# out in detail here:
#
# <http://gnu-automake.7480.n7.nabble.com/error-while-loading-shared-libraries-foo-so-0-cannot-open-shared-object-file-No-such-file-or-directo-tp3970p3973.html>
#
# Cutting to the heart of the matter, the problem is this:
#
# 1. at the end of the install, libtool will call 'ldconfig -n $libdir'
# 2. 'ldconfig -n' implies '-N' (see 'man ldconfig') which means don't
#    update the cache (it just adds symlinks if needed, which they
#    shouldn't be)
# 3. when the user tries to run scribbu, ld.so does *not* search all
#    directories in /etc/ld.so.conf; it searches
#    - LD_LIBRARY_PATH
#    - candidates in /etc/ld.so.cache, which of course hasn't been
#      updated
#    - /usr/lib & /lib
#
# The author (who seems to be a maintainer) says of this "Strictly
# speaking, I guess libtool should call plain ldconfig.  *That*
# however could be unbearably slow if done for every library being
# installed."
#
# A later conversation
# <https://lists.gnu.org/archive/html/automake/2013-12/msg00011.html>
# suggests using install-exec-hook target, at the end of the install,
# to run ldconfig to update the cache.

install-exec-hook:
	if type ldconfig 2> /dev/null; then sudo ldconfig $(DESTDIR)$(libdir); fi

uninstall-hook:
	if type ldconfig 2> /dev/null; then sudo ldconfig $(DESTDIR)$(libdir); fi
