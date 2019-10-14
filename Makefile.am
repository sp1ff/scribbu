SUBDIRS = scribbu src scheme test doc
ACLOCAL_AMFLAGS = -I macros
EXTRA_DIST = examples
dist-hook:
	rm -rf `find $(distdir)/examples -type f -name *~`
