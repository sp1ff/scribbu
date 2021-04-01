# Tell automake that these two must be built first; we say how below
BUILT_SOURCES = tbt-parser.hh tbt-lexer.hh scheme.x

AM_YFLAGS = -d
AM_CPPFLAGS = -I$(srcdir)/.. $(BOOST_CPPFLAGS) $(OPENSSL_INCLUDES) $(GUILE_CFLAGS)
AM_CXXFLAGS = -std=c++17 -Wno-register $(GUILE_CFLAGS)

lib_LTLIBRARIES = libscribbu.la

libscribbu_la_SOURCES = tbt-parser.yy		\
			tbt-lexer.ll		\
			scribbu.cc		\
			charsets.cc		\
			ostream.cc		\
			id3v1.cc		\
			framesv2.cc		\
			framesv22.cc		\
			framesv23.cc		\
			framesv24.cc		\
			id3v2.cc		\
			id3v22.cc		\
			id3v23.cc		\
			id3v24.cc		\
			id3v2-utils.cc		\
			tbt-support.cc		\
			pprinter.cc		\
			csv-pprinter.cc		\
			tdf-pprinter.cc		\
			tagset.cc		\
			dynwind-context.cc	\
			scheme.cc		\
			scheme-serde.cc         \
			winamp-genres.cc

pkginclude_HEADERS = scribbu.hh		\
		     errors.hh		\
		     charsets.hh	\
		     ostream.hh		\
		     id3v1.hh		\
		     framesv2.hh	\
		     framesv22.hh	\
		     framesv23.hh	\
		     framesv24.hh	\
		     id3v2.hh		\
		     id3v22.hh		\
		     id3v23.hh		\
		     id3v24.hh		\
		     id3v2-utils.hh	\
		     tbt-support.hh	\
		     pprinter.hh	\
		     csv-pprinter.hh	\
		     tdf-pprinter.hh	\
		     tagset.hh		\
		     dynwind-context.hh \
		     scheme.hh		\
		     scheme-serde.hh    \
		     winamp-genres.hh

libscribbu_la_LDFLAGS = -version-info 3:0:0

# automake will figure out how to make tbt-parser.hh, but we need to tell it how 
# to make tbt-lexer.h:
tbt-lexer.hh: Makefile
	$(LEX) $(AM_LFLAGS) $(LFLAGS) $(srcdir)/tbt-lexer.ll

DISTCLEANFILES = tbt-lexer.cc tbt-lexer.hh scheme.x

SUFFIXES = .x
.cc.x:
	guile-snarf -o $@ $< $(AM_CPPFLAGS) $(AM_CXXFLAGS) -I$(builddir)/..
