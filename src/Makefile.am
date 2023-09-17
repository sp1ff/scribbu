bin_PROGRAMS = scribbu
scribbu_SOURCES = scribbu.cc command-utilities.hh command-utilities.cc \
	rename.cc report.cc split.cc dump.cc popm.cc xtag.cc text.cc   \
        genre.cc m3u.cc encodings.cc
AM_CPPFLAGS = -I$(srcdir)/.. $(BOOST_CPPFLAGS) $(GUILE_CFLAGS) -DDATADIR=\"$(datadir)\" -DSCRIBBU_ALWAYS_DUMP_CORE
# AM_CPPFLAGS = -I$(srcdir)/.. $(BOOST_CPPFLAGS) $(GUILE_CFLAGS) -DDATADIR=\"$(datadir)\"
AM_CXXFLAGS = -std=c++17 $(GUILE_CFLAGS)
AM_LDFLAGS  = $(BOOST_LDFLAGS) $(LIBUNISTRING)
scribbu_LDADD = ../scribbu/libscribbu.la \
	$(GUILE_LIBS)                    \
	$(OPENSSL_LIBS)                  \
	$(BOOST_IOSTREAMS_LIB)           \
	$(BOOST_FILESYSTEM_LIB)          \
	$(BOOST_LOG_LIB)                 \
	$(BOOST_PROGRAM_OPTIONS_LIB)     \
	$(BOOST_REGEX_LIB)               \
	$(BOOST_SYSTEM_LIB)
