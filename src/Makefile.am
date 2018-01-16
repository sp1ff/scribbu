bin_PROGRAMS = scribbu
scribbu_SOURCES = scribbu.cc command-utilities.hh command-utilities.cc	\
	       	  rename.cc report.cc split.cc dump.cc
AM_CPPFLAGS = -I$(srcdir)/.. $(BOOST_CPPFLAGS) $(GUILE_CFLAGS) -DSCRIBBU_ALWAYS_DUMP_CORE
AM_CXXFLAGS = -std=c++17 $(GUILE_CFLAGS)
AM_LDFLAGS  = -static $(BOOST_LDFLAGS) $(GUILE_LIBS) $(LIBUNISTRING)
scribbu_LDADD = ../scribbu/libscribbu.la \
	$(OPENSSL_LIBS)                  \
	-ldl                             \
	$(BOOST_FILESYSTEM_LIB)          \
	$(BOOST_LOG_LIB)                 \
	$(BOOST_PROGRAM_OPTIONS_LIB)     \
	$(BOOST_REGEX_LIB)               \
	$(BOOST_SYSTEM_LIB)              \
	-lpthread -lcrypt -lltdl -lgmp
dist_man_MANS = scribbu.1 scribbu-dump.1 scribbu-rename.1 scribbu-report.1 \
		scribbu-split.1
