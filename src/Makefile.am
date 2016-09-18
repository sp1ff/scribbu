bin_PROGRAMS = scribbu
scribbu_SOURCES = scribbu.cc command.hh command.cc		\
		  command-utilities.hh command-utilities.cc	\
	          dump.hh dump.cc                               \
		  execute.hh execute.cc                         \
		  find.hh find.cc                               \
	       	  rename.hh rename.cc				\
	       	  report.hh report.cc				\
		  split.hh split.cc
AM_CPPFLAGS = -I$(srcdir)/.. $(BOOST_CPPFLAGS)
AM_CXXFLAGS = -std=c++11
AM_LDFLAGS  = -all-static -static $(BOOST_LDFLAGS)
scribbu_LDADD = ../scribbu/libscribbu.la \
	$(OPENSSL_LIBS)                  \
	-ldl                             \
	$(BOOST_FILESYSTEM_LIB)          \
	$(BOOST_LOG_LIB)                 \
	$(BOOST_PROGRAM_OPTIONS_LIB)     \
	$(BOOST_REGEX_LIB)               \
	$(BOOST_SYSTEM_LIB)              \
	-lpthread
