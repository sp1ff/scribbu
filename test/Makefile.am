check_PROGRAMS = unit
SUBDIRS = data
EXTRA_DIST = test-split test-rename test-report test-dump
CLEANFILES = id3v20B id3v21B report.last.csv id3v20A trackB report.csv id3v1A trackA Pogues,\ The\ --\ Lorca\'s\ Novena.mp3 dump.log
TESTS = $(check_PROGRAMS) test-split test-rename test-report test-dump
unit_SOURCES = unit.cc charsets.cc ostream.cc id3v1.cc framesv2.cc \
	framesv22.cc framesv23.cc framesv24.cc id3v2.cc id3v22.cc id3v23.cc \
	id3v24.cc id3v2-utils.cc pprinter.cc csv-pprinter.cc scribbu.cc
AM_CPPFLAGS = -I$(srcdir)/.. $(BOOST_CPPFLAGS)
AM_CXXFLAGS = -std=c++11
AM_LDFLAGS  = -all-static -static $(BOOST_LDFLAGS)
unit_LDADD = ../scribbu/libscribbu.la    \
	$(BOOST_FILESYSTEM_LIB)          \
	$(OPENSSL_LIBS)                  \
	-ldl                             \
	$(BOOST_UNIT_TEST_FRAMEWORK_LIB) \
	$(BOOST_SYSTEM_LIB)              \
	-lpthread 
