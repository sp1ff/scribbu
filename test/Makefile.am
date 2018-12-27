check_PROGRAMS = unit
SUBDIRS = data
EXTRA_DIST = test-split test-rename test-report test-dump test-fs-generator \
	test-fs-generator.scm test-with-track-in test-cleanup-encoded-by.scm \
	test-cleanup-encoded-by test-cleanup-from-audacity.scm test-cleanup-from-audacity
CLEANFILES = id3v20B id3v21B report.last.csv id3v20A trackB report.csv \
	id3v1A trackA Pogues,\ The\ --\ Lorca\'s\ Novena.mp3 dump1.log \
	dump2.log with-track-in.log test.write.2.2.dat tag22.dat \
	elliot-goldenthal.id3v1.tag.out test-cleanup-encoded-by.out \
	test.tag test-cleanup-from-audacity.out
TESTS = $(check_PROGRAMS) test-split test-rename test-report test-dump \
	test-with-track-in test-fs-generator test-cleanup-encoded-by \
	test-cleanup-from-audacity
unit_SOURCES = unit.cc charsets.cc ostream.cc id3v1.cc framesv2.cc \
	framesv22.cc framesv23.cc framesv24.cc id3v2.cc id3v22.cc id3v23.cc \
	id3v24.cc id3v2-utils.cc pprinter.cc csv-pprinter.cc tdf-pprinter.cc \
	scribbu.cc id3v2-edit.cc
include_HEADERS = unit.hh
AM_CPPFLAGS = -DBOOST_TEST_DYN_LINK -I$(srcdir)/.. $(BOOST_CPPFLAGS)
AM_CXXFLAGS = -std=c++17
AM_LDFLAGS  = $(BOOST_LDFLAGS)
unit_LDADD = ../scribbu/libscribbu.la    \
	$(GUILE_LIBS)                    \
	$(OPENSSL_LIBS)                  \
	-ldl                             \
	$(BOOST_FILESYSTEM_LIB)          \
	$(BOOST_PROGRAM_OPTIONS_LIB)     \
	$(BOOST_UNIT_TEST_FRAMEWORK_LIB) \
	$(BOOST_SYSTEM_LIB)              \
	-lpthread
