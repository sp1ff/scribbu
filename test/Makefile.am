check_PROGRAMS = unit
SUBDIRS = data
EXTRA_DIST = test-util test-split test-rename test-report test-report-tdf \
	test-dump test-fs-generator test-fs-generator.scm test-with-track-in \
	test-cleanup-encoded-by.scm test-cleanup-encoded-by \
	test-cleanup-from-audacity.scm test-cleanup-from-audacity \
	test-tagsets-from-scheme.scm test-tagsets-from-scheme \
	test-frames-from-scheme.scm test-frames-from-scheme \
	test-scripting test-options test-popm test-xtag test-text test-genre
CLEANFILES = id3v20B id3v21B report.last.csv report-tdf.last.tdf \
	report.csv report-last.csv report-tdf.tdf \
	id3v20A trackB id3v1A trackA \
	Pogues,\ The\ --\ Lorca\'s\ Novena.mp3 dump1.log \
	dump2.log with-track-in.log test.write.2.2.dat tag22.dat \
	elliot-goldenthal.id3v1.tag.out test-cleanup-encoded-by.out \
	test.tag test-cleanup-from-audacity.out \
	test-xtag-tmp/xtag_smoke_tests/id3v2.3.tag \
	test-xtag-tmp/xtag_smoke_tests/id3v2.3.tag.1 \
	test-popm-tmp/smoke_tests/id3v2.3.tag \
	test-popm-tmp/smoke_tests/id3v2.3.tag.1 \
	test-popm-tmp/test_issue_1/id3v2.3.tag \
	test-popm-tmp/test_issue_1/id3v2.3.tag.1 \
	test-popm-tmp/test_id3v1_only/v1-only.mp3 \
	test-popm-tmp/test_id3v1_only/v1-only.mp3.1 \
	test-text-tmp/smoke_tests/id3v2.3.tag
TESTS = $(check_PROGRAMS) test-split test-rename test-report test-report-tdf \
	test-dump test-with-track-in test-fs-generator \
	test-cleanup-encoded-by test-cleanup-from-audacity \
	test-tagsets-from-scheme test-frames-from-scheme test-scripting \
	test-options test-popm test-xtag test-text test-genre
unit_SOURCES = unit.cc charsets.cc ostream.cc id3v1.cc framesv2.cc \
	framesv22.cc framesv23.cc framesv24.cc id3v2.cc id3v22.cc id3v23.cc \
	id3v24.cc id3v2-utils.cc pprinter.cc csv-pprinter.cc tdf-pprinter.cc \
	scribbu.cc id3v2-edit.cc tagset.cc winamp-genres.cc
include_HEADERS = unit.hh
AM_CPPFLAGS = -DBOOST_TEST_DYN_LINK -I$(srcdir)/.. $(BOOST_CPPFLAGS)
AM_CXXFLAGS = -std=c++17
AM_LDFLAGS  = $(BOOST_LDFLAGS)
unit_LDADD = ../scribbu/libscribbu.la    \
	$(GUILE_LIBS)                    \
	$(OPENSSL_LIBS)                  \
	$(BOOST_IOSTREAMS_LIB)           \
	$(BOOST_FILESYSTEM_LIB)          \
	$(BOOST_PROGRAM_OPTIONS_LIB)     \
	$(BOOST_UNIT_TEST_FRAMEWORK_LIB) \
	$(BOOST_SYSTEM_LIB)
