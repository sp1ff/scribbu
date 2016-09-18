BUILT_SOURCES = tbt-parser.cc tbt-parser.hh tbt-lexer.cc tbt-lexer.hh program-parser.cc program-parser.hh program-lexer.cc program-lexer.hh
AM_YFLAGS = -d
AM_CPPFLAGS = -I$(srcdir)/.. $(BOOST_CPPFLAGS) $(OPENSSL_INCLUDES)
lib_LTLIBRARIES = libscribbu.la
libscribbu_la_SOURCES = tbt-parser.yy tbt-lexer.ll program-parser.yy program-lexer.ll scribbu.cc id3v1.cc framesv2.cc framesv22.cc framesv23.cc framesv24.cc id3v2.cc id3v22.cc id3v23.cc id3v24.cc id3v2-utils.cc tbt-support.cc program-support.cc
include_HEADERS = scribbu.hh errors.hh id3v1.hh framesv2.hh framesv22.hh framesv23.hh framesv24.hh id3v2.hh id3v22.hh id3v23.hh id3v24.hh id3v2-utils.hh tbt-support.hh program-support.hh
AM_CXXFLAGS = -std=c++11

