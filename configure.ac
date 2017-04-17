AC_INIT([scribbu], [0.2], [sp1ff@pobox.com])
AC_CONFIG_MACRO_DIR([macros])
AC_CONFIG_SRCDIR([src/scribbu.cc])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall -Werror])
AC_PROG_CXX
AM_PROG_AR
AC_PROG_LIBTOOL
AC_PROG_YACC
AM_PROG_LEX

AX_BOOST_BASE([1.57], [], [AC_MSG_ERROR([Scribbu requires boost_base 1.57 or later.])])
AX_BOOST_FILESYSTEM
AX_BOOST_LOG
AX_BOOST_PROGRAM_OPTIONS
AX_BOOST_REGEX
AX_BOOST_SYSTEM
AX_BOOST_UNIT_TEST_FRAMEWORK
AX_CHECK_OPENSSL([],[AC_MSG_ERROR([Scribbu requires openssl.])])
AX_CHECK_ZLIB([],[AC_MSG_ERROR([Scribbu requires zlib.])])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile scribbu/Makefile src/Makefile test/Makefile test/data/Makefile])
AC_OUTPUT
