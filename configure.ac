#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ([2.69])
AC_INIT([scribbu], [0.6.23], [sp1ff@pobox.com], [scribbu], [https://github.com/sp1ff/scribbu])
AC_CONFIG_MACRO_DIR([macros])
AC_CONFIG_SRCDIR([src/scribbu.cc])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall -Werror dist-bzip2 dist-xz dist-zstd])
AC_CANONICAL_HOST
AH_TEMPLATE([MACOS],[Defined when running on a MacOS.])
case "${host_os}" in
     darwin*)
	AC_DEFINE([MACOS]);;
esac

# Checks for programs.
AC_PROG_CXX
AC_C_VARARRAYS
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AM_PROG_AR
AC_PROG_LIBTOOL
AC_PROG_YACC
AC_PROG_LEX

# Checks for libraries.
AX_BOOST_BASE([1.63], [], [AC_MSG_ERROR([Scribbu requires boost_base 1.63 or later.])])
AX_BOOST_IOSTREAMS
AX_BOOST_LOG
AX_BOOST_PROGRAM_OPTIONS
AX_BOOST_REGEX
AX_BOOST_SYSTEM
AX_BOOST_UNIT_TEST_FRAMEWORK
AX_CHECK_OPENSSL([],[AC_MSG_ERROR([Scribbu requires openssl.])])
AX_CHECK_ZLIB([],[AC_MSG_ERROR([Scribbu requires zlib.])])

gl_LIBUNISTRING
if test x$HAVE_LIBUNISTRING != xyes; then
 AC_MSG_ERROR([GNU libunistring is required])
fi

# https://llvm.org/svn/llvm-project/llvm/tags/cremebrulee/cremebrulee-53/autoconf/configure.ac
AC_SEARCH_LIBS(ffi_call,ffi,AC_DEFINE([HAVE_FFI_CALL],[1],
                 [Define if libffi is available on this platform.]),
                 AC_MSG_ERROR([libffi not found]))

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h])
AC_CHECK_HEADERS([libintl.h])
AC_CHECK_HEADERS([stddef.h])
AC_CHECK_HEADERS([stdint.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_INT8_T
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset strdup strerror floor])

GUILE_PKG([3.0 2.2 2.0])
GUILE_FLAGS
GUILE_PROGS
GUILE_SITE_DIR

DX_DOXYGEN_FEATURE(ON)
DX_DOT_FEATURE(ON)
DX_HTML_FEATURE(ON)
DX_CHM_FEATURE(OFF)
DX_CHI_FEATURE(OFF)
DX_MAN_FEATURE(OFF)
DX_RTF_FEATURE(OFF)
DX_XML_FEATURE(OFF)
DX_PDF_FEATURE(OFF)
DX_PS_FEATURE(OFF)
DX_INIT_DOXYGEN([scribbu], [$(builddir)/Doxyfile], [$(builddir)/doxygen-doc])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile scribbu/Makefile src/Makefile scheme/Makefile])
AC_CONFIG_FILES([test/Makefile test/data/Makefile doc/Makefile doc/Doxyfile])
AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([contrib/rip-tracks], [chmod +x contrib/rip-tracks])
AC_OUTPUT
