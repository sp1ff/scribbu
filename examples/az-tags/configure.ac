AC_PREREQ([2.69])
LT_PREREQ ([2.4])
AC_INIT([az-tags], [0.1], [sp1ff@pobox.com])
AC_CONFIG_MACRO_DIR([macros])
AC_CONFIG_SRCDIR([src/main.cc])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([-Wall -Werror])
LT_INIT

AC_PROG_CXX

dnl scribbu is not an optional package for `az-tags'; it would make no
dnl sense to invoke configure with --with-no-scribbu.

AC_ARG_WITH([scribbu],
    AS_HELP_STRING([--with-scribbu=DIR], 
                   [root directory of scribbu installation]),
    [
        case "$withval" in
	"" | y | ye | yes | n | no)
	    AC_MSG_ERROR([--with-scribbu takes a root directory]);;
	*) 
	    scribbu_dirs="$withval";;
	esac
    ],
    [
        # Just use the defaults
	scribbu_dirs="/usr/local /usr /opt/local /sw"
    ])

dnl One way or another, we have one or more candidates in ${scribbu_dirs}
found=no
for scribbu_home in ${scribbu_dirs}; do
    AC_MSG_CHECKING([for scribbu/scribbu.h under ${scribbu_home}])
    if test -f "${scribbu_home}/include/scribbu/scribbu.hh"; then
        SCRIBBU_CPPFLAGS="-I${scribbu_home}/include"
	SCRIBBU_LDFLAGS="-L${scribbu_home}/lib"
	SCRIBBU_LIBS="-lscribbu"
	found=yes
	AC_MSG_RESULT([yes])
	break
    else
        AC_MSG_RESULT([no])
    fi
done

if test "$found" != "yes"; then
    AC_MSG_ERROR([couldn't find scribbu])
fi

# try the preprocessor and linker with our new flags,
# being careful not to pollute the global LIBS, LDFLAGS, and CPPFLAGS
AC_MSG_CHECKING([whether compiling and linking against scribbu will work])

save_LIBS="$LIBS"
save_LDFLAGS="$LDFLAGS"
save_CPPFLAGS="$CPPFLAGS"
save_CXXFLAGS="$CXXFLAGS"
LIBS="$SCRIBBU_LIBS $LIBS"
LDFLAGS="$SCRIBBU_LDFLAGS $LDFLAGS"
CPPFLAGS="$SCRIBBU_CPPFLAGS $CPPFLAGS"
CXXFLAGS="-std=c++17"

AC_LANG_PUSH([C++])
AC_CHECK_HEADER([scribbu/scribbu.hh], [scribbu_hh=yes], [scribbu_hh=no])
# I'd like to do AC_CHECK_LIB here, but I can't link against libscribbu
# in a test because it, in turn depends on a bunch of other libs
AC_CHECK_FILE([${scribbu_home}/lib/libscribbu.la],
    [scribbu_la=yes], [scribbu_la=no])
AC_LANG_POP([C++])

LIBS="$save_LIBS"
LDFLAGS="$save_LDFLAGS"
CPPFLAGS="$save_CPPFLAGS"
CXXFLAGS="$save_CXXFLAGS"

if test "yes" = "$scribbu_hh" && test "yes" = "$scribbu_la"; then
    AC_DEFINE([HAVE_SCRIBBU], [1], [Define to 1 if you have libscribbu])
else
    AC_MSG_ERROR([az-tags requires scribbu])
fi

AC_SUBST([SCRIBBU_CPPFLAGS])
AC_SUBST([SCRIBBU_LIBS])
AC_SUBST([SCRIBBU_LDFLAGS])

PKG_CHECK_MODULES([GUILE], [guile-2.2])
AX_BOOST_BASE([1.58], [], 
    [AC_MSG_ERROR([Scribbu requires boost_base 1.58 or later.])])

AX_BOOST_IOSTREAMS
AX_BOOST_FILESYSTEM
AX_BOOST_SYSTEM
AX_CHECK_OPENSSL([],[AC_MSG_ERROR([Scribbu requires openssl.])])

AC_CONFIG_FILES([Makefile src/Makefile])

AC_OUTPUT
