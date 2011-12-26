AC_PREREQ(2.54)
AC_INIT(rts, 0.1)

AC_CANONICAL_HOST
AC_LANG(C)
AC_PROG_CC

C_COMPILER=$CC
C_PREPROC=$CPP
GAUCHE_CFLAGS=$GAUCHE_CFLAGS
GAUCHE_LIBS=$GAUCHE_LIBS
ARCHFILES=$ARCHFILES


#--------------------------------------------------------------------------------
# configuration options
#   --with-cffi           make librts for cffi (default)
#   --with-gauche           make libgaucherts for gauche (default)
#--------------------------------------------------------------------------------

AC_ARG_WITH(cffi,	[  --with-cffi		  use CFFI], ,with_cffi=yes )
AC_ARG_WITH(gauche,	[  --with-gauche		  use GAUCHE],,with_gauche=yes)



SOEXT="" 

if test "${CFLAGS+set}" != set; then
  CFLAGS=""
fi

if test "${CXXFLAGS+set}" != set; then
  CXXFLAGS=""
fi

if test "${LDFLAGS+set}" != set; then
  LDFLAGS=""
fi

if test "${GAUCHE_CFLAGS+set}" != set; then
  GAUCHE_CFLAGS=""
fi

if test "${GAUCHE_LIBS+set}" != set; then
  GAUCHE_LIBS=""
fi


AC_DEFUN(AC_CHECK_C_COMPILER_OPT,
	[AC_CACHE_CHECK(whether $C_COMPILER accepts [$1], [$2],
	[AC_SUBST($2)
	rm -f conftest.$ac_ext
	cat >conftest.$ac_ext <<_ACEOF
#line $LINENO "configure"
int
main ()
{
  return 0;
}
_ACEOF



ac_test_CXXFLAGS=${CXXFLAGS+set}
ac_save_CFLAGS=$CFLAGS
ac_save_CXXFLAGS=$CXXFLAGS

CFLAGS="$1"
CXXFLAGS="$1"
if { (eval echo "$as_me:$LINENO: \"$ac_link\"") >&5
  (eval $ac_link) 2>&5
 ac_status=$?
  echo "$as_me:$LINENO: \$? = $ac_status" >&5
  (exit $ac_status); }; then
  $2=" $1"
else
  $2=""
fi
if test "$ac_test_CFLAGS" = set; then
  CFLAGS=$ac_save_CFLAGS
fi
if test "$ac_test_CXXFLAGS" = set; then
  CXXFLAGS=$ac_save_CXXFLAGS
fi
])])

    

AC_SUBST(C_COMPILER)
AC_SUBST(C_PREPROC)
AC_SUBST(GAUCHE_CFLAGS)
AC_SUBST(GAUCHE_LIBS)


if test "$GCC" = yes -o "$GXX" = yes; then

  ########## Using gcc...

  # preprocessor options:
  AC_CHECK_C_COMPILER_OPT(-no-cpp-precomp,DASH_no_cpp_precomp)

  # debugging options:
  AC_CHECK_C_COMPILER_OPT(-Wall,DASH_Wall)
  AC_CHECK_C_COMPILER_OPT(-W,DASH_W)
  AC_CHECK_C_COMPILER_OPT(-Wno-unused,DASH_Wno_unused)
  AC_CHECK_C_COMPILER_OPT(-g,DASH_g)


  # code generation options:
  case "$host" in

     *mingw*) # no special code generation options needed by MinGW
             ;;

          *) # check for code generation options for other systems
             AC_CHECK_C_COMPILER_OPT(-fPIC,DASH_fPIC)
             AC_CHECK_C_COMPILER_OPT(-fpic,DASH_fpic)
             ;;

  esac
  AC_CHECK_C_COMPILER_OPT(-fno-common,DASH_fno_common)

  # linking options:
  case "$host" in

     *mingw*) # i think this is ok
             ;;

   *solaris*) # check for options needed by Solaris
             AC_CHECK_C_COMPILER_OPT(-shared,DASH_shared)
	     
             ;;

    *darwin*) # check for special options added by Apple
             AC_CHECK_C_COMPILER_OPT(-dynamiclib,DASH_dynamiclib)
             AC_CHECK_C_COMPILER_OPT(-bundle,DASH_bundle)
             AC_CHECK_C_COMPILER_OPT(-flat_namespace -undefined suppress,DASH_flat_namespace_undefined_suppress)
	    
             ;;

          *) # for other systems
             AC_CHECK_C_COMPILER_OPT(-rdynamic,DASH_rdynamic)
             AC_CHECK_C_COMPILER_OPT(-shared,DASH_shared)
	    
             ;;
  esac

  # determine which flags to add to CFLAGS, CXXFLAGS and LDFLAGS

  case "$host" in
    *darwin*) # avoid the default C preprocessor which is setup for Objective-C
             FLAGS_OBJ="$FLAGS_OBJ$DASH_no_cpp_precomp"
             FLAGS_DYN="$FLAGS_DYN$DASH_no_cpp_precomp"
             ;;
  esac

  # determine shared library extension
  
   case "$host" in
   	 *mingw*) 
	 	SOEXT="dll"
		;;
	 *darwin*)
		SOEXT="dylib"
		;;
	  *)
		SOEXT="so"
		;;
  esac

  FLAGS_OBJ="$FLAGS_OBJ$DASH_Wall$DASH_W$DASH_Wno_unused"
  FLAGS_DYN="$FLAGS_DYN$DASH_Wall$DASH_W$DASH_Wno_unused"

  if test "$ENABLE_DEBUG" = yes; then
    FLAGS_OBJ="$FLAGS_OBJ$DASH_g"
    FLAGS_DYN="$FLAGS_DYN$DASH_g"
  fi


  case "$host" in

     *mingw*) # MinGW's setjmp seems to need the frame pointer
             ;;

          *) # other systems are fine as far as I can tell
             if test "$ENABLE_DEBUG" != yes -o "$ENABLE_PROFILE" != yes; then
               FLAGS_OBJ="$FLAGS_OBJ$DASH_fomit_frame_pointer"
               FLAGS_DYN="$FLAGS_DYN$DASH_fomit_frame_pointer"
             fi
             ;;

  esac

  if test "x$DASH_fPIC" != x; then
    FLAGS_OBJ="$FLAGS_OBJ$DASH_fPIC"
    FLAGS_DYN="$FLAGS_DYN$DASH_fPIC"
  else
    FLAGS_OBJ="$FLAGS_OBJ$DASH_fpic"
    FLAGS_DYN="$FLAGS_DYN$DASH_fpic"
  fi

  FLAGS_OBJ="$FLAGS_OBJ$DASH_fno_common"
  FLAGS_DYN="$FLAGS_DYN$DASH_fno_common"

  case "$host" in

    *darwin*) # check for special options added by Apple
             FLAGS_DYN="$FLAGS_DYN$DASH_bundle$DASH_flat_namespace_undefined_suppress"
             FLAGS_LIB="$FLAGS_LIB$DASH_dynamiclib$DASH_flat_namespace_undefined_suppress"
             ;;

          *) # for other systems
             FLAGS_DYN="$FLAGS_DYN$DASH_rdynamic$DASH_shared"
             FLAGS_LIB="$FLAGS_LIB$DASH_rdynamic$DASH_shared"
             FLAGS_EXE="$FLAGS_EXE$DASH_rdynamic"
             ;;

  esac

fi



if test "$with_cffi" = yes ; then
   ARCHFILES="$ARCHFILES librts.$SOEXT"
   
fi



#this does not check for version
if test "$with_gauche" = yes ; then
  AC_PATH_PROG(GAUCHE_CONFIG, gauche-config, no)
  AC_MSG_CHECKING([for Gauche])
  if test "$GAUCHE_CONFIG" = "no" ; then
    AC_MSG_WARN([can't find Gauche!])
  else

    gauche_version="`$GAUCHE_CONFIG -V`"
    AC_MSG_RESULT($gauche_version)
    
    GAUCHE_CFLAGS=`$GAUCHE_CONFIG -I | sed 's/\x27//g'`
    GAUCHE_CFLAGS="$GAUCHE_CFLAGS -DHAVE_GAUCHE"
    AC_DEFINE(HAVE_GAUCHE)
    GAUCHE_LIBS=`$GAUCHE_CONFIG -L  | sed 's/\x27//g'`
    GAUCHE_LIBS="$GAUCHE_LIBS -lgauche"
    ARCHFILES="$ARCHFILES libgaucherts.$SOEXT"
 fi
fi



AC_SUBST(ARCHFILES)


AC_SUBST(SOEXT)
AC_SUBST(FLAGS_DYN)
AC_SUBST(FLAGS_OBJ)


AC_OUTPUT(Makefile)

