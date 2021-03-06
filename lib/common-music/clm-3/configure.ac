dnl Configuration script for CLM (taken from Snd)
dnl Process this file with autoconf 2.56 to produce a configure script.

AC_INIT(CLM, 3, bil@ccrma.stanford.edu)
CLM_VERSION=3
AC_CONFIG_SRCDIR(clm.c)
AC_CANONICAL_HOST
AC_CONFIG_HEADERS(mus-config.h sndlib.h)
# no makefile -- actual compilation goes through all.lisp (and probably shouldn't!)
#   TODO: make a makefile that creates libclm.so
AC_PROG_CC
AC_HEADER_STDC

audio_system="unknown"

#--------------------------------------------------------------------------------
# configuration options
#   --with-alsa           use ALSA if possible
#   --with-jack           use Jack
#   --with-static-alsa    use ALSA statically loaded (for RPM generation)
#   --with-doubles        use doubles throughout (default is floats)
#   --with-float-samples  represent samples internally as floats 
#   --with-sample-width=N use N bits of samples (default = 24)
#   --with-esd            use Enlightened Sound Daemon
#   --with-static-gsl     try to statically load GSL
#   --with-modules        put sndlib names into modules
#--------------------------------------------------------------------------------

AC_ARG_WITH(esd,	[  --with-esd		  use ESD])
AC_ARG_WITH(alsa,	[  --with-alsa		  use ALSA])
AC_ARG_WITH(jack,       [  --with-jack		  use JACK])
AC_ARG_WITH(static-alsa,[  --with-static-alsa	  use ALSA statically loaded])
AC_ARG_WITH(doubles,	[  --with-doubles	  use doubles throughout])
AC_ARG_WITH(modules,    [  --with-modules	  use if sndlib uses modules])

if test "$with_doubles" = yes; then
  AC_DEFINE(Float,double)
else
  AC_DEFINE(Float,float)
fi

SNDLIB_LANGUAGE="None"
SNDLIB_BITS="24"
AC_ARG_WITH(float-samples,
	[  --with-float-samples	  use floats as the internal sample respresentation],
  	if test "$with_float_samples" = yes ; then
	    AC_DEFINE(SNDLIB_USE_FLOATS)
	    SNDLIB_BITS="0"
	fi)

AC_ARG_WITH(sample-width,
	[  --with-sample-width=N   use N bits of samples],
	AC_MSG_RESULT(Using $with_sample_width bit samples)
	AC_DEFINE_UNQUOTED(MUS_SAMPLE_BITS,$with_sample_width)
        SNDLIB_BITS=$with_sample_width
	)
AC_SUBST(SNDLIB_BITS)


#--------------------------------------------------------------------------------
# standard libraries, header files, functions, OSS special cases
#--------------------------------------------------------------------------------

AC_PROG_INSTALL

AC_CHECK_LIB(m,main)
AC_CHECK_LIB(c,main)
AC_CHECK_LIB(dl,main)
LIBS=""

AC_CHECK_HEADERS(fcntl.h limits.h unistd.h string.h sys/soundcard.h machine/soundcard.h sys/mixer.h byteswap.h stdbool.h libc.h)

AC_CHECK_HEADER(/usr/local/lib/oss/include/sys/soundcard.h,[AC_DEFINE(MUS_USR_LOCAL_LIB_OSS)])
AC_CHECK_HEADER(/usr/lib/oss/include/sys/soundcard.h,[AC_DEFINE(MUS_USR_LIB_OSS)])
AC_CHECK_HEADER(/opt/oss/include/sys/soundcard.h,[AC_DEFINE(MUS_OPT_OSS)])
AC_CHECK_HEADER(/var/lib/oss/include/sys/soundcard.h,[AC_DEFINE(MUS_VAR_LIB_OSS)])
AC_CHECK_HEADER(sys/sam9407.h,[AC_DEFINE(HAVE_SAM_9407)])
AC_CHECK_HEADER(alsa/asoundlib.h,[AC_DEFINE(HAVE_ALSA_ASOUNDLIB_H)])

AC_TYPE_MODE_T
AC_TYPE_SIZE_T
AC_TYPE_PID_T

AC_C_BIGENDIAN( [AC_DEFINE(MUS_LITTLE_ENDIAN, 0)
		 AC_DEFINE(WORDS_BIGENDIAN)
    		 case "$host" in
      		  *-apple-*)
        		AC_DEFINE(MUS_AUDIO_COMPATIBLE_FORMAT, MUS_BFLOAT)
        		;;
      		  *)
			AC_DEFINE(MUS_AUDIO_COMPATIBLE_FORMAT, MUS_BSHORT)
			;;
    		 esac
		 if test "$with_float_samples" != no ; then
		    if test "$with_doubles" = yes ; then
                      AC_DEFINE(MUS_OUT_FORMAT, MUS_BDOUBLE)
                    else
                      AC_DEFINE(MUS_OUT_FORMAT, MUS_BFLOAT)
                    fi
                  else
                   AC_DEFINE(MUS_OUT_FORMAT, MUS_BINT)
 		 fi
 		],
		[AC_DEFINE(MUS_LITTLE_ENDIAN, 1)
    		 case "$host" in
      		  *-apple-*)
        		AC_DEFINE(MUS_AUDIO_COMPATIBLE_FORMAT, MUS_LFLOAT)
        		;;
      		  *)
			AC_DEFINE(MUS_AUDIO_COMPATIBLE_FORMAT, MUS_LSHORT)
			;;
    		 esac
		 if test "$with_float_samples" != no ; then
		    if test "$with_doubles" = yes ; then
                      AC_DEFINE(MUS_OUT_FORMAT, MUS_LDOUBLE)
                    else
                      AC_DEFINE(MUS_OUT_FORMAT, MUS_LFLOAT)
                    fi
                  else
                   AC_DEFINE(MUS_OUT_FORMAT, MUS_LINT)
 		 fi
		])

AC_TYPE_OFF_T
AC_SYS_LARGEFILE
AC_CHECK_SIZEOF(off_t)

AC_CHECK_DECLS(isnan,,,[#include <math.h>])
AC_CHECK_DECLS(isinf,,,[#include <math.h>])

AC_TYPE_SIGNAL
AC_CHECK_FUNCS(getcwd strerror access vsnprintf snprintf memmove strdup fileno strftime)

AC_DEFINE(HAVE_EXTENSION_LANGUAGE,0)


#--------------------------------------------------------------------------------
# Audio library
#--------------------------------------------------------------------------------

AUDIO_LIB=""
LDSO_FLAGS=""
SO_FLAGS=""
SO_LD="ld"
SO_INSTALL="install"
A_LD="ld"
A_LD_FLAGS=""
LD_FLAGS="-r"

dnl we need the sndlib.h equivalents to try to find the native sound support (see config.guess)
dnl this only matters for those cases where we've implemented the audio code in audio.c
dnl test for ALSA courtesy of Paul Barton-Davis
dnl test for ESD courtesy of Nick Bailey
dnl test for BSD courtesy of Steven Schultz
dnl test for Jack courtesy of Kjetil S. Matheussen

if test "$with_esd" = yes ; then
  AC_CHECK_LIB(esd,main,[
    AC_CHECK_HEADER(esd.h,
      [
	AC_DEFINE(MUS_ESD)
	esd_version="`esd-config --version`"
        AC_DEFINE_UNQUOTED(MUS_ESD_VERSION,"${esd_version}")
        audiofile_version="`audiofile-config --version`"
        AC_DEFINE_UNQUOTED(MUS_AUDIOFILE_VERSION,"${audiofile_version}")
        AUDIO_LIB="`esd-config --libs`"
#       ESD_CFLAGS="`esd-config --cflags`"
	AC_SUBST(AUDIO_LIB)
        audio_system=ESD
      ],
      [AC_MSG_WARN(can't find the ESD header files)])],
    [AC_MSG_WARN(can't find the ESD library)])
fi

if test "$audio_system" != unknown ; then
  AC_MSG_RESULT(Using the $audio_system audio system)
else

case "$host" in
    *-*-linux*) 
	AC_DEFINE(MUS_LINUX)
        LDSO_FLAGS="-shared"
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi
        SO_LD="gcc"
	A_LD="ld"
dnl     This ^ used to be gcc, but that no longer seems to work
        LIBS="-L/usr/lib -lm"
dnl     This ^ used to be "" but in redhat 7 the -lm needs to be explicit for make sndsine etc
dnl     it was "" to cancel -lXpm without the needed -L<dir> for the same case
        audio_system=OSS

	if test "$with_alsa" = yes || test "$with_static_alsa" = yes ; then 
          AC_CHECK_LIB(asound,main,[ 
	    AC_CHECK_HEADER(sys/asoundlib.h,
                [audio_system=ALSA],
		[AC_CHECK_HEADER(alsa/asoundlib.h,
                    [audio_system=ALSA],
   	            [AC_MSG_WARN(can't find the ALSA header files)])])],
          [AC_MSG_WARN(can't find the ALSA library)])
	else
	  if test "$with_jack" = yes ; then 
            AC_CHECK_LIB(jack,main,[ 
	      AC_CHECK_HEADER(jack/jack.h,
                  [audio_system=JACK],
   	          [with_jack=no
                   AC_MSG_WARN(can't find the JACK header files)])],
              [with_jack=no
	       AC_MSG_WARN(can't find the JACK library)])
          fi
        fi

	case $audio_system in
	    ALSA)
	    	AC_DEFINE(HAVE_ALSA)
		if test "$with_static_alsa" = yes ; then
                  AUDIO_LIB="/usr/lib/libasound.a"
		else
	    	  AUDIO_LIB="-lasound"
		fi
	    	AC_SUBST(AUDIO_LIB)
# snd_config_get_id argnum changed in Nov-01
                snd_config_get_id_args=1
                AC_MSG_CHECKING([snd_config_get_id args])
                AC_COMPILE_IFELSE(
		  [AC_LANG_PROGRAM([#include <alsa/asoundlib.h>],
	          		   [snd_config_t *c; 
				    const char *id; 
		      		    snd_config_get_id(c, &id)])],
       		  snd_config_get_id_args=2)
                AC_MSG_RESULT($snd_config_get_id_args)
                AC_DEFINE_UNQUOTED(SND_CONFIG_GET_ID_ARGS,$snd_config_get_id_args)
		if test "$with_jack" = yes ; then 
		  if test "$with_static_alsa" = yes ; then
		    AUDIO_LIB="/usr/lib/libasound.a -ljack -l samplerate"
		  else
		    AUDIO_LIB="-lasound -ljack -lsamplerate"
                  fi
		 AC_DEFINE(HAVE_JACK)
 		fi
 		;;
	    JACK)
		AC_DEFINE(HAVE_JACK)
		AUDIO_LIB="-ljack -lsamplerate"
		;;
	    OSS)
		AC_DEFINE(HAVE_OSS)
		;;
	esac
	;;
    *-*-sunos4*) 
        AC_DEFINE(MUS_SUN)
	LIBS="-lm"
	audio_system=Sun
        ;;
    *-*-solaris*) 
	AC_DEFINE(MUS_SUN)
#	LIBS="-lm -ldl"
	LIBS="-lm"
	audio_system=Sun
	LDSO_FLAGS="-G"
        ;;
    *-*-hpux*) 
        AC_DEFINE(MUS_HPUX)
	audio_system=Hpux
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi
	LDSO_FLAGS="+z -Ae +DA1.1"
        ;;
    *-sgi*) 
        AC_DEFINE(MUS_SGI)
	audio_system=SGI
	AUDIO_LIB="-laudio -lmd"
	AC_SUBST(AUDIO_LIB)
        if test $GUILE_CONFIG_works = no; then
 	  LIBS="-lm -lc"
        else
	  LIBS="$LIBS -lm -lc"
        fi
        LDSO_FLAGS=""
	SO_INSTALL=":"
	SO_LD=":"
	LD_FLAGS=""
	if test $GCC = yes; then
	  LD_FLAGS=""
        fi
        ;;
    alpha*) 
        AC_DEFINE(MUS_ALPHA)
        ;;
    *-*-bsdi*) 
	AC_DEFINE(HAVE_OSS)
	LIBS="-lm"
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi
	audio_system=OSS
        ;;
    *-*-freebsd*)
	AC_DEFINE(HAVE_OSS)
	LIBS="-lm"
	audio_system=OSS
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi
        SO_LD="gcc -shared"
        A_LD="gcc"
	;;
    *-*-openbsd*) 
        AC_DEFINE(MUS_OPENBSD)
	audio_system=Sun
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi
        ;;
    *-*-netbsd*) 
        AC_DEFINE(MUS_NETBSD)
	audio_system=NetBSD
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi
	;;
    *-*-cygwin*)
	AC_DEFINE(MUS_WINDOZE)
	audio_system=Windoze
	;;
    *-apple-*)
        LDSO_FLAGS=""
	SO_INSTALL=":"
	SO_LD=":"
	AC_DEFINE(MUS_MAC_OSX)
	audio_system=MacOSX
	AUDIO_LIB="-framework CoreAudio -framework CoreFoundation -framework CoreMIDI"
	AC_SUBST(AUDIO_LIB)
	AC_MSG_CHECKING([for kAudioDevicePropertyDeviceManufacturer])
	AC_COMPILE_IFELSE(
          [AC_LANG_PROGRAM([#include <CoreServices/CoreServices.h>
                            #include <CoreAudio/CoreAudio.h>],
			   [AudioDeviceID deviceID; 
			    UInt32 trans_size = 0, trans;
      			    trans_size = sizeof(UInt32);
     			    AudioDeviceGetProperty(deviceID, 0, true, kAudioDevicePropertyTransportType, &trans_size, &trans)
			   ])],
	  [AC_DEFINE(HAVE_KAUDIODEVICEPROPERTYTRANSPORTTYPE)
	   AC_MSG_RESULT(yes)],
	  [AC_MSG_RESULT(no)])
	AC_MSG_CHECKING([for kLinearPCMFormatFlagIsNonInterleaved])
	AC_COMPILE_IFELSE(
          [AC_LANG_PROGRAM([#include <CoreServices/CoreServices.h>
                            #include <CoreAudio/CoreAudio.h>],
			   [int i; i = kLinearPCMFormatFlagIsNonInterleaved])],
	  [AC_DEFINE(HAVE_KLINEARPCMFORMATFLAGISNONINTERLEAVED)
	   AC_MSG_RESULT(yes)],
	  [AC_MSG_RESULT(no)])
	;;
esac
AC_MSG_CHECKING([for audio system])
AC_MSG_RESULT($audio_system)
fi

CFLAGS="-I. $CFLAGS"
# needed since we're looking for <config.h>, I think

AC_SUBST(LDSO_FLAGS)
AC_SUBST(SO_FLAGS)
AC_SUBST(SO_INSTALL)
AC_SUBST(SO_LD)
AC_SUBST(A_LD)
AC_SUBST(A_LD_FLAGS)
AC_SUBST(LD_FLAGS)
AC_SUBST(SNDLIB_VERSION)
AC_SUBST(SNDLIB_LANGUAGE)
AC_SUBST(SNDLIB_MODULES)

AC_OUTPUT

