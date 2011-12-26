/* mus-config.h.  Generated from mus-config.h.in by configure.  */
#ifndef CONFIG_H_LOADED
#define CONFIG_H_LOADED

#define RETSIGTYPE void

/* Define to `int' or something if <sys/types.h> doesn't define.  */
/* #undef mode_t */
/* #undef pid_t */
/* #undef size_t */
/* #undef off_t */

/* #undef WORDS_BIGENDIAN */

#define HAVE_GETCWD 1
#define HAVE_STRFTIME 1
#define HAVE_STRERROR 1
#define HAVE_ACCESS 1
#define HAVE_VSNPRINTF 1
#define HAVE_SNPRINTF 1
#define HAVE_MEMMOVE 1
#define HAVE_STRDUP 1
#define HAVE_FILENO 1

#define HAVE_DECL_ISNAN 1
#define HAVE_DECL_ISINF 1

#define STDC_HEADERS 1
#define HAVE_FCNTL_H 1
#define HAVE_LIMITS_H 1
#define HAVE_STRING_H 1
#define HAVE_UNISTD_H 1
#define HAVE_STDBOOL_H 1
/* #undef HAVE_SYS_SOUNDCARD_H */
/* #undef HAVE_MACHINE_SOUNDCARD_H */
/* #undef HAVE_SYS_MIXER_H */
/* #undef MUS_USR_LIB_OSS */
/* #undef MUS_USR_LOCAL_LIB_OSS */
/* #undef MUS_OPT_OSS */
/* #undef MUS_VAR_LIB_OSS */
#define HAVE_LIBC_H 1
/* #undef HAVE_ALSA_ASOUNDLIB_H */
/* #undef HAVE_BYTESWAP_H */

/* #undef _FILE_OFFSET_BITS */
/* #undef _LARGE_FILES */
#define SIZEOF_OFF_T 8

/* #undef MUS_LINUX */
/* #undef MUS_SGI */
/* #undef MUS_ALPHA */
/* #undef MUS_HPUX */
/* #undef MUS_SUN */
/* #undef MUS_OPENBSD */
/* #undef MUS_WINDOZE */
/* #undef HAVE_OSS */
/* #undef HAVE_ALSA */
/* #undef HAVE_JACK */
/* #undef HAVE_SAM_9407 */
#define MUS_MAC_OSX 1
/* #undef MUS_ESD */

#define MUS_OUT_FORMAT MUS_LDOUBLE

#define HAVE_EXTENSION_LANGUAGE 0
/* #undef SND_CONFIG_GET_ID_ARGS */
#define Float double
#define SNDLIB_USE_FLOATS 1
/* #undef MUS_SAMPLE_BITS */
/* #undef MUS_ESD_VERSION */
/* #undef MUS_AUDIOFILE_VERSION */
/* #undef WITH_MODULES */
/* #undef HAVE_KAUDIODEVICEPROPERTYTRANSPORTTYPE */
/* #undef HAVE_KLINEARPCMFORMATFLAGISNONINTERLEAVED */

#define USE_SND 0
#define CLM 1
#endif
