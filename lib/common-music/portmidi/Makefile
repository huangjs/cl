C_COMPILER = gcc
C_PREPROC = 
SHELL       = /bin/sh
SOEXT = dylib
OBJEXT = o
EXEEXT = 
LDFLAGS =  -no-cpp-precomp -Wall -W -Wno-unused -fPIC -fno-common -bundle -flat_namespace -undefined suppress
CFLAGS =  -no-cpp-precomp -Wall -W -Wno-unused -fPIC -fno-common
ARCHFILES =  libpmrecv.dylib 
GAUCHE_CFLAGS = 
GAUCHE_LIBS = 



TARGET = $(ARCHFILES)

FASLS = *.fasl *.fas *.dfas *.dfsl *.ppcf *.lib
GENERATED = 
CONFIG_GENERATED = Makefile config.cache config.log config.status \
                   configure.lineno autom4te*.cache 

pm_recv_SRC = portmidi-recv.c
pm_recv_HEADERS = portmidi-recv.h

all : $(TARGET)

libpmrecv.$(SOEXT) : portmidi-recv.$(OBJEXT)
	$(C_COMPILER) $(LDFLAGS) -lportmidi -o libpmrecv.$(SOEXT) portmidi-recv.$(OBJEXT)

libgauchepmrecv.$(SOEXT) : portmidi-recv-gauche.$(OBJEXT)
	$(C_COMPILER) $(LDFLAGS) $(GAUCHE_LIBS) -o libgauchepmrecv.$(SOEXT) portmidi-recv-gauche.$(OBJEXT)

portmidi-recv.o : $(rts_SRC) $(rts_HEADERS)
	$(C_COMPILER) $(CFLAGS) -c $(pm_recv_SRC) -o portmidi-recv.$(OBJEXT)

portmidi-recv-gauche.o : $(rts_SRC) $(rts_HEADERS)
	$(C_COMPILER) $(CFLAGS) $(GAUCHE_CFLAGS) -c $(pm_recv_SRC) -o portmidi-recv-gauche.$(OBJEXT)

clean : 
	rm -f $(FASLS) *.$(OBJEXT) $(ARCHFILES) core *~ \#*

distclean : clean
	rm -f $(CONFIG_GENERATED)







