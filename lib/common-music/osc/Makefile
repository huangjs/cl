C_COMPILER = gcc
C_PREPROC = 
SHELL       = /bin/sh
SOEXT = dylib
OBJEXT = o
EXEEXT = 
LDFLAGS =  -no-cpp-precomp -Wall -W -Wno-unused -fPIC -fno-common -bundle -flat_namespace -undefined suppress
CFLAGS =  -no-cpp-precomp -Wall -W -Wno-unused -fPIC -fno-common
ARCHFILES =  liboscrecv.dylib libgaucheoscrecv.dylib 
GAUCHE_CFLAGS = -I'/usr/local/lib/gauche/0.8.7/include' -DHAVE_GAUCHE
GAUCHE_LIBS = -L'/usr/local/lib/gauche/0.8.7/powerpc-apple-darwin8.4.0'  -lgauche



TARGET = $(ARCHFILES)

FASLS = *.fasl *.fas *.dfas *.dfsl *.ppcf *.lib
GENERATED = 
CONFIG_GENERATED = Makefile config.cache config.log config.status \
                   configure.lineno autom4te*.cache 

osc_recv_SRC = osc-recv.c
osc_recv_HEADERS = osc-recv.h

all : $(TARGET)

liboscrecv.$(SOEXT) : osc-recv.$(OBJEXT)
	$(C_COMPILER) $(LDFLAGS) -lportmidi -o liboscrecv.$(SOEXT) osc-recv.$(OBJEXT)

libgaucheoscrecv.$(SOEXT) : osc-recv-gauche.$(OBJEXT)
	$(C_COMPILER) $(LDFLAGS) $(GAUCHE_LIBS) -o libgaucheoscrecv.$(SOEXT) osc-recv-gauche.$(OBJEXT)

osc-recv.o : $(osc_SRC) $(osc_HEADERS)
	$(C_COMPILER) $(CFLAGS) -c $(osc_recv_SRC) -o osc-recv.$(OBJEXT)

osc-recv-gauche.o : $(osc_SRC) $(osc_HEADERS)
	$(C_COMPILER) $(CFLAGS) $(GAUCHE_CFLAGS) -c $(osc_recv_SRC) -o osc-recv-gauche.$(OBJEXT)

clean : 
	rm -f $(FASLS) *.$(OBJEXT) $(ARCHFILES) core *~ \#*

distclean : clean
	rm -f $(CONFIG_GENERATED)







