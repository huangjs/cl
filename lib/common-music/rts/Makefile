C_COMPILER = gcc
C_PREPROC = 
SHELL       = /bin/sh
SOEXT = dylib
OBJEXT = o
EXEEXT = 
LDFLAGS =  -no-cpp-precomp -Wall -W -Wno-unused -fPIC -fno-common -bundle -flat_namespace -undefined suppress
CFLAGS =  -no-cpp-precomp -Wall -W -Wno-unused -fPIC -fno-common
ARCHFILES =  librts.dylib 
GAUCHE_CFLAGS = 
GAUCHE_LIBS = 



TARGET = $(ARCHFILES)

FASLS = *.fasl *.fas *.dfas *.dfsl *.ppcf *.lib
GENERATED = 
CONFIG_GENERATED = Makefile config.cache config.log config.status \
                   configure.lineno autom4te*.cache 

rts_SRC = scheduler.c
rts_HEADERS = scheduler.h

all : $(TARGET)

librts.$(SOEXT) : scheduler.$(OBJEXT) tools.$(OBJEXT)
	$(C_COMPILER) $(LDFLAGS) -o librts.$(SOEXT) scheduler.$(OBJEXT) tools.$(OBJEXT)

libgaucherts.$(SOEXT) : scheduler_gauche.$(OBJEXT) tools.$(OBJEXT)
	$(C_COMPILER) $(LDFLAGS) $(GAUCHE_LIBS) -o libgaucherts.$(SOEXT) scheduler_gauche.$(OBJEXT) tools.$(OBJEXT)

scheduler.o : $(rts_SRC) $(rts_HEADERS)
	$(C_COMPILER) $(CFLAGS) -c $(rts_SRC) -o scheduler.$(OBJEXT)

scheduler_gauche.o : $(rts_SRC) $(rts_HEADERS)
	$(C_COMPILER) $(CFLAGS) $(GAUCHE_CFLAGS) -c $(rts_SRC) -o scheduler_gauche.$(OBJEXT)

tools.o : tools.c
	$(C_COMPILER) $(CFLAGS) -c tools.c -o tools.$(OBJEXT)

clean : 
	rm -f $(FASLS) *.$(OBJEXT) $(ARCHFILES) core *~ \#*

distclean : clean
	rm -f $(CONFIG_GENERATED)







