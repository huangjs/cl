
DIRS = 1999 2000 2002 2003 2004 2006
include ../Makefile-psubdirs

tar:
	@for dir in $(DIRS) ; \
	do \
	(cd $$dir ; \
	$(MAKE) tar ; \
	cd ..) ; \
	done
