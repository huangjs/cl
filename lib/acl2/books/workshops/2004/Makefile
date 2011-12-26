# Makefile for certifying workshop/2004 books.

# The following subdirectories do not have supporting materials with certifiable books:

# acl2-directions
# alves-foss-taylor
# austel
# cowles-gamboa-kuzmina
# fisler
# greve-richards-wilding
# hunt-krug-moore
# ray
# richards-greve-wilding
# toma-borrione
# young

DIRS =  cowles-gamboa/support \
	davis/support \
	gameiro-manolios/support \
	greve/support \
	legato/support \
	manolios-srinivasan/support/Supporting-Books \
	matthews-vroon/support \
	ray/support \
	roach-fraij/support \
	ruiz-et-al/support \
	sawada/support \
	schmaltz-borrione/support \
	smith-et-al/support \
	sumners-ray/support

include ../../Makefile-psubdirs

tar: clean
	@for dir in $(DIRS) ; \
	do \
	tar cvf $$dir.tar $$dir ; \
	done
