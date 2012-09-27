# -*- Mode: Makefile; tab-width: 4; indent-tabs-mode: t -*-
#
# Makefile
#
# Copyright (c) 2006-2007, Jack D. Unrue
#

VERSION      = 0.9

CHM-DEPS     = gfc-package-tmp.xml gfg-package-tmp.xml gfs-package-tmp.xml        \
               gfw-package-tmp.xml constants.xml api.xml                          \
               catalog.xml glossary.xml graphic-forms.xml image-data-plugins.xml  \
               introduction.xml legal.xml protocols.xml miscellaneous-topics.xml

COMMON-DEPS  = symbols.xsl packages.xsl clhs-table.xml win32-api-table.xml        \
               packages.xml

GFC-PKG-DEPS = gfc-class-symbols-tmp.xml gfc-function-symbols-tmp.xml gfc-macro-symbols-tmp.xml

GFG-PKG-DEPS = gfg-class-symbols-tmp.xml gfg-function-symbols-tmp.xml gfg-macro-symbols-tmp.xml

GFS-PKG-DEPS = gfs-class-symbols-tmp.xml gfs-function-symbols-tmp.xml gfs-macro-symbols-tmp.xml

GFW-PKG-DEPS = gfw-class-symbols-tmp.xml gfw-function-symbols-tmp.xml gfw-macro-symbols-tmp.xml

XSLT-PROC    = xsltproc --nonet

graphic-forms-$(VERSION).chm: $(CHM-DEPS)
	cat graphic-forms.xml | sed -e 's/VERSION/$(VERSION)/g' > tmp.xml
	$(XSLT-PROC) --stringparam version $(VERSION) graphic-forms.xsl tmp.xml
	-hhc htmlhelp.hhp; exit 0 # muffle Error Ignored msg due to hhc exit value 1
	find . \( -name "*~" -o -name "*.html" -o -name "*.hhk" -o -name "*.hhc" -o -name "*.hhp" \) -exec rm {} \;

%-symbols-tmp.xml: %-symbols.xml $(COMMON-DEPS)
	$(XSLT-PROC) --output $@ symbols.xsl $*-symbols.xml

gfc-package-tmp.xml: $(GFC-PKG-DEPS) $(COMMON-DEPS)
	$(XSLT-PROC) --stringparam nickname gfc --output $@ packages.xsl packages.xml

gfg-package-tmp.xml: $(GFG-PKG-DEPS) $(COMMON-DEPS)
	$(XSLT-PROC) --stringparam nickname gfg --output $@ packages.xsl packages.xml

gfs-package-tmp.xml: $(GFS-PKG-DEPS) $(COMMON-DEPS)
	$(XSLT-PROC) --stringparam nickname gfs --output $@ packages.xsl packages.xml

gfw-package-tmp.xml: $(GFW-PKG-DEPS) $(COMMON-DEPS)
	$(XSLT-PROC) --stringparam nickname gfw --output $@ packages.xsl packages.xml

clean:
	find . \( -name "*-tmp.xml" -o -name "tmp.xml" \) -exec rm {} \;
	find . \( -name "*~" -o -name "*.html" -o -name "*.hhk" -o -name "*.hhc" -o -name "*.hhp" \) -exec rm {} \;

scrub: clean
	find . -name "*.chm" -exec rm {} \;
