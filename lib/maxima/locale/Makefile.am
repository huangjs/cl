TRANSLATIONS = es

localedir = $(datadir)/locale

install-data-local: install-maxima-catalogs

install-maxima-catalogs:
	@for i in $(TRANSLATIONS) ; do \
		$(mkinstalldirs) $(DESTDIR)$(localedir)/$$i/LC_MESSAGES ; \
		$(INSTALL_DATA) $$i.mo $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/maxima.mo ; \
	done

# the programs we use (TODO: use configure to detect them)
MSGFMT=msgfmt --verbose
MSGMERGE=msgmerge
XGETTEXT=xgettext
XARGS=xargs

XGETTEXT_ARGS=-s -c -j

SUFFIXES=	.mo .po

.po.mo:
	$(MSGFMT) -o $@ $<

es.po: maxima.pot	# TODO: patterns
	touch $@
	$(MSGMERGE) $@ maxima.pot > $@.new && mv $@.new $@;

maxima.pot:
	touch $@
	find ../src -name "*.lisp" | $(XARGS) $(XGETTEXT) $(XGETTEXT_ARGS) -o maxima.pot

allpo: force-update maxima.pot
	@for t in $(TRANSLATIONS); do $(MAKE) $$t.po; done

allmo:
	@for t in $(TRANSLATIONS); do $(MAKE) $$t.mo; done

force-update:
	$(RM) maxima.pot

all: allmo

EXTRA_DIST = es.po es.mo

