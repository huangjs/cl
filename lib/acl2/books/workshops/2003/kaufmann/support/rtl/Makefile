TOP_MODULE = FOO
TOP_FILE = foo.v

run: diffs.out

diffs.out: bvecp.cert
	rm -f diffs.out
	diff model.lisp results/model.lisp > diffs.out
	diff bvecp.lisp results/bvecp.lisp >> diffs.out
	diff model-eq.lisp results/model-eq.lisp >> diffs.out
	@if [ ! -z "`head -1 diffs.out`" ] ; then \
	  echo "Found unexpected results." ;\
	  exit 1 ;\
	  fi

BOOKS_DIR = ../../../../..
LIB_DIR = $(BOOKS_DIR)/rtl/rel4/lib

include $(BOOKS_DIR)/Makefile-generic

ACL2 = ../../../../../../saved_acl2
INHIBIT = (assign inhibit-output-lst (list (quote prove) (quote proof-tree) (quote warning) (quote observation) (quote event)))
# We need to assign BOOKS in order to avoid attempting to certify the non-book
# model-macro-aliases.lisp.
BOOKS = bvecp-raw bvecp common model-defs model-eq model-macros model-raw model package-defs

model-defs.lisp model-eq.lisp bvecp.lisp: tool/simplify-defuns.cert tool/wrapper.cert bvecp-raw.cert
	echo "Running transform-defuns."
	echo '(acl2::value :q)' > workxxx
	echo '(acl2::lp)' >> workxxx
	echo '(include-book "tool/simplify-defuns")' >> workxxx
	echo '(include-book "tool/wrapper")' >> workxxx
	echo '(simplify-model)' >> workxxx
	echo '(acl2::value :q)' >> workxxx
	echo '(acl2::exit-lisp)' >> workxxx
	$(ACL2) < workxxx > model.lisp.out
	rm -f workxxx

model.lisp: model-defs.lisp model-macro-aliases.lisp
	cat model-defs.lisp model-macro-aliases.lisp > model.lisp

clean-extra:
	-rm -f model.lisp model-defs.lisp model-eq.lisp model.lisp bvecp.lisp

clean: clean-extra

bvecp-raw.cert: bvecp-raw.lisp
bvecp-raw.cert: model-raw.cert
bvecp-raw.cert: $(BOOKS_DIR)/rtl/rel4/support/fast-and.cert
bvecp-raw.cert: $(BOOKS_DIR)/rtl/rel4/support/bvecp-helpers.cert

bvecp.cert: bvecp.lisp
bvecp.cert: model.cert
bvecp.cert: model-eq.cert
bvecp.cert: bvecp-raw.cert

common.cert: common.lisp
common.cert: package-defs.cert
common.cert: $(LIB_DIR)/rtl.cert
common.cert: $(LIB_DIR)/rtlarr.cert
common.cert: $(LIB_DIR)/util.cert
common.cert: $(BOOKS_DIR)/misc/symbol-btree.cert
common.cert: $(BOOKS_DIR)/misc/rtl-untranslate.cert

model-defs.cert: model-defs.lisp
model-defs.cert: common.cert
model-defs.cert: model-macros.cert

model-eq.cert: model-eq.lisp
model-eq.cert: model-raw.cert
model-eq.cert: model.cert

model.cert: model.lisp
model.cert: common.cert
model.cert: model-macros.cert

model-macro-aliases.cert: model-macro-aliases.lisp

model-macros.cert: model-macros.lisp

model-raw.cert: model-raw.lisp
model-raw.cert: common.cert

package-defs.cert: package-defs.lisp
