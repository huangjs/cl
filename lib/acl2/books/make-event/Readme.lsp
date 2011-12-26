#|

In order to learn about make-event, it may be useful to look at the files below
in the order shown, probably just those marked with + (or referred to in
comments in those files).  Typically, we add "-check" to indicate a variant
that uses :check-expansion t, and we add "-include" to suggest a book that
includes a previous book.

+ basic.lisp
    Simple examples; make-event-debug.  Suggestion: Look at the output.
+ basic-check.lisp
    Interesting experiment:
    (assign make-event-debug t)
    (include-book "basic-check") ; see checking of expansions
    :u
    (include-book "basic") ; no checking of expansions
  basic-pkg.lisp
  basic-pkg-check.lisp
+ read-from-file.lisp
    Create events by reading a file.  Includes remark explaining why we enable
    proofs during evaluation of the expansion result when :check-expansion is
    not nil.
+ proof-by-arith.lisp
    Search among different proof strategies.
+ gen-defun.lisp
    Generate events and event names based on the current logical world.
  gen-defun-check.lisp
  local-requires-skip-check.lisp
  local-requires-skip-check-include.lisp
+ defconst-fast.lisp
+ defconst-fast-examples.lisp
+ macros.lisp
    Some tests that give a deeper understand of the interaction of make-event
    with macros, local, and redundancy.
+ macros-include.lisp
    Gives deeper understanding of the expansions stored, which are checked by
    reading a .cert file inside a make-event.
  macros-skip-proofs-include.lisp
  macros-skip-proofs.lisp
+ gen-defthm.lisp
    Generate theorems using state modification during expansion.
  gen-defthm-check.lisp
+ eval.lisp
    Useful macros must-succeed and must-fail.
+ eval-tests.lisp
    Tests illustrating the use of macros must-succeed and must-fail.
  eval-check.lisp
+ eval-check-tests.lisp
    Like eval-tests.lisp, but illustrates why state global 'ld-skip-proofsp is
    set to t at the start of expansion.
+ assert.lisp
    Assertions that can be put into a book.
+ assert-include.lisp
    Check that expansion blows away the make-event if :check-expansion is nil.
+ assert-check.lisp
    Check that expansion does NOT blow away the make-event if :check-expansion
    is t (using the two books just below).
  assert-check-include-1.lisp
  assert-check-include.lisp
+ test-case.lisp
    Variant of assert.
  test-case-check.lisp
  nested.lisp
  nested-check.lisp
+ embedded-defaxioms.lisp
    Illustrates the idea of using make-event to check that books do NOT
    certify.
  portcullis-expansion.lisp
  portcullis-expansion-include.lisp
+ stobj-test.lisp
    Illustrates the use of stobjs during make-event expansion.
+ dotimes.lisp
    Defines a dotimes$ macro and provides an example.
+ logical-tangent.lisp
    Provides a wormhole-like capability, where you can experiment for awhile
    and then the built-in part of the state, including the logical world, will
    be reverted.

The rest of this file is metadata for the ACL2 system.

|#

((:FILES ; non-empty list of filenames, generated from Unix command "ls -1R"
"
.:
Makefile
Readme.lsp
assert-check-include-1.acl2
assert-check-include-1.lisp
assert-check-include.lisp
assert-check.lisp
assert-include.acl2
assert-include.lisp
assert.lisp
basic-check.lisp
basic-pkg-check.acl2
basic-pkg-check.lisp
basic-pkg.acl2
basic-pkg.lisp
basic.lisp
defconst-fast-examples.lisp
defconst-fast.lisp
dotimes.lisp
embedded-defaxioms
embedded-defaxioms.acl2
embedded-defaxioms.lisp
eval-check-tests.lisp
eval-check.lisp
eval-tests.lisp
eval.lisp
gen-defthm-check.lisp
gen-defthm.lisp
gen-defun-check.lisp
gen-defun.lisp
local-requires-skip-check-include.lisp
local-requires-skip-check.lisp
logical-tangent.lisp
macros-include.lisp
macros-skip-proofs-include.acl2
macros-skip-proofs-include.lisp
macros-skip-proofs.acl2
macros-skip-proofs.lisp
macros.lisp
nested-check.lisp
nested.lisp
portcullis-expansion-include.acl2
portcullis-expansion-include.lisp
portcullis-expansion.acl2
portcullis-expansion.lisp
proof-by-arith.lisp
read-from-file-data-mod.lsp
read-from-file-data.lsp
read-from-file.lisp
stobj-test.lisp
test-case-check.lisp
test-case.lisp

./embedded-defaxioms:
Makefile
bar.lisp
baruser.lisp
foo-a1.lisp
foo-a21.lisp
foo-b1.lisp
foo-b2.lisp
foo-b3.lisp
foo-c1.lisp
foo-c2.lisp
foo-d.lisp
foo-e.lisp
foo-f.lisp
local-defaxiom-1.lisp
local-defaxiom-2.lisp
")
 (:TITLE    "Examples illustrating the use of make-event")
 (:AUTHOR/S "M. Kaufmann" "J Moore" "David Rager" "Peter Dillinger")
 (:KEYWORDS ; non-empty list of keywords, case-insensitive
   "make-event" "assert!" "assert!!" "must-fail" "must-succeed" "must-eval-to"
   "must-eval-to-t" "test-case" "dotimes$" "logical-tangent")
 (:ABSTRACT "
The .lisp files in this directory illustrate a number of potential
uses of make-event.  In particular, eval.lisp defines some macros
that allow one to put tests into one's certifiable books; but there
are many other examples as well.")
  (:PERMISSION ; author/s permission for distribution and copying:
"make-event
Copyright (C) 2006 University of Texas at Austin
for files not explicitly copyrighted otherwise

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA."))

