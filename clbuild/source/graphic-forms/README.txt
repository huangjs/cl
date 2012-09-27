
Graphic-Forms README for version 0.9.0 (xx xxxxxx 2007)
Copyright (c) 2006-2007, Jack D. Unrue

Graphic-Forms is a user interface library implemented in Common Lisp focusing
on the Windows(R) platform. Graphic-Forms is licensed under the terms of the
BSD License.


Dependencies
------------

Graphic-Forms requires the following libraries which must be downloaded
separately:

 - ASDF
   http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/cclan/asdf/
   *note: ASDF is bundled with SBCL*

 - CFFI (cffi-070816 or later)
   http://common-lisp.net/project/cffi/

 - Closer to MOP
   http://common-lisp.net/project/closer/downloads.html

 - lw-compat
   http://common-lisp.net/project/closer/downloads.html

The following libraries are bundled with Graphic-Forms:

 - Practical Common Lisp Chapter08 and Chapter24
   http://www.gigamonkeys.com/book/practicals-1.0.3.tar.gz

 - lisp-unit
   http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html

The following libraries are optional:

 - ImageMagick 6.2.6.5-Q16
   http://www.imagemagick.org/download/binaries/ImageMagick-6.2.6-5-Q16-windows-dll.exe


Supported Common Lisp Implementations
-------------------------------------

Graphic-Forms currently supports Allegro CL 8.0, CLISP 2.40 or higher,
LispWorks 5.0.1, and SBCL 1.0.5 or higher (with a small patch).


Known Problems
--------------

Aside from the fact that there are a myriad of classes, functions, and
features in general that are not yet implemented, this section lists
known problems in this release:

1. The following bug filed against CLISP 2.38

   http://sourceforge.net/tracker/index.php?func=detail&aid=1463994&group_id=1355&atid=101355

   may result in a GPF if a window's layout manager is changed. Compared
   to prior releases of Graphic-Forms, there is much less chance of this
   problem affecting layout management.

2. Please be advised that SBCL is itself still in the early stages of
   supporting Windows, and as a consequence, you may experience problems
   such as 'GC invariant lost' errors that result in a crash to LDB.

3. The 'unblocked' and 'textedit' demo programs are not yet complete.

4. The gfg:text-extent method currently does not return the correct text
   height value. As a workaround, get the text metrics for the font and
   compute height from that. The gfg:text-extent function does return
   the correct width.

5. If a Graphic-Forms application is launched from within SLIME with
   CLISP or SBCL as the backend (both of which are single-threaded on
   Win32), further SLIME commands will be 'pipelined' until the
   Graphic-Forms main message loop exits. If/when these implementations
   gain multi-threading support on Win32, then the Graphic-Forms library
   code will be updated to launch a separate thread, as is currently done
   for Allegro and LispWorks.


How To Configure and Build
--------------------------

NOTE: in a future release, this library will be packaged for delivery
via asdf-install.

NOTE: the following steps are only suggestions provided as a default
procedure for people new to Graphic-Forms or Common Lisp.

1. [OPTIONAL] Install ImageMagick 6.2.6.5-Q16 (note in particular that it
   is the Q16 version that is needed, not the Q8 version). The default
   installation directory is "c:/Program Files/ImageMagick-6.2.6-Q16/".

2. Extract the Graphic-Forms distribution archive somewhere on your
   machine (or check out the source from Subversion).

3. Change to the Graphic-Forms top-level directory.

4. Load ASDF into your Lisp image if it is not already present. Note that
   SBCL bundles ASDF, so in this case you just need to (require 'asdf)

5. Execute the following forms at your REPL

  ;;
  ;; If you need the ImageMagick plugin, execute:

  (push :load-imagemagick-plugin *features*)
  (setf cl-user::*magick-library-directory* "c:/path/to/ImageMagick/")

  ;; ... the latter being necessary only if ImageMagick is not installed
  ;; in the default location.

  ;;
  ;; Next, execute:

  (load "config.lisp")

  ;;
  ;; Set these variables as needed for your specific environment to
  ;; load the other dependencies besides ImageMagick. Or if your Lisp
  ;; image already has these systems loaded, set the variables to nil.
  ;;
  ;; Note that *gf-dir* should be the Graphic-Forms top-level directory
  ;; path.
  ;;
  ;;   gfsys::*cffi-dir*
  ;;   gfsys::*closer-mop-dir*
  ;;   gfsys::*gf-dir*
  ;;   gfsys::*lw-compat-dir*

  ;;
  ;; Execute the following form to populate asdf:*central-registry*
  ;; Note that it will skip any systems whose location variables were
  ;; set to nil in the previous step.

  (gfsys::configure-asdf)

  ;;
  ;; Now load the graphic-forms system and its dependencies.

  (asdf:operate 'asdf:load-op :graphic-forms-uitoolkit)

6. You may optionally compile the reference manual. See the README.txt
   file in the docs/manual subdirectory for more details.

7. Proceed to the next section to run the tests, or start coding!


How To Run Tests And Demos
--------------------------

1. Load the graphic-forms-uitoolkit system as described in the previous
   section.

2. Execute the following forms from your REPL:

  ;;
  ;; configure ASDF for the test programs and then load it

  (load "tests.lisp")
  (gfsys::load-tests)

  ;;
  ;; execute demos and test programs
  ;;
  (gft:hello-world)

  (gft:unblocked)

  (gft:textedit)

  ;;
  ;; see src/tests/uitoolkit/README.txt
  ;; for details on other test programs

  ;;
  ;; execute the unit-tests
  ;;
  (in-package :gft)
  (run-tests)


Feedback and Bug Reports
------------------------

Please provide feedback via the following channels:

The development mailing list:
  http://www.common-lisp.net/mailman/listinfo/graphic-forms-devel

The bug tracking system:
  http://sourceforge.net/tracker/?group_id=163034&atid=826145

The patch tracker:
  http://sourceforge.net/tracker/?group_id=163034&atid=826147


[the end]
