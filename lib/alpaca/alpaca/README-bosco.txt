[Alpaca 0.5 is built using the Bosco application framework.]

Bosco 0.3
---------
copyright 2003 by mikel evins

You may use this software for any purpose you like, subject to the
terms of the LLGPL. See the accompanying file "LICENSE" for details.

INTRODUCTION
------------

Bosco is a simple template for a Cocoa application written in
OpenMCL. It is designed to make it easy to begin work on a Mac OS X
application in OpenMCL, an application that has all the familiar
features of other Cocoa applications on Mac OS X.

By following the build instructions in this document you can quickly
have a working Cocoa application. That application will have only a
few features:

- It launches from a double-click in the Finder

- It has the standard set of menus and menu items, and displays a
  single empty window

- It has an application delegate, defined in Lisp code, that you can
  modify to add features to the application

- It supports remote connections to a read-eval-print loop, for
  debugging and experimentation while the application is running

BUILDING
--------

To build Bosco, follow these steps:

1. Obtain and install OpenMCL 0.14 from: 

   http://openmcl.clozure.com/Distributions/index.html

   I recommend using version 0.14-030901 or later. Follow the
   instructions at the OpenMCL site to download and install OpenMCL
   and the accompanying interface database for DarwinPPC/Mac OS
   X. Satisfy yourself that your installation of OpenMCL works
   properly before continuing.

2. Unpack the Bosco distribution some place convenient.

3. In a Terminal window, cd to the root bosco directory. Issuing "ls"
   in that directory should show the following listing:

     LICENSE  README.txt  asdf.lisp  bin/  bosco.asd  src/

4. Copy asdf.lisp to your OpenMCL library directory. For example, if
   you installed OpenMCL at

     /usr/local/openmcl/ccl

   then you should copy asdf.lisp to 

     /usr/local/openmcl/ccl/library/asdf.lisp

5. From the root Boosco directory, Execute the following expression to
   build Bosco:

     openmcl -l bosco.asd -e "(make)"

   OpenMCL loads bosco.asd, then evaluates the form "(make)", saving
   a working Lisp image into the proper subdirectory of bin/Bosco.app

6. Test the built application. You can double-click bin/Bosco.app in
   the Finder, but you will see more interesting output in the
   Terminal if you execute

     bin/Bosco.app/Contents/MacOS/dppccl

   The Bosco application should launch.

7. Connect a remote repl session. With Bosco still running, open
   another Terminal window and execute the following command:

     telnet localhost 10101

   You should see output something like this:

     telnet: connect to address ::1: Connection refused
     Trying ::1...
     Trying 127.0.0.1...
     Connected to localhost.
     Escape character is '^]'.
     Welcome to OpenMCL Version (Alpha: Darwin) 0.14-030901!
     ?

   You are now connected to the Lisp image that is running Bosco, and
   all of Common Lisp is available to you. Most of Bosco's code is
   defined in the "CCL" package, so you may want to start by
   evaluating

     ? (in-package :ccl)

   You can then message Objective C objects and evaluate lisp
   code. For example:

     ? (setq del (send *nsapp* 'delegate))
     #<A Mac Pointer #x435610>
     ? (description del)
     "<LispApplicationDelegate: 0x435610>"

   You may want to start an inferior shell in Emacs and start the
   remote repl session from there; that way you have the very handy
   Emacs editing commands available while debugging. The OpenMCL line
   editor is regrettably too primitive for comfort.

MODIFYING
---------

To modify Bosco to create your own Cocoa application in Lisp, follow
these steps:


1. Modify the nibfiles in
   bin/Bosco.app/Contents/Resources/English.lproj to change or add
   menus, windows, and other resources. Add additional nibfiles as
   needed.

2. Modify and add definitions to main.lisp and delegate.lisp to
   implement the classes and methods that you declare in the
   nibfiles. Add additional Lisp source files as it suits you. When
   you add new Lisp source files, be sure to add them to the system
   definition in bosco.asd so that they will be compiled into the
   Bosco image.

3. Edit the Info.plist, Bosco.icns, and InfoPlist.strings files to
   change the version strings, document types, and other application
   features as needed.
