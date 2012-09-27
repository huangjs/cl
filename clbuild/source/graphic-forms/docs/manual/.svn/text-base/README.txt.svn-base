
The Programming Reference source consists of XML-based source files,
some of which are DocBook files and others which are transformed
into DocBook, along with custom XSLT and CSS files, and a catalog
file for resolving URIs. Several utilities comprise the translation
process from sources into HTML Help (CHM) format.

Before anything else, you should install a version of GNU Make
for Windows, since the docs build process is managed with a
makefile. Just install MSYS or Cygwin, if you haven't already.

Next, you will need a version of xsltproc and its dependencies. The
version of xsltproc that I have had success using is available from:

  http://www.zlatkovic.com/libxml.en.html

Download and install the following packages:

 - libxslt-1.1.17.win32.zip
 - libxml2-2.6.26.win32.zip
 - iconv-1.9.2.win32.zip
 - zlib-1.2.3.win32.zip

Note: I did not have success with libxslt from GnuWin32 so I would not
recommend using that version.

Third, you will need the hhc.exe command-line compiler from the
HTML Help Workshop, available here:

  http://go.microsoft.com/fwlink/?LinkId=14188

Make sure that your PATH is updated so that the executables and DLLs
obtained from downloading all of those packages can be found.

In order to translate from DocBook into HTML Help source and then into
a CHM file, open a command prompt and cd into the docs/manual
subdirectory underneath where you installed the Graphic-Forms source.

Modify the URI values in catalog.xml to suit your particular
environment, then run make on Makefile. A file named
graphic-forms-<version>.chm is created in the current directory.
<version> is the version of Graphic-Forms. Double-click on the
CHM file and start enjoying the Programming Reference :-)


More information about configuring DocBook and xsltproc, as well
as a quick tutorial on the whole process, see these links:

  http://www.pnotepad.org/devlog/archives/000173.html
  http://www.codeproject.com/winhelp/docbook_howto.asp

[the end]
