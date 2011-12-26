#!/usr/local/bin/tcl -f
# $Id: etags.tcl,v 1.1 2002/09/05 02:54:30 mikeclarkson Exp $
#
# Make Emacs-style TAGS file for Tcl source.
# Tom Tromey <tromey@cns.caltech.edu> Mon Feb 15 1993
#

# tcltags is not part of GNU Emacs, but is distributed under the same
# terms (IE the GNU Public License).  tcltags is really only useful
# with GNU Emacs anyway.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY.  No author or distributor
# accepts responsibility to anyone for the consequences of using it
# or for whether it serves any particular purpose or works at all,
# unless he says so in writing.  Refer to the GNU Emacs General Public
# License for full details.

# Everyone is granted permission to copy, modify and redistribute
# GNU Emacs, but only under the conditions described in the
# GNU Emacs General Public License.   A copy of this license is
# supposed to have been given to you along with GNU Emacs so you
# can know your rights and responsibilities.  It should be in a
# file named COPYING.  Among other things, the copyright notice
# and this notice must be preserved on all copies.


# KNOWN BUGS:
# * Should support updating existing tags files, ctags format, etc.
# * Should integrate with etags program somehow.

# Configuration stuff:
set verbose 1

#
# "rexp" is an array of regular expressions.  Each must have exactly one
# parenthesized subexpression, which should match the tag exactly.
# The array indices are unimportant.  The regexp as a whole should
# match the line containing the tag, up to the tag but not past it.
#
# Bogus quoting gyrations because Tcl regexps interpret \t as
# "t" and not TAB.
set rexp(proc) "^proc\[\ \t\]+(\[^\ \t\]+)"

# Next two are for local Tcl procs, for example purposes only.
# I can't give out defvar and defoption, sorry.
# set rexp(defvar) "^defvar\[\ \t\]+(\[^\ \t\]+)"
# set rexp(defoption) "^defoption\[\ \t\]+(\[^\ \t\]+)"

# Next two are for ObjTix classes, and Objected Tcl classes.
set rexp(objtix_class) "^objtix_class\[\ \t\]+(\[^\ \t\]+)"
set rexp(objtix_widget_class) "^objtix_widget_class\[\ \t\]+(\[^\ \t\]+)"
set rexp(object_class) "^object_class\[\ \t\]+(\[^\ \t\]+)"
set rexp(object_frame) "^object_frame\[\ \t\]+(\[^\ \t\]+)"
set rexp(object_toplevel) "^object_toplevel\[\ \t\]+(\[^\ \t\]+)"

#
# Figure out tags for one file.
#
proc tagify_file {file TAGS} {
  global rexp verbose

  if {$verbose} {
    puts stderr "Doing $file..." nonewline
  }

  set f [open $file r]
  set where 0
  set lineNo 0
  while {[gets $f line] >= 0} {
    foreach try [array names rexp] {
      if [regexp $rexp($try) $line match tag] then {
	if [info exists fileTags($tag)] then {
	  puts stderr "\n\tDuplicate tag $tag, ignoring"
	} else {
	  set fileTags($tag) $match
	  append fileTags($tag) \177 $lineNo,$where \n
	}
	break
      }
    }
    incr where [string length $line]
    incr lineNo
  }
  close $f

  # Now sort list by tag, and create entry, but only if a tag was
  # found.
  set entry {}
  if [string length [info locals fileTags]] then {
    foreach tag [lsort [array names fileTags]] {
      append entry $fileTags($tag)
    }
  }

  # Write file part and then entry to TAGS file.
  puts $TAGS \014
  puts $TAGS $file,[string length $entry]
  puts $TAGS $entry nonewline

  if $verbose then {
    puts stderr done
  }
}

# Open output file
set TAGS [open TAGS w]

# Munge every file listed on the command line.
foreach file $argv {
  tagify_file $file $TAGS
}

close $TAGS
