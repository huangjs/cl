# uri.tcl --
#
#	URI parsing and fetch
#
# Copyright (c) 2000 Zveno Pty Ltd
# Steve Ball, http://www.zveno.com/
# Derived from urls.tcl by Andreas Kupries
#
# TODO:
#	Handle www-url-encoding details
#
# CVS: $Id: uri.tcl,v 1.1 2002/09/22 03:20:13 mikeclarkson Exp $

package require Tcl 8.2
package provide uri 1.1

namespace eval uri {

    namespace export split join
    namespace export resolve isrelative
    namespace export geturl
    namespace export canonicalize
    namespace export register

    variable file:counter 0
    
    # extend these variable in the coming namespaces
    variable schemes       {}
    variable schemePattern ""
    variable url           ""
    variable url2part
    array set url2part     {}

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # basic regular expressions used in URL syntax.

    namespace eval basic {
	variable	loAlpha		{[a-z]}
	variable	hiAlpha		{[A-Z]}
	variable	digit		{[0-9]}
	variable	alpha		{[a-zA-Z]}
	variable	safe		{[$_.+-]}
	variable	extra		{[!*'(,)]}
	# danger in next pattern, order important for []
	variable	national	{[][|\}\{\^~`]}
	variable	punctuation	{[<>#%"]}	;#" fake emacs hilit
	variable	reserved	{[;/?:@&=]}
	variable	hex		{[0-9A-Fa-f]}
	variable	alphaDigit	{[A-Za-z0-9]}
	variable	alphaDigitMinus	{[A-Za-z0-9-]}
	
	# next is <national | punctuation>
	variable	unsafe		{[][<>"#%\{\}|\\^~`]} ;#" emacs hilit
	variable	escape		"%${hex}${hex}"
	
	#	unreserved	= alpha | digit | safe | extra
	#	xchar		= unreserved | reserved | escape

	variable	unreserved	{[a-zA-Z0-9$_.+!*'(,)-]}
	variable	uChar		"(${unreserved}|${escape})"
	variable	xCharN		{[a-zA-Z0-9$_.+!*'(,);/?:@&=-]}
	variable	xChar		"(${xCharN}|${escape})"
	variable	digits		"${digit}+"

	variable	toplabel	\
		"(${alpha}${alphaDigitMinus}*${alphaDigit}|${alpha})"
	variable	domainlabel	\
		"(${alphaDigit}${alphaDigitMinus}*${alphaDigit}|${alphaDigit})"

	variable	hostname	\
		"((${domainlabel}\\.)*${toplabel})"
	variable	hostnumber	\
		"(${digits}\\.${digits}\\.${digits}\\.${digits})"

	variable	host		"(${hostname}|${hostnumber})"

	variable	port		$digits
	variable	hostOrPort	"${host}(:${port})?"

	variable	usrCharN	{[a-zA-Z0-9$_.+!*'(,);?&=-]}
	variable	usrChar		"(${usrCharN}|${escape})"
	variable	user		"${usrChar}*"
	variable	password	$user
	variable	login		"(${user}(:${password})?@)?${hostOrPort}"
    } ;# basic {}
}


# uri::register --
#
#	Register a scheme (and aliases) in the package. The command
#	creates a namespace below "::uri" with the same name as the
#	scheme and executes the script declaring the pattern variables
#	for this scheme in the new namespace. At last it updates the
#	uri variables keeping track of overall scheme information.
#
#	The script has to declare at least the variable "schemepart",
#	the pattern for an url of the registered scheme after the
#	scheme declaration. Not declaring this variable is an error.
#
# Arguments:
#	schemeList	Name of the scheme to register, plus aliases
#       script		Script declaring the scheme patterns
#
# Results:
#	None.

proc uri::register {schemeList script} {
    variable schemes
    variable schemePattern
    variable url
    variable url2part

    # Check scheme and its aliases for existence.
    foreach scheme $schemeList {
	if {[lsearch -exact $schemes $scheme] >= 0} {
	    return -code error \
		    "trying to register scheme (\"$scheme\") which is already known"
	}
    }

    # Get the main scheme
    set scheme  [lindex $schemeList 0]

    if {[catch {namespace eval $scheme $script} msg]} {
	catch {namespace delete $scheme}
	return -code error \
	    "error while evaluating scheme script: $msg"
    }

    if {![info exists ${scheme}::schemepart]} {
	namespace delete $scheme
	return -code error \
	    "Variable \"schemepart\" is missing."
    }

    # Now we can extend the variables which keep track of the registered schemes.

    eval lappend schemes $schemeList
    set schemePattern	"([::join $schemes |]):"

    foreach s schemeList {
	# FRINK: nocheck
	set url2part($s) "${s}:[set ${scheme}::schemepart]"
	# FRINK: nocheck
	append url "(${s}:[set ${scheme}::schemepart])|"
    }
    set url [string trimright $url |]
    return
}

# uri::split --
#
#	Splits the given <a url> into its constituents.
#
# Arguments:
#	url	the URL to split
#
# Results:
#	Tcl list containing constituents, suitable for 'array set'.

proc uri::split {url} {

    set url [string trim $url]
    set scheme {}

    # RFC 1738:	scheme = 1*[ lowalpha | digit | "+" | "-" | "." ]
    regexp -- {^([a-z0-9+-.][a-z0-9+-.]*):} $url dummy scheme

    if {$scheme == {}} {
	set scheme http
    }

    # ease maintenance: dynamic dispatch, able to handle all schemes
    # added in future!

    if {[::info procs Split[string totitle $scheme]] == {}} {
	error "unknown scheme '$scheme' in '$url'"
    }

    regsub -- "^${scheme}:" $url {} url

    set       parts(scheme) $scheme
    array set parts [Split[string totitle $scheme] $url]

    # should decode all encoded characters!

    return [array get parts]
}

proc uri::SplitFtp {url} {
    # @c Splits the given ftp-<a url> into its constituents.
    # @a url: The url to split, without! scheme specification.
    # @r List containing the constituents, suitable for 'array set'.

    # general syntax:
    # //<user>:<password>@<host>:<port>/<cwd1>/.../<cwdN>/<name>;type=<typecode>
    #
    # additional rules:
    #
    # <user>:<password> are optional, detectable by presence of @.
    # <password> is optional too.
    #
    # "//" [ <user> [":" <password> ] "@"] <host> [":" <port>] "/"
    #	<cwd1> "/" ..."/" <cwdN> "/" <name> [";type=" <typecode>]

    upvar \#0 [namespace current]::ftp::typepart ftptype

    array set parts {user {} pwd {} host {} port {} path {} type {}}

    # slash off possible type specification

    if {[regexp -indices -- "${ftptype}$" $url dummy ftype]} {

	set from	[lindex $ftype 0]
	set to		[lindex $ftype 1]

	set parts(type)	[string range   $url $from $to]

	set from	[lindex $dummy 0]
	set url		[string replace $url $from end]
    }

    # Handle user, password, host and port

    if {[string match "//*" $url]} {
	set url [string range $url 2 end]

	array set parts [GetUPHP url]
    }

    set parts(path) [string trimleft $url /]

    return [array get parts]
}

proc uri::JoinFtp args {
    array set components {
	user {} pwd {} host {} port {}
	path {} type {}
    }
    array set components $args

    set userPwd {}
    if {[string length $components(user)] || [string length $components(pwd)]} {
	set userPwd $components(user)[expr {[string length $components(pwd)] ? ":$components(pwd)" : {}}]@
    }

    set port {}
    if {[string length $components(port)]} {
	set port :$components(port)
    }

    set type {}
    if {[string length $components(type)]} {
	set type \;type=$components(type)
    }
    
    return ftp://${userPwd}$components(host)${port}/[string trimleft $components(path) /]$type
}

proc uri::SplitHttps {url} {
    uri::SplitHttp $url
}

proc uri::SplitHttp {url} {
    # @c Splits the given http-<a url> into its constituents.
    # @a url: The url to split, without! scheme specification.
    # @r List containing the constituents, suitable for 'array set'.

    # general syntax:
    # //<host>:<port>/<path>?<searchpart>
    #
    #   where <host> and <port> are as described in Section 3.1. If :<port>
    #   is omitted, the port defaults to 80.  No user name or password is
    #   allowed.  <path> is an HTTP selector, and <searchpart> is a query
    #   string. The <path> is optional, as is the <searchpart> and its
    #   preceding "?". If neither <path> nor <searchpart> is present, the "/"
    #   may also be omitted.
    #
    #   Within the <path> and <searchpart> components, "/", ";", "?" are
    #   reserved.  The "/" character may be used within HTTP to designate a
    #   hierarchical structure.
    #
    # path == <cwd1> "/" ..."/" <cwdN> "/" <name> ["#" <fragment>]

    upvar #0 [namespace current]::http::search  search
    upvar #0 [namespace current]::http::segment segment

    array set parts {host {} port {} path {} query {}}

    set searchPattern   "\\?(${search})\$"
    set fragmentPattern "#(${segment})\$"

    # slash off possible query

    if {[regexp -indices -- $searchPattern $url match query]} {
	set from [lindex $query 0]
	set to   [lindex $query 1]

	set parts(query) [string range $url $from $to]

	set url [string replace $url [lindex $match 0] end]
    }

    # slash off possible fragment

    if {[regexp -indices -- $fragmentPattern $url match fragment]} {
	set from [lindex $fragment 0]
	set to   [lindex $fragment 1]

	set parts(fragment) [string range $url $from $to]

	set url [string replace $url [lindex $match 0] end]
    }

    if {[string match "//*" $url]} {
	set url [string range $url 2 end]

	array set parts [GetHostPort url]
    }

    set parts(path) [string trimleft $url /]

    return [array get parts]
}

proc uri::JoinHttp {args} {
    eval uri::JoinHttpInner http 80 $args
}

proc uri::JoinHttps {args} {
    eval uri::JoinHttpInner https 443 $args
}

proc uri::JoinHttpInner {scheme defport args} {
    array set components [list \
	host {} port $defport path {} query {} \
    ]
    array set components $args

    set port {}
    if {[string length $components(port)] && $components(port) != $defport} {
	set port :$components(port)
    }

    set query {}
    if {[string length $components(query)]} {
	set query ?$components(query)
    }

    regsub -- {^/} $components(path) {} components(path)

    return $scheme://$components(host)$port/$components(path)$query
}

proc uri::SplitFile {url} {
    # @c Splits the given file-<a url> into its constituents.
    # @a url: The url to split, without! scheme specification.
    # @r List containing the constituents, suitable for 'array set'.

    upvar #0 [namespace current]::basic::hostname	hostname
    upvar #0 [namespace current]::basic::hostnumber	hostnumber

    if {[string match "//*" $url]} {
	set url [string range $url 2 end]

	set hostPattern "^($hostname|$hostnumber)"
	switch -exact -- $::tcl_platform(platform) {
	    windows {
		# Catch drive letter
		append hostPattern :?
	    }
	    default {
		# Proceed as usual
	    }
	}

	if {[regexp -indices -- $hostPattern $url match host]} {
	    set fh	[lindex $host 0]
	    set th	[lindex $host 1]

	    set parts(host)	[string range $url $fh $th]

	    set  matchEnd   [lindex $match 1]
	    incr matchEnd

	    set url	[string range $url $matchEnd end]
	}
    }

    set parts(path) $url

    return [array get parts]
}

proc uri::JoinFile args {
    array set components {
	host {} port {} path {}
    }
    array set components $args

    switch -exact -- $::tcl_platform(platform) {
	windows {
	    if {[string length $components(host)]} {
		return file://$components(host):$components(path)
	    } else {
		return file://$components(path)
	    }
	}
	default {
	    return file://$components(host)$components(path)
	}
    }
}

proc uri::SplitMailto {url} {
    # @c Splits the given mailto-<a url> into its constituents.
    # @a url: The url to split, without! scheme specification.
    # @r List containing the constituents, suitable for 'array set'.

    if {[regexp -- @ $url]} {
	set url [::split $url @]
	return [list user [lindex $url 0] host [lindex $url 1]]
    } else {
	return [list user $url]
    }
}

proc uri::JoinMailto args {
    array set components {
	user {} host {}
    }
    array set components $args

    return mailto:$components(user)@$components(host)
}

proc uri::GetUPHP {urlvar} {
    # @c Parse user, password host and port out of the url stored in
    # @c variable <a urlvar>.
    # @d Side effect: The extracted information is removed from the given url.
    # @r List containing the extracted information in a format suitable for
    # @r 'array set'.
    # @a urlvar: Name of the variable containing the url to parse.

    upvar \#0 [namespace current]::basic::user		user
    upvar \#0 [namespace current]::basic::password	password
    upvar \#0 [namespace current]::basic::hostname	hostname
    upvar \#0 [namespace current]::basic::hostnumber	hostnumber
    upvar \#0 [namespace current]::basic::port		port

    upvar $urlvar url

    array set parts {user {} pwd {} host {} port {}}

    # syntax
    # "//" [ <user> [":" <password> ] "@"] <host> [":" <port>] "/"
    # "//" already cut off by caller

    set upPattern "^(${user})(:(${password}))?@"

    if {[regexp -indices -- $upPattern $url match theUser c d thePassword]} {
	set fu	[lindex $theUser 0]
	set tu	[lindex $theUser 1]
	    
	set fp	[lindex $thePassword 0]
	set tp	[lindex $thePassword 1]

	set parts(user)	[string range $url $fu $tu]
	set parts(pwd)	[string range $url $fp $tp]

	set  matchEnd   [lindex $match 1]
	incr matchEnd

	set url	[string range $url $matchEnd end]
    }

    set hpPattern "^($hostname|$hostnumber)(:($port))?"

    if {[regexp -indices -- $hpPattern $url match theHost c d e f g h thePort]} {
	set fh	[lindex $theHost 0]
	set th	[lindex $theHost 1]

	set fp	[lindex $thePort 0]
	set tp	[lindex $thePort 1]

	set parts(host)	[string range $url $fh $th]
	set parts(port)	[string range $url $fp $tp]

	set  matchEnd   [lindex $match 1]
	incr matchEnd

	set url	[string range $url $matchEnd end]
    }

    return [array get parts]
}

proc uri::GetHostPort {urlvar} {
    # @c Parse host and port out of the url stored in variable <a urlvar>.
    # @d Side effect: The extracted information is removed from the given url.
    # @r List containing the extracted information in a format suitable for
    # @r 'array set'.
    # @a urlvar: Name of the variable containing the url to parse.

    upvar #0 [namespace current]::basic::hostname	hostname
    upvar #0 [namespace current]::basic::hostnumber	hostnumber
    upvar #0 [namespace current]::basic::port		port

    upvar $urlvar url

    set pattern "^(${hostname}|${hostnumber})(:(${port}))?"

    if {[regexp -indices -- $pattern $url match host c d e f g h thePort]} {
	set fromHost	[lindex $host 0]
	set toHost	[lindex $host 1]

	set fromPort	[lindex $thePort 0]
	set toPort	[lindex $thePort 1]

	set parts(host)	[string range $url $fromHost $toHost]
	set parts(port)	[string range $url $fromPort $toPort]

	set  matchEnd   [lindex $match 1]
	incr matchEnd

	set url [string range $url $matchEnd end]
    }

    return [array get parts]
}

# uri::resolve --
#
#	Resolve an arbitrary URL, given a base URL
#
# Arguments:
#	base	base URL (absolute)
#	url	arbitrary URL
#
# Results:
#	Returns a URL

proc uri::resolve {base url} {
    if {[string length $url]} {
	if {[isrelative $url]} {

	    array set baseparts [split $base]

	    switch -- $baseparts(scheme) {
		http -
		https -
		ftp -
		file {
		    if {[string match /* $url]} {
			array set relparts [split "$baseparts(scheme)://$url"]
			array set baseparts [list path $relparts(path)]
			catch {array set baseparts [list query $relparts(query)]}
			return [eval join [array get baseparts]]
		    } elseif {[string match */ $baseparts(path)]} {
			return ${base}$url
		    } else {
			set path [lreplace [::split $baseparts(path) /] end end]
			array set baseparts [list path [::join $path /]/$url]
			return [eval join [array get baseparts]]
		    }
		}

		default {
		    return -code error "unable to resolve relative URL \"$url\""
		}
	    }

	} else {
	    return $url
	}
    } else {
	return $base
    }
}

# isrelative --
#
#	Determines whether a URL is absolute or relative
#
# Arguments:
#	url	URL to check
#
# Results:
#	Returns 1 if the URL is relative, 0 otherwise

proc uri::isrelative url {
    return [expr {![regexp -- {^[a-z0-9+-.][a-z0-9+-.]*:} $url]}]
}

# geturl --
#
#	Fetch the data from an arbitrary URL.
#
#	This package provides a handler for the file:
#	scheme, since this conflicts with the file command.
#
# Arguments:
#	url	address of data resource
#	args	configuration options
#
# Results:
#	Depends on scheme

proc uri::geturl {url args} {
    array set urlparts [split $url]

    switch -- $urlparts(scheme) {
	file {
	    return [eval file_geturl [list $url] $args]
	}
	default {
	    # Load a geturl package for the scheme first and only if
	    # that fails the scheme package itself. This prevents
	    # cyclic dependencies between packages.
	    if {[catch {package require $urlparts(scheme)::geturl}]} {
		package require $urlparts(scheme)
	    }
	    return [eval [list $urlparts(scheme)::geturl $url] $args]
	}
    }
}

# uri::file_geturl --
#
#	geturl implementation for file: scheme
#
# TODO:
#	This is an initial, basic implementation.
#	Eventually want to support all options for geturl.
#
# Arguments:
#	url	URL to fetch
#	args	configuration options
#
# Results:
#	Returns data from file

proc uri::file_geturl {url args} {
    variable file:counter

    set var [namespace current]::file[incr file:counter]
    upvar #0 $var state
    array set state {data {}}

    array set parts [split $url]

    set ch [open $parts(path)]
    # Could determine text/binary from file extension,
    # except on Macintosh
    # fconfigure $ch -translation binary
    set state(data) [read $ch]
    close $ch

    return $var
}

# uri::join --
#
#	Format a URL
#
# Arguments:
#	args	components, key-value format
#
# Results:
#	A URL

proc uri::join args {
    array set components $args

    return [eval [list Join[string totitle $components(scheme)]] $args]
}

# uri::canonicalize --
#
#	Canonicalize a URL
#
# Acknowledgements:
#	Andreas Kupries, a.kupries@westend.com
#
# Arguments:
#	uri	URI (which contains a path component)
#
# Results:
#	The canonical form of the URI

proc uri::canonicalize uri {

    # Make uri canonical with respect to dots (path changing commands)
    #
    # Remove single dots (.)  => pwd not changing
    # Remove double dots (..) => gobble previous segment of path

    while {[regexp -- {/\./} $uri]} {
        regsub -all -- {/\./} $uri {/} uri
    }
    while {[regexp -- {/\.\./} $uri]} {
	if {![regsub -- {/[^./]*/\.\./} $uri {/} uri]} {
	    # The regexp found 'foo://bar.com/../baz', but this
	    # cannot be handled by the regsub. Simply remove the
	    # dots, as is done for the singles to break out of
	    # infinity.
	    regsub -- {/\.\./} $uri {/} uri
	}
    }

    return $uri
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# regular expressions covering various url schemes

# Currently known URL schemes:
#
# (RFC 1738)
# ------------------------------------------------
# scheme	basic syntax of scheme specific part
# ------------------------------------------------
# ftp		//<user>:<password>@<host>:<port>/<cwd1>/.../<cwdN>/<name>;type=<typecode>
#
# http		//<host>:<port>/<path>?<searchpart>
#
# gopher	//<host>:<port>/<gophertype><selector>
#				<gophertype><selector>%09<search>
#		<gophertype><selector>%09<search>%09<gopher+_string>
#
# mailto	<rfc822-addr-spec>
# news		<newsgroup-name>
#		<message-id>
# nntp		//<host>:<port>/<newsgroup-name>/<article-number>
# telnet	//<user>:<password>@<host>:<port>/
# wais		//<host>:<port>/<database>
#		//<host>:<port>/<database>?<search>
#		//<host>:<port>/<database>/<wtype>/<wpath>
# file		//<host>/<path>
# prospero	//<host>:<port>/<hsoname>;<field>=<value>
# ------------------------------------------------
#
# (RFC 2111)
# ------------------------------------------------
# scheme	basic syntax of scheme specific part
# ------------------------------------------------
# mid	message-id
#		message-id/content-id
# cid	content-id
# ------------------------------------------------

# FTP
uri::register ftp {
    set escape [set [namespace parent [namespace current]]::basic::escape]
    set login  [set [namespace parent [namespace current]]::basic::login]
    
    variable	charN	{[a-zA-Z0-9$_.+!*'(,)?:@&=-]}
    variable	char	"(${charN}|${escape})"
    variable	segment	"${char}*"
    variable	path	"${segment}(/${segment})*"

    variable	type		{[AaDdIi]}
    variable	typepart	";type=(${type})"
    variable	schemepart	\
		    "//${login}(/${path}(${typepart})?)?"

    variable	url		"ftp:${schemepart}"
}

# FILE
uri::register file {
    set host [set [namespace parent [namespace current]]::basic::host]
    set path [set [namespace parent [namespace current]]::ftp::path]

    variable	schemepart	"//(${host}|localhost)?/${path}"
    variable	url		"file:${schemepart}"
}

# HTTP
uri::register http {
    set escape		[set [namespace parent [namespace current]]::basic::escape]
    set hostOrPort	[set [namespace parent [namespace current]]::basic::hostOrPort]

    variable	charN		{[a-zA-Z0-9$_.+!*'(,);:@&=-]}
    variable	char		"($charN|${escape})"
    variable	segment		"${char}*"

    variable	path		"${segment}(/${segment})*"
    variable	search		$segment
    variable	schemepart	\
	    "//${hostOrPort}(/${path}(\\?${search})?)?"

    variable	url		"http:${schemepart}"
}

# GOPHER
uri::register gopher {
    set xChar		[set [namespace parent [namespace current]]::basic::xChar]
    set hostOrPort	[set [namespace parent [namespace current]]::basic::hostOrPort]
    set search		[set [namespace parent [namespace current]]::http::search]

    variable	type		$xChar
    variable	selector	"$xChar*"
    variable	string		$selector
    variable	schemepart	\
	    "//${hostOrPort}(/(${type}(${selector}(%09${search}(%09${string})?)?)?)?)?"
    variable	url		"gopher:${schemepart}"
}
	
# MAILTO
uri::register mailto {
    set xChar	[set [namespace parent [namespace current]]::basic::xChar]
    set host	[set [namespace parent [namespace current]]::basic::host]

    variable	schemepart	"$xChar+(@${host})?"
    variable	url		"mailto:${schemepart}"
}

# NEWS
uri::register news {
    set escape		[set [namespace parent [namespace current]]::basic::escape]
    set alpha		[set [namespace parent [namespace current]]::basic::alpha]
    set host		[set [namespace parent [namespace current]]::basic::host]

    variable	aCharN		{[a-zA-Z0-9$_.+!*'(,);/?:&=-]}
    variable	aChar		"($aCharN|${escape})"
    variable	gChar		{[a-zA-Z0-9$_.+-]}
    variable	group		"${alpha}${gChar}*"
    variable	article		"${aChar}+@${host}"
    variable	schemepart	"\\*|${group}|${article}"
    variable	url		"news:${schemepart}"
}

# WAIS
uri::register wais {
    set uChar		[set [namespace parent [namespace current]]::basic::xChar]
    set hostOrPort	[set [namespace parent [namespace current]]::basic::hostOrPort]
    set search		[set [namespace parent [namespace current]]::http::search]

    variable	db		"${uChar}*"
    variable	type		"${uChar}*"
    variable	path		"${uChar}*"

    variable	database	"//${hostOrPort}/${db}"
    variable	index		"//${hostOrPort}/${db}\\?${search}"
    variable	doc		"//${hostOrPort}/${db}/${type}/${path}"

    #variable	schemepart	"${doc}|${index}|${database}"

    variable schemepart \
	    "//${hostOrPort}/${db}((\\?${search})|(/${type}/${path}))?"

    variable	url		"wais:${schemepart}"
}

# PROSPERO
uri::register prospero {
    set escape		[set [namespace parent [namespace current]]::basic::escape]
    set hostOrPort	[set [namespace parent [namespace current]]::basic::hostOrPort]
    set path		[set [namespace parent [namespace current]]::ftp::path]

    variable	charN		{[a-zA-Z0-9$_.+!*'(,)?:@&-]}
    variable	char		"(${charN}|$escape)"

    variable	fieldname	"${char}*"
    variable	fieldvalue	"${char}*"
    variable	fieldspec	";${fieldname}=${fieldvalue}"

    variable	schemepart	"//${hostOrPort}/${path}(${fieldspec})*"
    variable	url		"prospero:$schemepart"
}
