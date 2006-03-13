# Common initialization for each test
#
# $Id$

if {[lsearch [namespace children] ::tcltest] == -1} {
    package require tcltest
    namespace import -force ::tcltest::*
}

set ::auto_path [concat [list [file join [pwd] ..]] $::auto_path]
package require pdf4tcl
