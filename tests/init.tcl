# Common initialization for each test
#
# $Id$

if {[lsearch [namespace children] ::tcltest] == -1} {
    package require tcltest
    namespace import -force ::tcltest::*
}

set tmp [file join [pwd] ..]
set ::auto_path [concat [list $tmp] $::auto_path]
if {[file exists $tmp/pdf4tcl.tcl_i]} {
    source $tmp/pdf4tcl.tcl_i
}
package require pdf4tcl
