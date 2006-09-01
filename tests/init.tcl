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

proc mytest {args} {
    set pattern [lindex $args end]
    set args [lrange $args 0 end-1]

    set cmds {}
    set opts {}
    set isopt 0
    foreach arg $args {
        if {$isopt} {
            set isopt 0
            lappend opts $arg
        } elseif {[string match "-*" $arg]} {
            set isopt 1
            lappend opts $arg
        } else {
            lappend cmds $arg
        }
    }
    set pdf [eval pdf4tcl::new %AUTO% -orient 0 $opts]
    $pdf startPage
    foreach cmd $cmds {
        eval \$pdf $cmd
    }
    set res [$pdf get]
    $pdf destroy

    regexp {stream.*endstream} $res res
    regsub -all {\s+} $res " " res

    set pattern *[string trim $pattern]*
    regsub -all {\s+} $pattern " " pattern

    if {[string match *[string trim $pattern]* $res]} {
        return 1
    } else {
        return $res
    }
}
