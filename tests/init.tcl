# Common initialization for each test
#
# $Id$
#
# The mytest page as this default look:
#
# --------------------------
# |       w=800 h=1000     |
# | -----------------------|
# | |(100,900)   (800,900)||
# | |                     ||
# | |                     ||
# | |                     ||
# | |(100,200)   (800,200)||
# | -----------------------|
# |                        |
# --------------------------

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
    # Default paper has a simple size
    set opts {-orient 0 -paper {800 1000} -margin {100 0 100 200}}
    set isopt 0
    set debug 0
    foreach arg $args {
        if {$isopt} {
            set isopt 0
            lappend opts $arg
        } elseif {[string match "-debug" $arg]} {
            set debug 1
        } elseif {[string match "-*" $arg]} {
            set isopt 1
            lappend opts $arg
        } else {
            lappend cmds $arg
        }
    }
    set pdf [eval pdf4tcl::new %AUTO% $opts]
    $pdf startPage
    foreach cmd $cmds {
        eval \$pdf $cmd
    }
    set res [$pdf get]
    $pdf destroy

    if {$debug} {
        set ch [open testdebug.pdf w]
        fconfigure $ch -translation binary
        puts -nonewline $ch $res
        close $ch
        foreach app {acroread kpdf xpdf kghostview} {
            if {[auto_execok $app] ne ""} {
                exec $app testdebug.pdf
                break
            }
        }
        #file delete testdebug.pdf
    }
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
