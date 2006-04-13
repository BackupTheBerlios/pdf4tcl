#!/usr/bin/env tclsh

# helper application -- extract char widths from afm file
# write a list of lists (char - width) to stdout

# Copyright (c) 2004 by Frank Richter <frichter@truckle.in-chemnitz.de>
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# 2004-11-30  Frank Richter  output glyph names instead of character codes

set resultfile [lindex $argv 0]
if {[file exists $resultfile]} {
    source $resultfile
    if {[info exists pdf4tcl::font_widths]} {
        array set font_widths [array get pdf4tcl::font_widths]
    }
}

set lpath {}
lappend lpath {/usr/share/fonts/afms/adobe}
lappend lpath {/usr/lib/openoffice/share/psprint/fontmetric/}
lappend lpath afm

foreach path $lpath {
    foreach file [glob -nocomplain [file join $path "*.afm"]] {
        set if [open $file "r"]
        set inMetrics 0
        set fontname ""
        set fixed 0
        set ascend 0
        set descend 0
        array unset widths
        array set widths {}
        while {[gets $if line]!=-1} {
            if {[regexp {^FontName\s*([^\s]*)} $line dummy match]} {
                set fontname $match
                if {![info exists font_widths($fontname)]} {
                    break
                }
            }
            if {[regexp {^IsFixedPitch\s*([^\s]*)} $line dummy match]} {
                set fixed [expr {!!$match}]
            }
            if {[regexp {^Ascender\s*([^\s]*)} $line dummy match]} {
                set ascend $match
            }
            if {[regexp {^Descender\s*([^\s]*)} $line dummy match]} {
                set descend $match
            }
            if {! $inMetrics} {
                if {[regexp {^StartCharMetrics} $line]} {
                    set inMetrics 1
                }
            } else {
                if {[regexp {^EndCharMetrics} $line]} {
                    break
                }
                if {[regexp {^C\s(-?\d*)\s*;\s*WX\s*(\d*)\s*;\s*N\s*([^;]*);} $line dummy ch w glyph_name]} {
                    set char [format "%c" $ch]
                    set glyph_name [string trim $glyph_name]
                    scan $w "%d" w
                    #set widths($char) $w
                    set widths($glyph_name) $w
                }
            }
        }
        close $if
        if {[array size widths] == 0} continue
        set wl [array get widths]
        if {[info exists font_widths($fontname)]} {
            array unset tmp
            array set tmp $font_widths($fontname)
            array set tmp $wl
            set wl [array get tmp]
        }
        set font_widths($fontname) $wl
        set font_metrics($fontname) [list ascend $ascend descend $descend \
                                             fixed $fixed]
    }
}

set ch [open $resultfile w]
puts $ch "
package provide pdf4tcl::metrics 0.3

# this file is auto-generated, please do NOT edit!

namespace eval pdf4tcl {
    variable font_widths
    variable font_metrics

"

# Sort everything on output to get consistant format.
foreach fontname [lsort -dictionary [array names font_widths]] {
    array unset tmp1
    array set tmp1 $font_widths($fontname)
    set wl {}
    foreach sym [lsort -dictionary [array names tmp1]] {
        lappend wl $sym $tmp1($sym)
    }
    puts $ch [list set font_widths($fontname) $wl]
}

foreach fontname [lsort -dictionary [array names font_metrics]] {
    array unset tmp1
    array set tmp1 $font_metrics($fontname)
    set wl {}
    foreach sym [lsort -dictionary [array names tmp1]] {
        lappend wl $sym $tmp1($sym)
    }
    puts $ch [list set font_metrics($fontname) $wl]
}

puts $ch "}"
close $ch

# vim: tw=0
