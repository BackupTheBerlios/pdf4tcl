#!/bin/sh
# -*- tcl -*- \
exec tclsh "$0" ${1+"$@"}

set ::auto_path [concat [pwd] $::auto_path]
package require pdf4tcl
cd web

set ch [open index.html]
set data [read $ch]
close $ch

foreach {_ code} [regexp -all -inline {<pre>(.*?)</pre>} $data] {
    eval $code
}
