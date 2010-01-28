#!/usr/bin/env tclsh

set auto_path [linsert $auto_path 0 [pwd]/..]
# puts stderr $auto_path
package require pdf4tcl

pdf4tcl::new p1 -compress false -paper a4
p1 startPage
p1 setFont 12 "Helvetica"
p1 text "Bookmark 1" -x 10 -y 10
p1 bookmarkAdd -title "Bookmark 1"
p1 bookmarkAdd -title "Bookmark 1.1" -level 1

#p1 setTextPosition 50 50
#set bboxb [p1 getFontMetric bboxb]
#set bboxt [p1 getFontMetric bboxt]
#set dh [expr {$bboxb - $bboxt}]
#set sp [expr {$dh / 12.0}]
#p1 text "FilljXYÅÄÖQ" -fill "1.0 0.0 0.0"
#p1 newLine $sp
#p1 text " iilljXYÅÄ" -fill "0.0 1.0 0.0"

p1 startPage
p1 text "Bookmark 2" -x 50 -y 50
p1 bookmarkAdd -title "Bookmark 2"
p1 bookmarkAdd -title "Bookmark 2.1" -level 1
p1 bookmarkAdd -title "Bookmark 2.1.1" -level 2
p1 bookmarkAdd -title "Bookmark 2.1.2" -level 2
p1 bookmarkAdd -title "Bookmark 2.2" -level 1
p1 bookmarkAdd -title "Bookmark 2.2.1" -level 2
p1 bookmarkAdd -title "Bookmark 2.2.2" -level 2

p1 startPage
p1 text "Bookmark 3" -x 100 -y 100
p1 bookmarkAdd -title "Bookmark 3"
p1 bookmarkAdd -title "Bookmark 3.1" -level 1
p1 bookmarkAdd -title "Bookmark 3.1.1" -level 2
p1 bookmarkAdd -title "Bookmark 3.1.2" -level 2
p1 bookmarkAdd -title "Bookmark 3.2" -level 1 -closed 1
p1 bookmarkAdd -title "Bookmark 3.2.1" -level 2
p1 bookmarkAdd -title "Bookmark 3.2.2" -level 2

p1 write -file test6.pdf
p1 cleanup
