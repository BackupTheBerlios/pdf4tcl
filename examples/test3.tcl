#!/usr/bin/env tclsh

lappend auto_path [pwd]/../..
package require pdf4tcl

pdf4tcl::new p1 -compress false -paper a4
p1 startPage
p1 setFont 12 "Helvetica"

# Test aimed at angled stuff

# Cross at 100 100
p1 line 50 100 150 100
p1 line 100 50 100 150
# Text leaning down
p1 text "Lean down" -x 100 -y 100 -align left -angle -20 -bg {1 0 0}

# Cross at 300 300
p1 line 250 300 350 300
p1 line 300 250 300 350
# Text leaning up
p1 text "Lean up" -x 300 -y 300 -align center -angle 20 -bg {0 1 0}

# Cross at 500 500
p1 line 450 500 550 500
p1 line 500 450 500 550
# Text leaning up
p1 text "Lean up left" -x 500 -y 500 -align right -angle 110 -bg {0.3 0.3 1}

# Cross at 100 500
p1 line  50 500 150 500
p1 line 100 450 100 550
# Text leaning up
p1 text "Lean up, yskew" -x 100 -y 500 -align center -angle 50 -bg {0.3 0.3 1} \
        -yangle 40
p1 text "Lean up, noskew" -x 130 -y 500 -align center -angle 50

# Cross at 500 100
p1 line 450 100 550 100
p1 line 500  50 500 150
# Text leaning up
p1 text "Lean down, xskew" -x 500 -y 100 -align center -angle -50 -bg {0.3 0.3 1} \
        -xangle 40
p1 text "Lean down, noskew" -x 470 -y 100 -align center -angle -50

p1 write -file test3.pdf
p1 cleanup

