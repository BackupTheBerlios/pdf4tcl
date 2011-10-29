# addon for generating tables for pdf4tcl
# 2009 Siim Salonen <zod(at)alkohol.ee>

# example:
#	set str "<TR><TD>1</TD><TD><font size=20 color=#ff0000>2</TD><TD>3</TD></TR><TR><TD><font color=#553399>x</TD><TD>y</TD></TR>"
#	set str [mypdf html2table $str]
#	mypdf create_pdf_table $str

# 'getFontMetric height' from pdf4tcl 0.7 returns different (larger) numbers than 0.5. now table is also uglier.

namespace eval pdf4tcl::pdf4tcl {

    method html2table { str } {
        # no syntax checks yet
        set str [string map -nocase {"<TR>" " \{ " "</TR>" " \} " "<TD>" " \{ " "</TD>" " \} " "<BR>" "\n"} $str]
        return $str
    }

    method html_parse { str } {
        upvar pdf_cell_format pdf_cell_format
        # This pattern matches the parts of an HTML tag
        set w " \t\r\n"                 ;# white space
    	set exp "<(\[^$w/>]+)\[$w]*(\[^>]*)>"

        set j 0
        foreach row $str {
            set i 0
            foreach cell $row {
                set pdf_cell_format($j,$i) ""
                foreach {1 2 3} [regexp -all -nocase -inline $exp $cell] {
                    switch [string tolower $2] {
                        "b" {
                            append pdf_cell_format($j,$i) { if {![string match *-bold* [string tolower $pdf(current_font)]]} { $self setFont $font_height $pdf(current_font)-Bold }; }
                        }
                        "font" {
                            foreach {param value} [split $3 "= "] {
                                switch $param {
                                    "color" {
                                        append pdf_cell_format($j,$i) "\$self setFillColor $value; "
                                    }
                                    "size" {
                                        append pdf_cell_format($j,$i) "\$self setFont $value; "
                                    }
                                }
                            }
                        }
                    }
                }
                incr i
            }
            incr j
        }
    }

    method cell_lengths { str font font_height} {
        upvar pdf_row_lengths pdf_row_lengths startX startX endX endX pdf_cell_format pdf_cell_format

        set j 0
        set mod 0
        foreach row $str {
            set i 0
            # make header row bold
            foreach cell $row {
                if { ![info exists pdf_row_lengths($i)] } {
                    set pdf_row_lengths($i) 0
                }
                if { $mod } {
                    $self setFont $font_height $font
                    $self setFillColor #000000
                    set mod 0
                }
                # add formatting
                if { $pdf_cell_format($j,$i)!="" } {
                    eval $pdf_cell_format($j,$i)
                    set mod 1
                }
                foreach scell [split [string trim $cell] "\n"] {
                    set len [expr [$self getStringWidth $scell] + [$self getCharWidth "X"]]
                    if { $len > $pdf_row_lengths($i) } {
                        set pdf_row_lengths($i) $len
                    }
                }
                incr i
            }
            incr j
        }
        set endX $startX
        foreach {name value} [array get pdf_row_lengths] {
            set endX [expr $endX + $value]
        }
    }

    method create_pdf_table { str args } {
        set font $pdf(current_font)
        set font_height [$self getFontMetric height]
        set pdf_rows [llength [lindex $str 0]]
        set startX [lindex [$self getTextPosition] 0]
        puts "[$self getTextPosition] - $font_height"
        set XDrawableArea [lindex [$self getDrawableArea] 0]
        set YDrawableArea [lindex [$self getDrawableArea] 1]
        set startY [expr [lindex [$self getTextPosition] 1] - $font_height]

        # need to add some args here

        #get cell html-style formatting to array pdf_cell_format
        $self html_parse $str
        #strip html code
        set w " \t\r\n"
        set exp "<(/?)(\[^$w>]+)\[$w]*(\[^>]*)>"
        regsub -all $exp $str "" str

        #get cell lengths
        $self cell_lengths $str $font $font_height

        # shrink table if necessary
        if { $endX > $XDrawableArea } {
            # size for bigger columns
            set i 0
            set width 0
            foreach {name value} [array get pdf_row_lengths] {
                if { $value < ($XDrawableArea-$startX)/$pdf_rows } {
                    set width [expr $width + $value]
                    incr i
                }
            }
            set width [expr { ($XDrawableArea-$startX-$width) / ($pdf_rows-$i) - [$self getCharWidth "X"]}]

            for { set i 0; set mod 0 } { $i < $pdf_rows } { incr i } {
                # test every row width
                if { $pdf_row_lengths($i) > $width } {
                    set j 0
                    foreach row $str {
                        set txt [string trim [lindex $row $i]]
                        if { $mod } {
                            $self setFont $font_height $font
                            $self setFillColor #000000
                            set mod 0
                        }
                        if { [info exists pdf_cell_format($j,$i)] } {
                            if { $pdf_cell_format($j,$i)!="" } {
                                eval $pdf_cell_format($j,$i)
                                set mod 1
                            }
                        }
                        if { [$self getStringWidth $txt] > $width } {
                            set len [string length $txt]
                            # run through chars until we exceed width or reach end
                            set start 0
                            set pos 0
                            set cwidth 0
                            set lastbp 0
                            set done 0
                            set str_mod 0 ;# string is modified?

                            while {! $done} {
                                set ch [string index $txt $pos]
                                # test for breakable character
                                if {[regexp "\[ \t\r\n-\]" $ch]} {
                                    set lastbp $pos
                                }
                                set w [$self getCharWidth $ch 1]
                                if {($cwidth+$w)>$width || $pos>=$len || $ch=="\n"} {
                                    if {$pos>=$len} {
                                        set done 1
                                    } else {
                                        # backtrack to last breakpoint
                                        if {$lastbp != $start} {
                                            set pos $lastbp
                                        } else {
                                            # Word longer than line, add newline
                                            if {$pos > $start} {
                                                set txt [string replace $txt $pos $pos "x[string index $txt $pos]"]
                                                incr len
                                            }
                                        }
                                        set txt [string replace $txt $pos $pos "\n"]
                                    }
                                    set start $pos
                                    incr start
                                    set cwidth 0
                                    set lastbp $start
                                    set str_mod 1
                                } else {
                                    set cwidth [expr {$cwidth+$w}]
                                }
                                incr pos
                            }
                            if { $str_mod } {
                                set row [lreplace $row $i $i $txt]
                                set str [lreplace $str $j $j $row]
                            }
                        }
                        incr j
                    }
                }
            }
            unset pdf_row_lengths
            $self cell_lengths $str $font $font_height
        }
        # get cell coordinates for backgruond fill.
        set j 0
        set ymax [expr [lindex [$self getTextPosition] 1] + $font_height]
        puts "[$self getTextPosition] + $font_height"
        set mod 0
        set tmp_font_height $font_height
        array set page_end_arr {}
        foreach row $str {
            set i 0
            set y [expr $ymax-$font_height]
            if { $y > $YDrawableArea } {
                array set page_end_arr "$j$i $y" ;# page ending point. used to draw last horiz. line
                set y $font_height
                set ymax 2*$font_height
            }
            lappend pdf_col_lengths $y
            foreach cell $row {
                if { $mod } {
                    $self setFont $font_height $font
                    $self setFillColor #000000
                    set mod 0
                    set tmp_font_height $font_height
                }
                # format cell
                if { $pdf_cell_format($j,$i)!="" } {
                    eval $pdf_cell_format($j,$i)
                    set mod 1
                    set tmp_font_height [$self getFontMetric height]
                }

                set y_tmp [expr $tmp_font_height-$font_height+$y] ;# add custom font size diff
                foreach scell [split $cell "\n"] {
                    set y_tmp [expr {$y_tmp + $tmp_font_height}]
                }
                set y_tmp [expr {$y_tmp + $font_height * 1.3}]
                if { $y_tmp > $ymax } {
                    set ymax $y_tmp
                }
                incr i
            }
            incr j
        }
        set endY [expr {$ymax - $font_height*2}]
        $self setFillColor .7 .9 .9
        # horizontal header
        $self rectangle $startX $startY [expr $endX-$startX] [expr [lindex $pdf_col_lengths 1]-[lindex $pdf_col_lengths 0]] -filled 1 -stroke 0
        # vertical header
        $self setFillColor 0 0 0

        # lighter lines
        $self setStrokeColor .6 .6 .6
        # print horizontal lines and text
        set j 0
        set mod 0
        set tmp_font_height $font_height
        set page_end_arr_s [array startsearch page_end_arr]
        $self line $startX $startY $endX $startY
        foreach row $str {
            set i 0
            set x [expr {$startX + [$self getCharWidth "X"]/2}]
            set y [lindex $pdf_col_lengths $j]
            if { $j && $y<[lindex $pdf_col_lengths [expr $j-1]] } {
                # vertical lines
                set key [array nextelement page_end_arr $page_end_arr_s]
                $self line $startX [expr {$page_end_arr($key)-$font_height}] $endX [expr {$page_end_arr($key)-$font_height}]
                $self write_ygrid $startX $startY [expr {$page_end_arr($key)-$font_height}] $pdf_rows
                $self startPage
                $self setStrokeColor .6 .6 .6
                set startY 0
            }
            $self line $startX [expr {$y - $font_height}] $endX [expr {$y - $font_height}]
            foreach cell $row {
                if { $mod } {
                    $self setFont $font_height $font
                    $self setFillColor #000000
                    set mod 0
                    set tmp_font_height $font_height
                }
                # format cell
                if { $pdf_cell_format($j,$i)!="" } {
                    eval $pdf_cell_format($j,$i)
                    set mod 1
                    set tmp_font_height [$self getFontMetric height]
                }
                set y_tmp [expr $tmp_font_height-$font_height+$y] ;# add custom font size diff
                foreach scell [split $cell "\n"] {
                    $self setTextPosition $x $y_tmp
                    $self text "[string trim $scell]"
                    set y_tmp [expr {$y_tmp + $tmp_font_height}]
                }
                set x [expr $x + $pdf_row_lengths($i)]
                incr i
            }
            incr j
        }
        $self line $startX $endY $endX $endY

        # vertical lines
        $self write_ygrid $startX $startY $endY $pdf_rows
        #parray pdf_row_lengths
    }

    method write_ygrid { startX startY endY pdf_rows} {
        upvar pdf_row_lengths pdf_row_lengths
        set x $startX
        for { set i 0 } { $i<$pdf_rows } { incr i } {
            $self line $x $startY $x $endY
            set x [expr $x + $pdf_row_lengths($i)]
        }
        $self line $x $startY $x $endY
    }
}
