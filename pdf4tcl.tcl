# library of tcl procedures for generating portable document format files
# this is a port of pdf4php from php to tcl

# Copyright (c) 2004 by Frank Richter <frichter@truckle.in-chemnitz.de> and
# Jens Pönisch <jens@ruessel.in-chemnitz.de>

# See the file "licence.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# Version 0.1   base features for generating correct pdf files
# Version 0.2   more graphic operators, fixed font handling

package provide pdf4tcl 0.2.1

package require pdf4tcl::metrics
package require pdf4tcl::glyphnames
package require snit

namespace eval pdf4tcl {
    # helper variables (constants) packaged into arrays to minimize
    # variable import statements
    variable g
    variable font_widths
    variable glyph_names
    variable font_afm
    variable paper_sizes

    # path to adobe afm files
    set g(ADOBE_AFM_PATH) {}
    # change this to reflect your machines install!
    lappend g(ADOBE_AFM_PATH) {/usr/share/texmf/fonts/afm/adobe/*}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/enscript}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/fonts/default/ghostscript}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/fonts/default/Type1}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/a2ps/afm}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/ogonkify/afm}

    # font width array
    array set font_widths {}

    # font name to afm file mapping array
    array set font_afm {}

    # known papersizes
    array set paper_sizes {
        a0     {2380 3368}
        a1     {1684 2380}
        a2     {1190 1684}
        a3     { 842 1190}
        a4     { 595  842}
        a5     { 421  595}
        a6     { 297  421}
        11x17  { 792 1224}
        ledger {1224  792}
        legal  { 612 1008}
        letter { 612  792}
    }

    if {[catch {package require zlib} err]} {
        set g(haveZlib) 0
    } else {
        set g(haveZlib) 1
    }

    proc Init {} {
        LoadAfmMapping
    }

    proc LoadAfmMapping {} {
        variable font_afm
        variable g

        foreach path $g(ADOBE_AFM_PATH) {
            foreach file [glob -nocomplain [file join $path "*.afm"]] {
                set if [open $file "r"]
                while {[gets $if line]!=-1} {
                    if {[regexp {^FontName\s*(.*)$} $line dummy fontname]} {
                        set font_afm($fontname) $file
                        break
                    }
                }
                close $if
            }
        }
        #parray font_afm
    }

    # Utility to look up paper size by name
    proc getPaperSize {papername} {
        variable paper_sizes

        if {[info exists paper_sizes($papername)]} {
            return $paper_sizes($papername)
        } else {
            return {}
        }
    }

    # Return a list of known paper sizes
    proc getPaperSizeList {} {
        variable paper_sizes
        return [array names paper_sizes]
    }

    # Get points from a measurement.
    proc getPoints {val} {
        if {[string is double -strict $val]} {
            return $val
        }
        if {[regexp {^\s*(\S+?)\s*([[:alpha:]]+)\s*$} $val -> num unit]} {
            if {[string is double -strict $num]} {
                switch -- $unit {
                    mm {
                        return [expr {$num / 25.4 * 72.0}]
                    }
                    i {
                        return [expr {$num * 72.0}]
                    }
                }
            }
        }
        return -code error "Unknown value $val"
    }

    # Wrapper to create pdf4tcl object
    proc new {args} {
        uplevel 1 pdf4tcl::pdf4tcl create $args
    }

    Init
}

# PDF-Struktur:
# 1 = Root
#     2 = Pages
#     3 = Resources
#     4 = First page
#             .
#             .
#             .
#     X = Fonts


# Object used for generating pdf
snit::type pdf4tcl::pdf4tcl {
    variable pdf

    # Global option handling
    option -paper     -default a4     -validatemethod CheckPaper
    option -landscape -default 0      -validatemethod CheckBoolean
    option -orient    -default 1      -validatemethod CheckBoolean
    option -compress  -default 0      -validatemethod CheckBoolean \
            -configuremethod SetCompress
    option -margin    -default 0      -validatemethod CheckMargin

    method CheckPaper {option value} {
        set papersize [pdf4tcl::getPaperSize $value]
        if {[llength $papersize] == 0} {
            return -code error "papersize $value is unknown"
        }
    }

    method CheckMargin {option value} {
        switch {[llength $value]} {
            1 - 2 - 4 {}
            default {
                return -code error "Bad margin list '$list'"
            }
        }
    }

    method CheckBoolean {option value} {
        if {![string is boolean -strict $value]} {
            return -code error "option $option must have a boolean value."
        }
    }

    method SetCompress {option value} {
        variable ::pdf4tcl::g
        if {$value} {
            if {$g(haveZlib)} {
                set options($option) 1
            } else {
                puts stderr "Package zlib not available. Sorry, no compression."
            }
        } else {
            set options($option) 0
        }
    }

    constructor {args} {
        variable ::pdf4tcl::g

        $self configurelist $args

        # Document data
        set pdf(pages) 0
        set pdf(pdf_obj) 4
        set pdf(out_pos) 0
        set pdf(data_start) 0
        set pdf(data_len) 0
        set pdf(fonts) {}
        set pdf(font_size) 8
        set pdf(current_font) ""
        set pdf(font_set) false
        set pdf(in_text_object) false
        set pdf(images) {}
        set pdf(compress) 0
        set pdf(finished) false
        set pdf(inPage) false

        # Page data
        set pdf(xpos) 0
        set pdf(width) 0
        set pdf(ypos) 0
        set pdf(height) 0
        set pdf(orient) 1
        set pdf(marginleft)   0
        set pdf(marginright)  0
        set pdf(margintop)    0
        set pdf(marginbottom) 0

        # output buffer (we need to compress whole pages)
        set pdf(ob) ""

        # collect output in memory
        set pdf(pdf) ""

        # Offsets
        set pdf(xoff) 0
        set pdf(yoff) 0

        # Start on pdfout
        $self pdfout "%PDF-1.3\n"

        # start with Helvetica as default font
        set pdf(font_size) 12
        set pdf(current_font) "Helvetica"
    }

    # Add data to accumulated pdf output
    method pdfout {out} {
        append pdf(ob) $out
        incr pdf(out_pos) [string length $out]
    }

    method startPage {args} {
        set localopts(-orient)    $options(-orient)
        set localopts(-landscape) $options(-landscape)
        set localopts(-margin)    $options(-margin)
        set localopts(-paper)     $options(-paper)
        set usepaper 1
        set paperset 0
        set landscapeset 0

        if {[llength $args] == 1} {
            # Single arg = paper
            $self CheckPaper -paper [lindex $args 0]
            set localopts(-paper) [lindex $args 0]
            set paperset 1
        } elseif {[llength $args] == 2 && [string is digit [join $args ""]]} {
            # Old style two numeric args
            set width  [lindex $args 0]
            set height [lindex $args 1]
            set usepaper 0
            set paperset 1
        } elseif {[llength $args] == 3 && [string is digit [join $args ""]]} {
            # Old style three numeric args
            set width  [lindex $args 0]
            set height [lindex $args 1]
            set localopts(-orient) [lindex $args 2]
            set usepaper 0
            set paperset 1
        } elseif {[llength $args] % 2 != 0} {
            # Uneven, error
            return -code error "AAAARRRGH"
        } else {
            # Parse options
            foreach {option value} $args {
                switch -- $option {
                    -paper {
                        $self CheckPaper $option $value
                        set paperset 1
                    }
                    -landscape {
                        $self CheckBoolean $option $value
                        set landscapeset 1
                    }
                    -margin {
                        $self CheckMargin $option $value
                    }
                    -orient {
                        $self CheckBoolean $option $value
                    }
                    default {
                        return -code error "Unknown option $option"
                    }
                }
                set localopts($option) $value
            }
        }

        if {$usepaper} {
            set papersize [pdf4tcl::getPaperSize $localopts(-paper)]
            set width  [lindex $papersize 0]
            set height [lindex $papersize 1]
        }
        # Switch if landscape has been asked for
        if {(!$paperset || $landscapeset) && $localopts(-landscape)} {
            set tmp    $width
            set width  $height
            set height $tmp
        }

        # Fill in margins
        set value $localopts(-margin)
        switch {[llength $value]} {
            1 {
                set pdf(marginleft)   [pdf4tcl::getPoints [lindex $value 0]]
                set pdf(marginright)  [pdf4tcl::getPoints [lindex $value 0]]
                set pdf(margintop)    [pdf4tcl::getPoints [lindex $value 0]]
                set pdf(marginbottom) [pdf4tcl::getPoints [lindex $value 0]]
            }
            2 {
                set pdf(marginleft)   [pdf4tcl::getPoints [lindex $value 0]]
                set pdf(marginright)  [pdf4tcl::getPoints [lindex $value 0]]
                set pdf(margintop)    [pdf4tcl::getPoints [lindex $value 1]]
                set pdf(marginbottom) [pdf4tcl::getPoints [lindex $value 1]]
            }
            4 {
                set pdf(marginleft)   [pdf4tcl::getPoints [lindex $value 0]]
                set pdf(marginright)  [pdf4tcl::getPoints [lindex $value 1]]
                set pdf(margintop)    [pdf4tcl::getPoints [lindex $value 2]]
                set pdf(marginbottom) [pdf4tcl::getPoints [lindex $value 3]]
            }
        }

        if {$pdf(inPage)} {
            $self endPage
        }
        set pdf(inPage) 1
        set pdf(ypos) $height
        set pdf(width) $width
        set pdf(height) $height
        set pdf(orient) $localopts(-orient)
        set pdf(xpos) 0
        incr pdf(pages)

        # dimensions
        set oid [$self get_oid]
        $self store_xref
        $self pdfout "$oid 0 obj\n"
        $self pdfout "<</Type /Page\n"
        $self pdfout "/Parent 2 0 R\n"
        $self pdfout "/Resources 3 0 R\n"
        $self pdfout [format "/MediaBox \[0 0 %g %g\]\n" $width $height]
        $self pdfout "/Contents \[[$self next_oid] 0 R \]\n"
        $self pdfout ">>\n"
        $self pdfout "endobj\n\n"

        # start of contents
        set oid [$self incr_oid]
        $self store_xref
        $self pdfout "$oid 0 obj\n"
        $self pdfout "<<\n/Length [$self next_oid] 0 R\n"
        if {$pdf(compress)} {
            $self pdfout "/Filter \[/FlateDecode\]\n"
        }
        $self pdfout ">>\nstream\n"
        set pdf(data_start) $pdf(out_pos)
        set pdf(in_text_object) false
        $self incr_oid

        # no font set on new pages
        set pdf(font_set) false

        # capture output
        append pdf(pdf) $pdf(ob)
        set pdf(ob) ""
    }

    method endPage {} {
        if {! $pdf(inPage)} {
            return
        }
        if {$pdf(in_text_object)} {
            $self pdfout "\nET\n"
        }
        # get buffer
        set data $pdf(ob)
        set pdf(ob) ""
        if {$pdf(compress) >0} {
            set data [zlib compress $data]
        }
        append pdf(pdf) $data
        set data_len [string length $data]
        set pdf(out_pos) [expr {$pdf(data_start)+$data_len}]
        $self pdfout "\nendstream\n"
        $self pdfout "endobj\n\n"
        $self store_xref
        $self pdfout "[$self get_oid] 0 obj\n"
        incr data_len
        $self pdfout "$data_len\n"
        $self pdfout "endobj\n\n"
        $self incr_oid
        set pdf(inPage) false
    }

    method finish {} {
        if {$pdf(finished)} {
            return
        }

        if {$pdf(inPage)} {
            $self endPage
        }
        set pdf(xref,1) $pdf(out_pos)
        $self pdfout "1 0 obj\n"
        $self pdfout "<<\n"
        $self pdfout "/Type /Catalog\n"
        $self pdfout "/Pages 2 0 R\n"
        $self pdfout ">>\n"
        $self pdfout "endobj\n\n"

        set pdf(xref,2) $pdf(out_pos)
        $self pdfout "2 0 obj\n"
        $self pdfout "<<\n/Type /Pages\n"
        $self pdfout "/Count $pdf(pages)\n"
        $self pdfout "/Kids \["
        for {set a 0} {$a<$pdf(pages)} {incr a} {
            set b [expr {4 + $a*3}]
            $self pdfout "$b 0 R "
        }
        $self pdfout "\]\n"
        $self pdfout ">>\n"
        $self pdfout "endobj\n\n"

        set pdf(xref,3) $pdf(out_pos)
        $self pdfout "3 0 obj\n"
        $self pdfout "<<\n"
        $self pdfout "/ProcSet\[/PDF /Text /ImageC\]\n"
        $self pdfout "/Font <<\n"

        # font references
        set count 0
        foreach fontname $pdf(fonts) {
            set nr [expr {$pdf(pdf_obj)+$count}]
            $self pdfout "/$fontname $nr 0 R\n"
            incr count
        }
        $self pdfout ">>\n"

        # image references
        if {[llength $pdf(images)]>0} {
            $self pdfout "/XObject <<\n"
            foreach {key value} $pdf(images) {
                set nr [expr {$pdf(pdf_obj)+$count}]
                $self pdfout "/$key $nr 0 R\n"
                incr count
            }
            $self pdfout ">>\n"
        }
        $self pdfout ">>\nendobj\n\n"

        # fonts
        foreach fontname $pdf(fonts) {
            $self store_xref
            $self pdfout "[$self get_oid] 0 obj\n"
            $self pdfout "<<\n/Type /Font\n"
            $self pdfout "/Subtype /Type1\n"
            $self pdfout "/Encoding /WinAnsiEncoding\n"
            $self pdfout "/Name /$fontname\n"
            $self pdfout "/BaseFont /$fontname\n"
            $self pdfout ">>\n"
            $self pdfout "endobj\n\n"
            $self incr_oid
        }

        # images
        foreach {key value} $pdf(images) {
            $self store_xref
            foreach {img_width img_height img_depth img_length img_data} $value {break}
            $self pdfout "[$self get_oid] 0 obj\n"
            $self pdfout "<<\n/Type /XObject\n"
            $self pdfout "/Subtype /Image\n"
            $self pdfout "/Width $img_width\n/Height $img_height\n"
            $self pdfout "/ColorSpace /DeviceRGB\n"
            $self pdfout "/BitsPerComponent $img_depth\n"
            $self pdfout "/Filter /DCTDecode\n"
            $self pdfout "/Length $img_length >>\n"
            $self pdfout "stream\n"
            $self pdfout $img_data
            $self pdfout "\nendstream\n"
            $self pdfout "endobj\n\n"
            $self incr_oid
        }

        # cross reference
        set xref_pos $pdf(out_pos)
        $self pdfout "xref\n"
        $self store_xref
        $self pdfout "0 [$self get_oid]\n"
        $self pdfout "0000000000 65535 f \n"
        for {set a 1} {$a<[$self get_oid]} {incr a} {
            set xref $pdf(xref,$a)
            $self pdfout [format "%010ld 00000 n \n" $xref]
        }
        $self pdfout "trailer\n"
        $self pdfout "<<\n"
        $self pdfout "/Size [$self get_oid]\n"
        $self pdfout "/Root 1 0 R\n"
        $self pdfout ">>\n"
        $self pdfout "\nstartxref\n"
        $self pdfout "$xref_pos\n"
        $self pdfout "%%EOF\n"
        append pdf(pdf) $pdf(ob)
        set pdf(ob) ""
        set pdf(finished) true
    }

    method get {} {
        if {$pdf(inPage)} {
            $self endPage
        }
        if {! $pdf(finished)} {
            $self finish
        }
        return $pdf(pdf)
    }

    method write {args} {
        set chan stdout
        set outfile 0
        foreach {arg value} $args {
            switch -- $arg {
                "-file" {
                    if {[catch {open $value "w"} chan]} {
                        return -code error "Could not open file $value for writing: $chan"
                    } else {
                        set outfile 1
                    }
                }
                default {
                    return -code error "unknown option $arg."
                }
            }
        }

        fconfigure $chan -translation binary
        puts -nonewline $chan [$self get]
        if {$outfile} {
            close $chan
        }
        return
    }

    method cleanup {} {
        $self destroy
    }

    # Transform user coordinates to page coordinates
    # This should take into account orientation, rotation, margins.
    method Trans {x y txName tyName} {
        upvar 1 $txName tx $tyName ty

        set px [pdf4tcl::getPoints $x]
        set py [pdf4tcl::getPoints $y]

        set tx [expr {$px + $pdf(xoff)}]
        set ty [expr {$py + $pdf(yoff)}]
        if {$pdf(orient)} {
            set ty [expr {$pdf(height) - $ty}]
        }
    }

    # Returns width and height of drawable area, excluding margins.
    method getDrawableArea {} {
        set w [expr {$pdf(width) - $pdf(marginleft) - $pdf(marginright)}]
        set h [expr {$pdf(height) - $pdf(margintop)  - $pdf(marginbottom)}]
        return [list $w $h]
    }

    method setFont {size {fontname ""}} {
        variable ::pdf4tcl::font_widths

        if {[string length $fontname]==0} {
            set fontname $pdf(current_font)
        }
        # font width already loaded?
        if {! [info exists font_widths($fontname)]} {
            if {[catch {loadFontMetrics $fontname} tmp]} {
                return -code error "Could not load font metrics for $fontname"
            } else {
                set font_widths($fontname) $tmp
            }
        }
        set pdf(font_size) $size
        $self pdfout "/$fontname $size Tf\n"
        $self pdfout "0 Tr\n"
        $self pdfout "$size TL\n"
        if {[lsearch $pdf(fonts) $fontname]==-1} {
            lappend pdf(fonts) $fontname
        }
        set pdf(current_font) $fontname

        set pdf(font_set) true
    }

    proc loadFontMetrics {font} {
        variable ::pdf4tcl::font_afm

        set file $font_afm($font)
        if {[catch {open $file "r"} if]} {
            return ""
        } else {
            set started false
            array set widths {}
            while {[gets $if line]!=-1} {
                if {! $started} {
                    if {[string first "StartCharMetrics" $line]==0} {
                        set started true
                    }
                } else {
                    # Done?
                    if {[string first "EndCharMetrics" $line]==0} {
                        break
                    }
                    if {[string index $line 0]=="C"} {
                        scan [string range $line 1 4] "%d" ch
                        if {($ch>0) && ($ch<256)} {
                            set pos [string first "WX" $line]
                            incr pos 2
                            set endpos $pos
                            incr endpos 4
                            scan [string range $line $pos $endpos] "%d" w
                            set char [format "%c" $ch]
                            set widths($char) $w
                        }
                    }
                }
            }
            close $if
            return [array get widths]
        }
    }

    method getStringWidth {txt} {
        set w 0
        for {set i 0} {$i<[string length $txt]} {incr i} {
            set ch [string index $txt $i]
            set w [expr {$w + [$self getCharWidth $ch]}]
        }
        return $w
    }

    method getCharWidth {ch} {
        variable ::pdf4tcl::font_widths
        variable ::pdf4tcl::glyph_names

        if {$ch=="\n"} {
            return 0
        }

        set afm2point [expr {0.001 * $pdf(font_size)}]
        if {[scan $ch %c n]!=1} {
            return 0
        }
        set ucs2 [format "%04.4X" $n]

        array set widths $font_widths($pdf(current_font))
        set glyph_name zero
        set w 0
        catch {set w $widths("zero")}
        catch {set glyph_name $glyph_names($ucs2)}
        switch -- $glyph_name {
            "spacehackarabic" {set glyph_name "space"}
        }
        catch {set w $widths($glyph_name)}
        ###puts stderr "ch: $ch  n: $n  ucs2: $ucs2  glyphname: $glyph_name  width: $w"
        return [expr {$w*$afm2point}]
    }

    method setTextPosition {x y} {
        $self beginTextObj
        set pdf(xpos) [expr {$x + $pdf(xoff)}]
        if {$pdf(orient)} {
            set pdf(ypos) [expr {$pdf(height) - $y - \
                                               $pdf(yoff)}]
        } else {
            set pdf(ypos) [expr {$y + $pdf(yoff)}]
        }
        $self pdfout [format "1 0 0 1 %s %s Tm\n" \
                              [nf $pdf(xpos)] [nf $pdf(ypos)]]
    }

    # draw text at current position with angle ang
    method drawText {str {ang 0}} {
        $self beginTextObj
        if {! $pdf(font_set)} {
            #SetBaseFont $name $pdf(current_font)
            $self setFont $pdf(font_size) $pdf(current_font)
        }
        $self pdfout "([cleanText $str]) '\n"
        set pdf(ypos) [expr {$pdf(ypos) + \
                                           $pdf(font_size)}]
    }

    method drawTextAt {x y str args} {
        set align "left"
        set angle 0
        foreach {arg value} $args {
            switch -- $arg {
                "-align" {
                    set align $value
                }
                "-angle" {
                    set angle $value
                }
                default {
                    return -code error \
                            "unknown option $arg"
                }
            }
        }

        $self beginTextObj

        if {! $pdf(font_set)} {
            $self setFont $pdf(font_size)
        }

        if {$align == "right"} {
            set x [expr {$x - [$self getStringWidth $str]}]
        } elseif {$align == "center"} {
            set x [expr {$x - [$self getStringWidth $str] / 2 * cos($angle*3.1415926/180.0)}]
            set y [expr {$y - [$self getStringWidth $str] / 2 * sin($angle*3.1415926/180.0)}]
        }
        if {$angle != 0} {
            set pdf(xpos) [expr {$x + $pdf(xoff)}]
            if {$pdf(orient)} {
                set pdf(ypos) [expr {$pdf(height) - $y - $pdf(yoff)}]
            } else {
                set pdf(ypos) [expr {$y + $pdf(yoff)}]
            }
            $self rotateText $angle
        } else {
            $self setTextPosition $x $y
        }
        $self pdfout "([cleanText $str]) Tj\n"
    }

    method drawTextBox {x y width height txt args} {
        foreach {arg value} $args {
            switch -- $arg {
                "-align" {
                    set align $value
                }
                default {
                    return -code error \
                            "unknown option $arg"
                }
            }
        }

        $self beginTextObj

        # pre-calculate some values
        set font_height $pdf(font_size)
        set space_width [$self getCharWidth " "]
        set ystart $y
        if {!$pdf(orient)} {
            set y [expr {$y+$height-3*$font_height/2}]
        }
        set len [string length $txt]

        # run through chars until we exceed width or reach end
        set start 0
        set pos 0
        set cwidth 0
        set lastbp 0
        set done false

        while {! $done} {
            set ch [string index $txt $pos]
            # test for breakable character
            if {[regexp "\[ \t\r\n-\]" $ch]} {
                set lastbp $pos
            }
            set w [$self getCharWidth $ch]
            if {($cwidth+$w)>$width || $pos>=$len || $ch=="\n"} {
                if {$pos>=$len} {
                    set done true
                } else {
                    # backtrack to last breakpoint
                    set pos $lastbp
                }
                set sent [string trim [string range $txt $start $pos]]
                switch -- $align {
                    "justify" {
                        # count number of spaces
                        set words [split $sent " "]
                        if {[llength $words]>1 && (!$done) && $ch!="\n"} {
                            # determine additional width per space
                            set sw [$self getStringWidth $sent]
                            set add [expr {($width-$sw)/([llength $words]-1)}]
                            # display words
                            set xx $x
                            for {set i 0} {$i<[llength $words]} {incr i} {
                                $self drawTextAt $xx $y [lindex $words $i]
                                set xx [expr {$xx+[$self getStringWidth [lindex $words $i]]+$space_width+$add}]
                            }
                        } else {
                            $self drawTextAt $x $y $sent
                        }
                    }
                    "right" {
                        $self drawTextAt [expr {$x+$width}] $y $sent -align right
                    }
                    "center" {
                        $self drawTextAt [expr {$x+$width/2.0}] $y $sent -align center
                    }
                    default {
                        $self drawTextAt $x $y $sent
                    }
                }
                if {$pdf(orient)} {
                    set y [expr {$y+$font_height}]
                } else {
                    set y [expr {$y-$font_height}]
                }
                # too big?
                if {($y+$font_height-$ystart)>=$height} {
                    return [string range $txt $pos end]
                }
                set start $pos
                incr start
                set cwidth 0
                set lastbp 0
            } else {
                set cwidth [expr {$cwidth+$w}]
            }
            incr pos
        }
        return ""
    }

    ###<jpo 2004-11-08: replaced "on off" by "args"
    ###                 to enable resetting dashed lines
    method setLineStyle {width args} {
        $self endTextObj
        $self pdfout "$width w\n"
        $self pdfout "\[$args\] 0 d\n"
    }

    method line {x1 y1 x2 y2} {
        $self endTextObj
        if {$pdf(orient)} {
            set y1 [expr {$pdf(height)-$y1}]
            set y2 [expr {$pdf(height)-$y2}]
        }
        $self pdfout [format "%g %g m\n" [nf [expr {$x1+$pdf(xoff)}]] [nf [expr {$y1+$pdf(yoff)}]]]
        $self pdfout [format "%g %g l\n" [nf [expr {$x2+$pdf(xoff)}]] [nf [expr {$y2+$pdf(yoff)}]]]
        $self pdfout "S\n"
    }

    ###>2004-11-03 jpo
    method qCurve {x1 y1 xc yc x2 y2} {
        $self endTextObj
        if {$pdf(orient)} {
            set y1 [expr {$pdf(height)-$y1}]
            set y2 [expr {$pdf(height)-$y2}]
            set yc [expr {$pdf(height)-$yc}]
        }
        $self pdfout [format "%g %g m\n" [nf [expr {$x1+$pdf(xoff)}]] [nf [expr {$y1+$pdf(yoff)}]]]
        $self pdfout [format "%g %g %g %g %g %g c\n" \
                              [nf [expr {0.3333*$x1+0.6667*$xc+$pdf(xoff)}]] \
                              [nf [expr {0.3333*$y1+0.6667*$yc+$pdf(yoff)}]] \
                              [nf [expr {0.3333*$x2+0.6667*$xc+$pdf(xoff)}]] \
                              [nf [expr {0.3333*$y2+0.6667*$yc+$pdf(yoff)}]] \
                              [nf [expr {$x2+$pdf(xoff)}]] \
                              [nf [expr {$y2+$pdf(yoff)}]] \
                             ]
        $self pdfout "S\n"
    }
    ###<jpo

    ###>2004-11-07 jpo
    # polygon name isFilled x0 y0 x1 y1 ...
    method polygon {isFilled args} {
        $self endTextObj
        if {$isFilled} {set op "b"} else {set op "s"}
        set start 1
        foreach {x y} $args {
            if {$pdf(orient)} {
                set y [expr {$pdf(height)-$y}]
            }
            if {$start} {
                $self pdfout [format "%g %g m\n" \
                                      [nf [expr {$x+$pdf(xoff)}]] \
                                      [nf [expr {$y+$pdf(yoff)}]]]
                set start 0
            } else {
                $self pdfout [format "%g %g l\n" \
                                      [nf [expr {$x+$pdf(xoff)}]] \
                                      [nf [expr {$y+$pdf(yoff)}]]]
            }
        }
        $self pdfout " $op\n"
    }

    method circle {isFilled x y r} {
        $self endTextObj
        if {$isFilled} {set op "b"} else {set op "s"}
        if {$pdf(orient)} {
            set y [expr {$pdf(height)-$y}]
        }
        set sq [expr {4.0*(sqrt(2.0)-1.0)/3.0}]
        set x0(0) [expr {$x+$r}]
        set y0(0) $y
        set x1(0) [expr {$x+$r}]
        set y1(0) [expr {$y+$r*$sq}]
        set x2(0) [expr {$x+$r*$sq}]
        set y2(0) [expr {$y+$r}]
        set x3(0) $x
        set y3(0) [expr {$y+$r}]
        set x1(1) [expr {$x-$r*$sq}]
        set y1(1) [expr {$y+$r}]
        set x2(1) [expr {$x-$r}]
        set y2(1) [expr {$y+$r*$sq}]
        set x3(1) [expr {$x-$r}]
        set y3(1) $y
        set x1(2) [expr {$x-$r}]
        set y1(2) [expr {$y-$r*$sq}]
        set x2(2) [expr {$x-$r*$sq}]
        set y2(2) [expr {$y-$r}]
        set x3(2) $x
        set y3(2) [expr {$y-$r}]
        set x1(3) [expr {$x+$r*$sq}]
        set y1(3) [expr {$y-$r}]
        set x2(3) [expr {$x+$r}]
        set y2(3) [expr {$y-$r*$sq}]
        set x3(3) [expr {$x+$r}]
        set y3(3) $y
        $self pdfout [format "%g %g m\n" \
                              [nf [expr {$x0(0)+$pdf(xoff)}]] \
                              [nf [expr {$y0(0)+$pdf(yoff)}]]]
        for {set i 0} {$i < 4} {incr i} {
            $self pdfout [format "%g %g %g %g %g %g c\n" \
                                  [nf [expr {$x1($i)+$pdf(xoff)}]] \
                                  [nf [expr {$y1($i)+$pdf(yoff)}]] \
                                  [nf [expr {$x2($i)+$pdf(xoff)}]] \
                                  [nf [expr {$y2($i)+$pdf(yoff)}]] \
                                  [nf [expr {$x3($i)+$pdf(xoff)}]] \
                                  [nf [expr {$y3($i)+$pdf(yoff)}]]]
        }
        $self pdfout " $op\n"
    }

    # scale with r, rotate by phi, and move by (dx, dy)
    proc transform {r phi dx dy points} {
        set cos_phi [expr {$r*cos($phi)}]
        set sin_phi [expr {$r*sin($phi)}]
        set res [list]
        foreach {x y} $points {
            set xn [expr {$x*$cos_phi - $y*$sin_phi + $dx}]
            set yn [expr {$x*$sin_phi + $y*$cos_phi + $dy}]
            lappend res $xn $yn
        }
        return $res
    }

    proc simplearc {phi2} {
        set x0 [expr {cos($phi2)}]
        set y0 [expr {-sin($phi2)}]
        set x3 $x0
        set y3 [expr {-$y0}]
        set x1 [expr {0.3333*(4.0-$x0)}]
        set y1 [expr {(1.0-$x0)*(3.0-$x0)/(3.0*$y0)}]
        set x2 $x1
        set y2 [expr {-$y1}]
        return [list $x0 $y0 $x1 $y1 $x2 $y2 $x3 $y3]
    }

    method arc {x0 y0 r phi extend} {
        if {abs($extend) >= 360.0} {
            $self circle 0 $x0 $y0 $r
            return
        }
        $self endTextObj
        if {abs($extend) < 0.01} return
        if {$pdf(orient)} {
            set y0 [expr {$pdf(height)-$y0}]
        }
        set count 1
        while {abs($extend) > 90} {
            set count [expr {2*$count}]
            set extend [expr {0.5*$extend}]
        }
        set phi [expr {$phi/180.0*3.1416}]
        set extend [expr {$extend/180.0*3.1416}]
        set phi2 [expr {0.5*$extend}]
        set x [expr {$x0+$r*cos($phi)}]
        set y [expr {$y0+$r*sin($phi)}]
        $self pdfout [format "%g %g m\n" \
                              [nf [expr {$x+$pdf(xoff)}]] \
                              [nf [expr {$y+$pdf(yoff)}]]]
        set points [simplearc $phi2]
        set phi [expr {$phi+$phi2}]
        for {set i 0} {$i < $count} {incr i} {
            foreach {x y x1 y1 x2 y2 x3 y3} \
                    [transform $r $phi $x0 $y0 $points] break
            set phi [expr {$phi+$extend}]
            $self pdfout [format "%g %g %g %g %g %g c\n" \
                                  [nf [expr {$x1+$pdf(xoff)}]] \
                                  [nf [expr {$y1+$pdf(yoff)}]] \
                                  [nf [expr {$x2+$pdf(xoff)}]] \
                                  [nf [expr {$y2+$pdf(yoff)}]] \
                                  [nf [expr {$x3+$pdf(xoff)}]] \
                                  [nf [expr {$y3+$pdf(yoff)}]]]
        }
        $self pdfout " S\n"
    }
    ###<jpo

    method arrow {x1 y1 x2 y2 sz {angle 20}} {
        $self line $x1 $y1 $x2 $y2
        set rad [expr {$angle*3.1415926/180.0}]
        set ang [expr {atan2(($y1-$y2), ($x1-$x2))}]
        $self line $x2 $y2 [expr {$x2+$sz*cos($ang+$rad)}] [expr {$y2+$sz*sin($ang+$rad)}]
        $self line $x2 $y2 [expr {$x2+$sz*cos($ang-$rad)}] [expr {$y2+$sz*sin($ang-$rad)}]
    }

    method setFillColor {red green blue} {
        $self pdfout "$red $green $blue rg\n"
    }

    method setStrokeColor {red green blue} {
        $self pdfout "$red $green $blue RG\n"
    }

    method rectangle {x y w h args} {
        set filled 0
        foreach {arg value} $args {
            switch -- $arg {
                "-filled" {
                    set filled 1
                }
                default {
                    return -code error "unknown option $arg"
                }
            }
        }
        $self endTextObj
        if {$pdf(orient)} {
            set y [expr {$pdf(height)-$y}]
            set h [expr {0-$h}]
        }
        $self pdfout [format "%g %g $w $h re\n" [nf [expr {$x+$pdf(xoff)}]] [nf [expr {$y+$pdf(yoff)}]]]
        if {$filled} {
            $self pdfout "B\n"
        } else {
            $self pdfout "B\n"
        }
    }

    method moveTo {x1 y1} {

        $self endTextObj
        set y1 [expr {$pdf(height)-$y1}]
        $self pdfout [format "%g %g m\n" [nf [expr {$x1+$pdf(xoff)}]] [[expr {$y1+$pdf(yoff)}]]]
    }

    method closePath {} {
        $self pdfout "b\n"
    }

    method rotateText {angle} {
        $self beginTextObj
        set rad [expr {$angle*3.1415926/180.0}]
        set c [nf [expr {cos($rad)}]]
        set s [nf [expr {sin($rad)}]]
        $self pdfout "$c [expr {0-$s}] $s $c $pdf(xpos) $pdf(ypos) Tm\n"
    }

    method skewText {xangle yangle} {
        set tx [expr {tan($xangle*3.1415926/180.0)}]
        set ty [expr {tan($yangle*3.1415926/180.0)}]
        $self pdfout [format "1 %g %g 1 %g %g Tm\n" $tx $ty $pdf(xpos) $pdf(ypos)]
        set pdf(xpos) 0
        set pdf(ypos) $pdf(height)
    }

    method addJpeg {filename id} {

        set imgOK false
        if {[catch {open $filename "r"} if]} {
            return -code error "Could not open file $filename"
        }
        fconfigure $if -translation binary
        set img [read $if]
        close $if
        binary scan $img "H4" h
        if {$h != "ffd8"} {
            close $if
            return -code error "file does not contain JPEG data."
        }
        set pos 2
        set img_length [string length $img]
        while {$pos < $img_length} {
            set endpos [expr {$pos+4}]
            binary scan [string range $img $pos $endpos] "H4S" h length
            set length [expr {$length & 0xffff}]
            if {$h == "ffc0"} {
                incr pos 4
                set endpos [expr {$pos+6}]
                binary scan [string range $img $pos $endpos] "cSS" depth height width
                set height [expr {$height & 0xffff}]
                set width [expr {$width & 0xffff}]
                set imgOK true
                break
            } else {
                incr pos 2
                incr pos $length
            }
        }
        if {$imgOK} {
            lappend pdf(images) $id [list $width $height $depth $img_length $img]
        } else {
            return -code error "something is wrong with jpeg data in file $filename"
        }
    }

    method putImage {id x y args} {
        array set aimg $pdf(images)
        foreach {width height depth length data} $aimg($id) {break}

        set w $width
        set h $height
        set wfix 0
        set hfix 0
        foreach {arg value} $args {
            switch -- $arg {
                "-width" {set w $value; set wfix 1}
                "-height" {set h $value; set hfix 1}
            }
        }
        if {$wfix && !$hfix} {
            set h [expr {$height*$w/$width}]
        }
        if {$hfix && !$wfix} {
            set w [expr {$width*$h/$height}]
        }

        $self endTextObj
        if {$pdf(orient)} {
            set y [expr {$pdf(height)-$y-$h}]
        }
        $self pdfout "q\n$w 0 0 $h $x $y cm\n/$id Do\nQ\n"
        return
    }

    # start text object, if not already in text
    method beginTextObj {} {
        if {! $pdf(in_text_object)} {
            $self pdfout "BT\n"
            set pdf(in_text_object) true
        }
    }

    # end text object, if in text, else do nothing
    method endTextObj {} {
        if {$pdf(in_text_object)} {
            $self pdfout "ET\n"
            set pdf(in_text_object) false
        }
    }

    # helper function: mask parentheses and backslash
    proc cleanText {in} {
        return [string map {( \\( ) \\) \\ \\\\} $in]
    }

    # helper function: return current object id
    method get_oid {} {
        return $pdf(pdf_obj)
    }

    # helper function: return next object id (without incrementing)
    method next_oid {} {
        set oid [$self get_oid]
        return [expr {$oid+1}]
    }

    # helper function: increment object id and return new value
    method incr_oid {} {
        incr pdf(pdf_obj)
        return $pdf(pdf_obj)
    }

    # helper function: set xref of current oid to current out_pos
    method store_xref {} {
        set oid $pdf(pdf_obj)
        set pdf(xref,$oid) $pdf(out_pos)
    }

    # helper function for formatting floating point numbers
    proc nf {n} {
        # precision: 4 digits
        set factor 10000.0
        return [expr {round($n*$factor)/$factor}]
    }
}

# vim: tw=0
