# library of tcl procedures for generating portable document format files
# this is a port of pdf4php from php to tcl

# Copyright (c) 2004 by Frank Richter <frichter@truckle.in-chemnitz.de> and
#                       Jens P�nisch <jens@ruessel.in-chemnitz.de>
# Copyright (c) 2006-2008 by Peter Spjuth <peter.spjuth@gmail.com>

# See the file "licence.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# $Revision$ $Date$

# Version 0.1   base features for generating correct pdf files
# Version 0.2   more graphic operators, fixed font handling
# Version 0.3   Redesigned to use Snit. A lot of added features.

package provide pdf4tcl 0.3

package require pdf4tcl::metrics
package require pdf4tcl::glyphnames
package require snit

namespace eval pdf4tcl {
    # helper variables (constants) packaged into arrays to minimize
    # variable import statements
    variable g
    variable font_widths
    variable font_metrics
    variable glyph_names
    variable font_afm
    variable paper_sizes
    variable units

    # path to adobe afm files
    set g(ADOBE_AFM_PATH) {}
    # change this to reflect your machines install!
    lappend g(ADOBE_AFM_PATH) {/usr/share/texmf/fonts/afm/adobe/*}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/enscript}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/fonts/default/ghostscript}
    lappend g(ADOBE_AFM_PATH) {/usr/share/fonts/default/Type1}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/a2ps/afm}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/ogonkify/afm}
    lappend g(ADOBE_AFM_PATH) /usr/X11R6/lib/X11/fonts/Type1
    lappend g(ADOBE_AFM_PATH) /usr/lib/openoffice/share/psprint/fontmetric

    # font width array
    array set font_widths {}
    array set font_metrics {}

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

    # Known units. The value is their relationship to points
    array set units [list \
            mm [expr {72.0 / 25.4}] \
            m  [expr {72.0 / 25.4}] \
            cm [expr {72.0 / 2.54}] \
            c  [expr {72.0 / 2.54}] \
            i  72.0                 \
            p  1.0                  \
    ]

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
        #puts [join [lsort -dict [array names ::pdf4tcl::font_widths]] \n]
        #exit
    }

    # Utility to look up paper size by name
    # A two element list of width and height is also allowed.
    proc getPaperSize {papername} {
        variable paper_sizes

        set papername [string tolower $papername]
        if {[info exists paper_sizes($papername)]} {
            return $paper_sizes($papername)
        }
        if {[catch {set len [llength $papername]}] || $len != 2} {
            return {}
        }
        foreach {w h} $papername break
        set w [getPoints $w]
        set h [getPoints $h]
        return [list $w $h]
    }

    # Return a list of known paper sizes
    proc getPaperSizeList {} {
        variable paper_sizes
        return [array names paper_sizes]
    }

    # Get points from a measurement.
    # No unit means points.
    # Supported units are "mm", "m", "cm", "c", "p" and "i".
    proc getPoints {val} {
        variable units
        if {[string is double -strict $val]} {
            # Always return a pure double value
            return [expr {$val * 1.0}]
        }
        if {[regexp {^\s*(\S+?)\s*([[:alpha:]]+)\s*$} $val -> num unit]} {
            if {[string is double -strict $num]} {
                if {[info exists units($unit)]} {
                    return [expr {$num * $units($unit)}]
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
snit::type pdf4tcl::pdf4tcl { ##nagelfar nocover
    variable pdf

    #######################################################################
    # Global option handling
    #######################################################################

    option -file      -default "" -readonly 1
    option -paper     -default a4     -validatemethod CheckPaper \
            -configuremethod SetPageOption
    option -landscape -default 0      -validatemethod CheckBoolean \
            -configuremethod SetPageOption
    option -orient    -default 1      -validatemethod CheckBoolean
    option -compress  -default 0      -validatemethod CheckBoolean \
            -configuremethod SetCompress -readonly 1
    option -margin    -default 0      -validatemethod CheckMargin \
            -configuremethod SetPageOption

    # Validator for -paper
    method CheckPaper {option value} {
        set papersize [pdf4tcl::getPaperSize $value]
        if {[llength $papersize] == 0} {
            return -code error "papersize $value is unknown"
        }
    }

    # Validator for -margin
    method CheckMargin {option value} {
        switch [llength $value] {
            1 - 2 - 4 {
                foreach elem $value {
                    if {[catch {pdf4tcl::getPoints $elem}]} {
                        return -code error "Bad margin value '$elem'"
                    }
                }
            }
            default {
                return -code error "Bad margin list '$value'"
            }
        }
    }

    # Validator for boolean options
    method CheckBoolean {option value} {
        if {![string is boolean -strict $value]} {
            return -code error "option $option must have a boolean value."
        }
    }

    # Configure method for -compress
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

    # Configure method for page properties
    method SetPageOption {option value} {
        set options($option) $value
        # Fill in page properies
        $self SetPageSize   $options(-paper) $options(-landscape)
        $self SetPageMargin $options(-margin)
    }

    #######################################################################
    # Constructor
    #######################################################################

    constructor {args} {
        variable images

        $self configurelist $args

        # Document data
        set pdf(pages) 0
        set pdf(pdf_obj) 4
        set pdf(out_pos) 0
        set pdf(data_start) 0
        set pdf(data_len) 0
        set pdf(fonts) {}
        set pdf(font_set) false
        set pdf(in_text_object) false
        array set images {}
        set pdf(compress) $options(-compress)
        set pdf(finished) false
        set pdf(inPage) false
        set pdf(fillColor) [list 0 0 0]
        # start with Helvetica as default font
        set pdf(font_size) 12
        set pdf(current_font) "Helvetica"
        set pdf(line_spacing) 1.0

        # Page data
        # Fill in page properies
        $self SetPageSize   $options(-paper) $options(-landscape)
        $self SetPageMargin $options(-margin)
        set pdf(orient) $options(-orient)

        # output buffer (we need to compress whole pages)
        set pdf(ob) ""

        # Write to file directly if requested.
        set pdf(ch) ""
        if {$options(-file) ne ""} {
            if {[catch {open $options(-file) "w"} ch]} {
                return -code error "Could not open file $options(-file) for writing: $ch"
            }
            fconfigure $ch -translation binary
            set pdf(ch) $ch
        }

        # collect output in memory
        set pdf(pdf) ""

        # Start on pdfout
        $self Pdfoutn "%PDF-1.3"
    }

    destructor {
        # Close any open channel
        if {[info exists pdf(ch)] && $pdf(ch) ne ""} {
            catch {$self finish}
            catch {close $pdf(ch)}
            set pdf(ch) ""
        }
    }

    # Deprecated destroy function
    method cleanup {} {
        $self destroy
    }


    #######################################################################
    # Collect PDF Output
    #######################################################################

    # Add raw data to accumulated pdf output
    method Pdfout {out} {
        append pdf(ob) $out
        incr pdf(out_pos) [string length $out]
    }

    # Add line of words to accumulated pdf output
    method Pdfoutn {args} {
        set out [join $args " "]\n
        $self Pdfout $out
    }

    # Helper to format a line consisiting of numbers and last a command
    method Pdfoutcmd {args} {
        set str ""
        foreach num [lrange $args 0 end-1] {
            append str [Nf $num] " "
        }
        append str "[lindex $args end]\n"
        $self Pdfout $str
    }

    #######################################################################
    # Page Handling
    #######################################################################

    # Fill in page margin from a user specified value
    method SetPageMargin {value} {
        switch -- [llength $value] {
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
            default { ##nagelfar nocover
                # This should not happen since validation should catch it
                puts "ARARARARARAR '$value'"
            }
        }
    }

    # Fill in page data from options
    method SetPageSize {paper landscape} {
        set papersize [pdf4tcl::getPaperSize $paper]
        set width  [lindex $papersize 0]
        set height [lindex $papersize 1]

        # Switch if landscape has been asked for
        if {$landscape} {
            set tmp    $width
            set width  $height
            set height $tmp
        }
        set pdf(width)  $width
        set pdf(height) $height
        set pdf(xpos)   0
        set pdf(ypos)   $height
    }

    # Start on a new page
    method startPage {args} {
        # Get defaults from document
        set localopts(-orient)    $options(-orient)
        set localopts(-landscape) $options(-landscape)
        set localopts(-margin)    $options(-margin)
        set localopts(-paper)     $options(-paper)

        if {[llength $args] == 1} {
            # Single arg = paper
            $self CheckPaper -paper [lindex $args 0]
            set localopts(-paper) [lindex $args 0]
        } elseif {[llength $args] == 2 && [string is digit [join $args ""]]} {
            # Old style two numeric args = {width height}
            $self CheckPaper -paper $args
            set localopts(-paper) $args
        } elseif {[llength $args] == 3 && [string is digit [join $args ""]]} {
            # Old style three numeric args = {width height orient}
            $self CheckPaper -paper [lrange $args 0 1]
            set localopts(-paper)   [lrange $args 0 1]
            set localopts(-orient)  [lindex $args 2]
        } elseif {[llength $args] % 2 != 0} {
            # Uneven, error
            return -code error "Uneven number of arguments to startPage"
        } else {
            # Parse options
            foreach {option value} $args {
                switch -- $option {
                    -paper {
                        $self CheckPaper $option $value
                    }
                    -landscape {
                        $self CheckBoolean $option $value
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

        if {$pdf(inPage)} {
            $self endPage
        }
        # Fill in page properies
        $self SetPageSize $localopts(-paper) $localopts(-landscape)
        $self SetPageMargin $localopts(-margin)
        set pdf(orient) $localopts(-orient)

        set pdf(inPage) 1
        incr pdf(pages)

        # dimensions
        set oid [$self GetOid]
        $self StoreXref
        $self Pdfout "$oid 0 obj\n"
        $self Pdfout "<</Type /Page\n"
        $self Pdfout "/Parent 2 0 R\n"
        $self Pdfout "/Resources 3 0 R\n"
        $self Pdfout [format "/MediaBox \[0 0 %g %g\]\n" $pdf(width) $pdf(height)]
        $self Pdfout "/Contents \[[$self NextOid] 0 R \]\n"
        $self Pdfout ">>\n"
        $self Pdfout "endobj\n\n"

        # start of contents
        set oid [$self IncrOid]
        $self StoreXref
        $self Pdfout "$oid 0 obj\n"
        $self Pdfout "<<\n/Length [$self NextOid] 0 R\n"
        if {$pdf(compress)} {
            $self Pdfout "/Filter \[/FlateDecode\]\n"
        }
        $self Pdfout ">>\nstream\n"
        set pdf(data_start) $pdf(out_pos)
        set pdf(in_text_object) false
        $self IncrOid

        # no font set on new pages
        set pdf(font_set) false

        # capture output
        if {$pdf(ch) eq ""} {
            append pdf(pdf) $pdf(ob)
        } else {
            puts -nonewline $pdf(ch) $pdf(ob)
        }
        set pdf(ob) ""
    }

    # Finish a page
    method endPage {} {
        if {! $pdf(inPage)} {
            return
        }
        if {$pdf(in_text_object)} {
            $self Pdfout "\nET\n"
        }
        # get buffer
        set data $pdf(ob)
        set pdf(ob) ""
        if {$pdf(compress) >0} {
            set data [zlib compress $data]
        }
        if {$pdf(ch) eq ""} {
            append pdf(pdf) $data
        } else {
            puts -nonewline $pdf(ch) $data
        }
        set data_len [string length $data]
        set pdf(out_pos) [expr {$pdf(data_start)+$data_len}]
        $self Pdfout "\nendstream\n"
        $self Pdfout "endobj\n\n"
        $self StoreXref
        $self Pdfout "[$self GetOid] 0 obj\n"
        incr data_len
        $self Pdfout "$data_len\n"
        $self Pdfout "endobj\n\n"
        $self IncrOid
        set pdf(inPage) false
    }

    # Finish document
    method finish {} {
        variable images
        if {$pdf(finished)} {
            return
        }

        if {$pdf(inPage)} {
            $self endPage
        }
        set pdf(xref,1) $pdf(out_pos)
        $self Pdfout "1 0 obj\n"
        $self Pdfout "<<\n"
        $self Pdfout "/Type /Catalog\n"
        $self Pdfout "/Pages 2 0 R\n"
        $self Pdfout ">>\n"
        $self Pdfout "endobj\n\n"

        set pdf(xref,2) $pdf(out_pos)
        $self Pdfout "2 0 obj\n"
        $self Pdfout "<<\n/Type /Pages\n"
        $self Pdfout "/Count $pdf(pages)\n"
        $self Pdfout "/Kids \["
        for {set a 0} {$a<$pdf(pages)} {incr a} {
            set b [expr {4 + $a*3}]
            $self Pdfout "$b 0 R "
        }
        $self Pdfout "\]\n"
        $self Pdfout ">>\n"
        $self Pdfout "endobj\n\n"

        set pdf(xref,3) $pdf(out_pos)
        $self Pdfout "3 0 obj\n"
        $self Pdfout "<<\n"
        $self Pdfout "/ProcSet\[/PDF /Text /ImageC\]\n"
        $self Pdfout "/Font <<\n"

        # font references
        set count 0
        foreach fontname $pdf(fonts) {
            set nr [expr {$pdf(pdf_obj)+$count}]
            $self Pdfout "/$fontname $nr 0 R\n"
            incr count
        }
        $self Pdfout ">>\n"

        # image references
        if {[array size images]>0} {
            $self Pdfout "/XObject <<\n"
            foreach key [array names images] {
                set nr [expr {$pdf(pdf_obj)+$count}]
                $self Pdfout "/$key $nr 0 R\n"
                incr count
            }
            $self Pdfout ">>\n"
        }
        $self Pdfout ">>\nendobj\n\n"

        # fonts
        foreach fontname $pdf(fonts) {
            $self StoreXref
            $self Pdfout "[$self GetOid] 0 obj\n"
            $self Pdfout "<<\n/Type /Font\n"
            $self Pdfout "/Subtype /Type1\n"
            $self Pdfout "/Encoding /WinAnsiEncoding\n"
            $self Pdfout "/Name /$fontname\n"
            $self Pdfout "/BaseFont /$fontname\n"
            $self Pdfout ">>\n"
            $self Pdfout "endobj\n\n"
            $self IncrOid
        }

        # images
        foreach key [array names images] {
            $self StoreXref
            foreach {img_width img_height xobject} $images($key) {break}
            $self Pdfout "[$self GetOid] 0 obj\n"
            $self Pdfout $xobject
            $self IncrOid
        }

        # cross reference
        set xref_pos $pdf(out_pos)
        $self Pdfout "xref\n"
        $self StoreXref
        $self Pdfout "0 [$self GetOid]\n"
        $self Pdfout "0000000000 65535 f \n"
        for {set a 1} {$a<[$self GetOid]} {incr a} {
            set xref $pdf(xref,$a)
            $self Pdfout [format "%010ld 00000 n \n" $xref]
        }
        $self Pdfout "trailer\n"
        $self Pdfout "<<\n"
        $self Pdfout "/Size [$self GetOid]\n"
        $self Pdfout "/Root 1 0 R\n"
        $self Pdfout ">>\n"
        $self Pdfout "\nstartxref\n"
        $self Pdfout "$xref_pos\n"
        $self Pdfout "%%EOF\n"
        if {$pdf(ch) eq ""} {
            append pdf(pdf) $pdf(ob)
        } else {
            puts -nonewline $pdf(ch) $pdf(ob)
        }
        set pdf(ob) ""
        set pdf(finished) true
    }

    # Get finished PDF data
    method get {} {
        if {$pdf(inPage)} {
            $self endPage
        }
        if {! $pdf(finished)} {
            $self finish
        }
        return $pdf(pdf)
    }

    # Write PDF data to file
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
    }

    # Transform absolute user coordinates to page coordinates
    # This should take into account orientation, margins.
    method Trans {x y txName tyName} {
        upvar 1 $txName tx $tyName ty

        set px [pdf4tcl::getPoints $x]
        set py [pdf4tcl::getPoints $y]

        set tx [expr {$px + $pdf(marginleft)}]
        if {$pdf(orient)} {
            set ty [expr {$py + $pdf(margintop)}]
            set ty [expr {$pdf(height) - $ty}]
        } else {
            set ty [expr {$py + $pdf(marginbottom)}]
        }
    }

    # Transform relative user coordinates to page coordinates
    # This should take into account orientation, but not margins.
    method TransR {x y txName tyName} {
        upvar 1 $txName tx $tyName ty

        set tx [pdf4tcl::getPoints $x]
        set ty [pdf4tcl::getPoints $y]

        if {$pdf(orient)} {
            set ty [expr {- $ty}]
        }
    }

    # Returns width and height of drawable area, excluding margins.
    method getDrawableArea {} {
        set w [expr {$pdf(width)  - $pdf(marginleft) - $pdf(marginright)}]
        set h [expr {$pdf(height) - $pdf(margintop)  - $pdf(marginbottom)}]
        return [list $w $h]
    }

    #######################################################################
    # Text Handling
    #######################################################################

    # Set current font
    method setFont {size {fontname ""}} {
        variable ::pdf4tcl::font_widths

        if {$fontname eq ""} {
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
        $self Pdfoutn "/$fontname $size" "Tf"
        $self Pdfoutcmd 0 "Tr"
        $self Pdfoutcmd $size "TL"
        if {[lsearch $pdf(fonts) $fontname]==-1} {
            lappend pdf(fonts) $fontname
        }
        set pdf(current_font) $fontname

        set pdf(font_set) true
    }

    # Load font metrics from AFM file
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

    # Get metrics from current font.
    # Supported metrics are ascend, descend, fixed, bboxy, height
    method getFontMetric {metric} {
        array set tmp $::pdf4tcl::font_metrics($pdf(current_font))
        switch $metric {
            bboxy   {set val [lindex $tmp(bbox) 1]}
            fixed   {return $tmp(fixed)}
            height  {return [expr {1.0 * $pdf(font_size)}]}
            default {set val $tmp($metric)}
        }
        return [expr {$val * 0.001 * $pdf(font_size)}]
    }

    # Get the width of a string under the current font.
    method getStringWidth {txt} {
        set w 0.0
        foreach ch [split $txt ""] {
            set w [expr {$w + [GetCharWidth $pdf(current_font) $ch]}]
        }
        return [expr {$w * $pdf(font_size)}]
    }

    # Get the width of a character. "ch" must be exacly one char long.
    # This is a proc for performance reasons since it is called a lot.
    # Currently this is four times slower as a method.
    # With a method it would be preferable to keep the cache in
    # the instance to clean things up.
    proc GetCharWidth {font ch} {
        if {[info exists ::pdf4tcl::FontWidthsCh($font,$ch)]} {
            return $::pdf4tcl::FontWidthsCh($font,$ch)
        }

        if {$ch eq "\n"} {
            set res 0.0
            set ::pdf4tcl::FontWidthsCh($font,$ch) $res
            return $res
        }

        if {![info exists ::pdf4tcl::FontWidthsCurrent] || \
                    $::pdf4tcl::FontWidthsCurrent ne $font} {
            array unset ::pdf4tcl::FontWidths
            array set ::pdf4tcl::FontWidths $::pdf4tcl::font_widths($font)
            set ::pdf4tcl::FontWidthsCurrent $font
        }

        # This can't fail since ch is always 1 char long
        scan $ch %c n

        set ucs2 [format "%04.4X" $n]

        set glyph_name zero
        set w 0
        catch {set w $::pdf4tcl::FontWidths(zero)}
        catch {set glyph_name $::pdf4tcl::glyph_names($ucs2)}
        if {$glyph_name eq "spacehackarabic"} {set glyph_name "space"}

        catch {set w $::pdf4tcl::FontWidths($glyph_name)}
        ###puts stderr "ch: $ch  n: $n  ucs2: $ucs2  glyphname: $glyph_name  width: $w"
        set res [expr {$w * 0.001}]
        set ::pdf4tcl::FontWidthsCh($font,$ch) $res
        return $res
    }

    # Get the width of a character under the current font.
    method getCharWidth {ch} {
        set len [string length $ch]
        if {$len == 0} {
            return 0.0
        } elseif {$len > 1} {
            set ch [string index $ch 0]
        }
        return [expr {[GetCharWidth $pdf(current_font) $ch] * $pdf(font_size)}]
    }

    # Set coordinate for next text command.
    method setTextPosition {x y {internal 0}} {
        $self beginTextObj
        if {$internal} {
            set pdf(xpos) $x
            set pdf(ypos) $y
        } else {
            $self Trans $x $y pdf(xpos) pdf(ypos)
            # Store for reference
            set pdf(origxpos) $pdf(xpos)
            set pdf(origypos) $pdf(ypos)
        }

        $self Pdfoutcmd 1 0 0 1 $pdf(xpos) $pdf(ypos) "Tm"
    }

    # Draw text at current position, with a newline before
    method drawText {str} {
        $self beginTextObj
        if {! $pdf(font_set)} {
            $self setFont $pdf(font_size) $pdf(current_font)
        }
        $self Pdfout "([CleanText $str]) '\n"
        # Update to next line
        set strWidth [$self getStringWidth $str]
        set pdf(ypos) [expr {$pdf(ypos) - $pdf(font_size) * $pdf(line_spacing)}]
        set pdf(xpos) [expr {$pdf(origxpos) + $strWidth}]
    }

    # Move text position to new line, relative to last
    # setTextPosition command.
    method newLine {{spacing {}}} {
        if {$spacing eq ""} {
            set spacing $pdf(line_spacing)
        } elseif {![string is double -strict $spacing]} {
            return -code error "Line spacing must be a number"
        }
        # Update to next line
        set y [expr {$pdf(ypos) - $pdf(font_size) * $spacing}]
        set x $pdf(origxpos)
        $self setTextPosition $x $y 1
    }

    # Set Line spacing factor (which is used by method newLine
    # if no explicit spacing is given)
    method setLineSpacing {spacing} {
        if {![string is double -strict $spacing]} {
            return -code error "Line spacing must be a number"
        }
        set pdf(line_spacing) $spacing
    }

    # Return the current line spacing factor
    method getLineSpacing {} {
        return $pdf(line_spacing)
    }

    # Draw a text string
    # Returns the width of the drawn string.
    method text {str args} {
        set align "left"
        set angle 0
        set fill 0
        set x $pdf(xpos)
        set y $pdf(ypos)
        set posSet 0

        foreach {arg value} $args {
            switch -- $arg {
                "-align" {
                    set align $value
                }
                "-angle" {
                    set angle $value
                }
                "-fill" {
                    set fill $value
                }
                "-y" {
                    $self Trans 0 $value _ y
                    set posSet 1
                }
                "-x" {
                    $self Trans $value 0 x _
                    set posSet 1
                }
                default {
                    return -code error \
                            "unknown option $arg"
                }
            }
        }

        if {! $pdf(font_set)} {
            $self setFont $pdf(font_size)
        }

        set strWidth [$self getStringWidth $str]
        if {$align == "right"} {
            set x [expr {$x - $strWidth}]
            set posSet 1
        } elseif {$align == "center"} {
            set x [expr {$x - $strWidth / 2 * cos($angle*3.1415926/180.0)}]
            set y [expr {$y - $strWidth / 2 * sin($angle*3.1415926/180.0)}]
            set posSet 1
        }
        if {[llength $fill] > 1 || $fill} {
            set bboxy [$self getFontMetric bboxy]
            set dy [expr {$y + $bboxy}]
            $self endTextObj
            # Temporarily shift fill color
            if {[llength $fill] > 1} {
                $self Pdfout "$fill rg\n"
            } else {
                $self Pdfout "$pdf(bgColor) rg\n"
            }
            $self DrawRect $x $dy $strWidth $pdf(font_size) 0 1
            $self Pdfout "$pdf(fillColor) rg\n"
            # Position needs to be set since we left the text object
            set posSet 1
        }
        $self beginTextObj
        if {$angle != 0} {
            set pdf(xpos) $x
            set pdf(ypos) $y
            $self rotateText $angle
        } elseif {$posSet} {
            $self setTextPosition $x $y 1
        }
        $self Pdfout "([CleanText $str]) Tj\n"
        set pdf(xpos) [expr {$x + $strWidth}]
        return $strWidth
    }

    # Draw a text string at a given position.
    # This function is deprecated, use text.
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

        if {! $pdf(font_set)} {
            $self setFont $pdf(font_size)
        }

        $self Trans $x $y x y
        set strWidth [$self getStringWidth $str]
        if {$align == "right"} {
            set x [expr {$x - $strWidth}]
        } elseif {$align == "center"} {
            set x [expr {$x - $strWidth / 2 * cos($angle*3.1415926/180.0)}]
            set y [expr {$y - $strWidth / 2 * sin($angle*3.1415926/180.0)}]
        }
        $self beginTextObj
        if {$angle != 0} {
            set pdf(xpos) $x
            set pdf(ypos) $y
            $self rotateText $angle
        } else {
            $self setTextPosition $x $y 1
        }
        $self Pdfout "([CleanText $str]) Tj\n"
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
        set font_height [expr {$pdf(font_size) * $pdf(line_spacing)}]
        set space_width [$self getCharWidth " "]
        set ystart $y
        if {!$pdf(orient)} { #FIXA
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
                if {$pdf(orient)} { #FIXA
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

    method rotateText {angle} {
        $self beginTextObj
        set rad [expr {$angle*3.1415926/180.0}]
        set c [expr {cos($rad)}]
        set s [expr {sin($rad)}]
        $self Pdfoutcmd $c [expr {-$s}] $s $c $pdf(xpos) $pdf(ypos) "Tm"
    }

    method skewText {xangle yangle} {
        set tx [expr {tan($xangle*3.1415926/180.0)}]
        set ty [expr {tan($yangle*3.1415926/180.0)}]
        $self Pdfoutcmd 1 $tx $ty 1 $pdf(xpos) $pdf(ypos) "Tm"
        set pdf(xpos) 0
        set pdf(ypos) $pdf(height)
    }

    # start text object, if not already in text
    method beginTextObj {} {
        if {! $pdf(in_text_object)} {
            $self Pdfout "BT\n"
            set pdf(in_text_object) true
        }
    }

    # end text object, if in text, else do nothing
    method endTextObj {} {
        if {$pdf(in_text_object)} {
            $self Pdfout "ET\n"
            set pdf(in_text_object) false
        }
    }

    #######################################################################
    # Graphics Handling
    #######################################################################

    ###<jpo 2004-11-08: replaced "on off" by "args"
    ###                 to enable resetting dashed lines
    method setLineStyle {width args} {
        $self endTextObj
        $self Pdfoutcmd $width "w"
        $self Pdfout "\[$args\] 0 d\n"
    }

    method line {x1 y1 x2 y2 {internal 0}} {
        $self endTextObj
        if {!$internal} {
            $self Trans $x1 $y1 x1 y1
            $self Trans $x2 $y2 x2 y2
        }
        $self Pdfoutcmd $x1 $y1 "m"
        $self Pdfoutcmd $x2 $y2 "l"
        $self Pdfoutcmd "S"
    }

    ###>2004-11-03 jpo
    method qCurve {x1 y1 xc yc x2 y2} {
        $self endTextObj
        $self Trans $x1 $y1 x1 y1
        $self Trans $xc $yc xc yc
        $self Trans $x2 $y2 x2 y2
        $self Pdfoutcmd $x1 $y1 "m"
        $self Pdfoutcmd \
                [expr {0.3333*$x1+0.6667*$xc}] \
                [expr {0.3333*$y1+0.6667*$yc}] \
                [expr {0.3333*$x2+0.6667*$xc}] \
                [expr {0.3333*$y2+0.6667*$yc}] \
                $x2 \
                $y2 "c"
        $self Pdfoutcmd "S"
    }
    ###<jpo

    ###>2004-11-07 jpo
    # polygon name isFilled x0 y0 x1 y1 ...
    method polygon {isFilled args} {
        $self endTextObj
        if {$isFilled} {set op "b"} else {set op "s"}
        set start 1
        foreach {x y} $args {
            $self Trans $x $y x y
            if {$start} {
                $self Pdfoutcmd $x $y "m"
                set start 0
            } else {
                $self Pdfoutcmd $x $y "l"
            }
        }
        $self Pdfoutcmd $op
    }

    method circle {isFilled x y r} {
        $self endTextObj
        if {$isFilled} {
            set op "b"
        } else {
            set op "s"
        }
        $self Trans $x $y x y
        set r [pdf4tcl::getPoints $r]

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
        $self Pdfoutcmd $x0(0) $y0(0) "m"
        for {set i 0} {$i < 4} {incr i} {
            $self Pdfoutcmd $x1($i) \
                            $y1($i) \
                            $x2($i) \
                            $y2($i) \
                            $x3($i) \
                            $y3($i) "c"
        }
        $self Pdfoutcmd " $op"
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

        $self Trans $x0 $y0 x0 y0
        set r [pdf4tcl::getPoints $r]

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
        $self Pdfoutcmd $x $y "m"
        set points [simplearc $phi2]
        set phi [expr {$phi+$phi2}]
        for {set i 0} {$i < $count} {incr i} {
            foreach {x y x1 y1 x2 y2 x3 y3} \
                    [transform $r $phi $x0 $y0 $points] break
            set phi [expr {$phi+$extend}]
            $self Pdfoutcmd $x1 $y1 $x2 $y2 $x3 $y3 "c"
        }
        $self Pdfout " S\n"
    }
    ###<jpo

    method arrow {x1 y1 x2 y2 sz {angle 20}} {
        $self Trans $x1 $y1 x1 y1
        $self Trans $x2 $y2 x2 y2
        set sz [pdf4tcl::getPoints $sz]

        $self line $x1 $y1 $x2 $y2 1
        set rad [expr {$angle*3.1415926/180.0}]
        set ang [expr {atan2(($y1-$y2), ($x1-$x2))}]
        $self line $x2 $y2 [expr {$x2+$sz*cos($ang+$rad)}] [expr {$y2+$sz*sin($ang+$rad)}] 1
        $self line $x2 $y2 [expr {$x2+$sz*cos($ang-$rad)}] [expr {$y2+$sz*sin($ang-$rad)}] 1
    }

    method setBgColor {red green blue} {
        set pdf(bgColor) [list $red $green $blue]
    }

    method setFillColor {red green blue} {
        set pdf(fillColor) [list $red $green $blue]
        $self Pdfout "$red $green $blue rg\n"
    }

    method setStrokeColor {red green blue} {
        set pdf(strokeColor) [list $red $green $blue]
        $self Pdfout "$red $green $blue RG\n"
    }

    # Draw a rectangle, internal version
    method DrawRect {x y w h stroke filled} {
        $self Pdfoutcmd $x $y $w $h "re"
        if {$filled && $stroke} {
            $self Pdfoutcmd "B"
        } elseif {$filled && !$stroke} {
            $self Pdfoutcmd "f"
        } else {
            $self Pdfoutcmd "S"
        }
    }

    # Draw a rectangle
    method rectangle {x y w h args} {
        set filled 0
        set stroke 1
        foreach {arg value} $args {
            switch -- $arg {
                "-filled" {
                    set filled 1
                }
                "-nostroke" {
                    set stroke 0
                }
                default {
                    return -code error "unknown option $arg"
                }
            }
        }
        $self endTextObj
        $self Trans $x $y x y
        $self TransR $w $h w h

        $self DrawRect $x $y $w $h $stroke $filled
    }

    method moveTo {x1 y1} {
        $self endTextObj
        $self Trans $x1 $y1 x1 y1
        $self Pdfoutcmd $x1 $y1 "m"
    }

    method closePath {} {
        $self Pdfout "b\n"
    }

    #######################################################################
    # Image Handling
    #######################################################################

    method GetImageId {} {
        variable images
        # Find a unique name. Could be improved...
        set last [lindex [lsort -dictionary [array names images _image*]] end]
        if {![regexp {\d+$} $last i]} {
            set i 0
        }
        while {[info exists images(_image$i)]} {
            incr i
        }
        return _image$i
    }

    # Add an image to the document
    method addImage {filename args} {
        set id ""
        set type ""
        foreach {arg val} $args {
            switch -- $arg {
                -id {
                    set id $val
                }
                -type {
                    set type $val
                }
            }
        }
        if {$id eq ""} {
            set id [$self GetImageId]
        }

        if {$type eq ""} {
            switch -glob $filename {
                *.png {
                    set type png
                }
                *.jpg - *.jpeg {
                    set type jpg
                }
                default {
                    return -code error "Unknown image type $filename"
                }
            }
        }
        switch $type {
            png {
                $self AddPng $filename $id
            }
            jpg - jpeg {
                $self addJpeg $filename $id
            }
            default {
                return -code error "Unknown image type $type"
            }
        }
        return $id
    }

    # Deprecated jpeg adder, use addImage
    method addJpeg {filename id} {
        variable images

        set imgOK false
        if {[catch {open $filename "r"} if]} {
            return -code error "Could not open file $filename"
        }

        fconfigure $if -translation binary
        set img [read $if]
        close $if
        binary scan $img "H4" h
        if {$h != "ffd8"} {
            return -code error "file $filename does not contain JPEG data."
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
        if {!$imgOK} {
            return -code error "something is wrong with jpeg data in file $filename"
        }
        set    xobject "<<\n/Type /XObject\n"
        append xobject "/Subtype /Image\n"
        append xobject "/Width $width\n/Height $height\n"
        append xobject "/ColorSpace /DeviceRGB\n"
        append xobject "/BitsPerComponent $depth\n"
        append xobject "/Filter /DCTDecode\n"
        append xobject "/Length $img_length >>\n"
        append xobject "stream\n"
        append xobject $img
        append xobject "\nendstream\n"
        append xobject "endobj\n\n"
        set images($id) [list $width $height $xobject]
        return $id
    }

    # Experimental Png-function
    method AddPng {filename id} {
        variable images

        set imgOK false
        if {[catch {open $filename "r"} if]} {
            return -code error "Could not open file $filename"
        }

        fconfigure $if -translation binary
        if {[read $if 8] != "\x89PNG\r\n\x1a\n"} {
            close $if
            return -code error "file does not contain PNG data"
        }
        set img [read $if]
        close $if

        set pos 0
        set img_length [string length $img]
        set img_data ""
        set palette ""
        while {$pos < $img_length} {
            # Scan one chunk
            binary scan $img "@${pos}Ia4" length type
            incr pos 8
            set data [string range $img $pos [expr {$pos + $length - 1}]]
            incr pos $length
            binary scan $img "@${pos}I" crc
            incr pos 4

            switch $type {
                "IHDR" {
                    set imgOK 1
                    binary scan $data IIccccc width height depth color \
                            compression filter interlace
                }
                "PLTE" {
                    set palette $data
                }
                "IDAT" {
                    append img_data $data
                }
            }
        }

        if {!$imgOK} {
            return -code error "something is wrong with PNG data in file $filename"
        }
        if {[string length $img_data] == 0} {
            return -code error "file does not contain any IDAT chunks"
        }
        if {$compression != 0} {
            return -code error "PNG file is of an unsupported compression type"
        }
        if {$filter != 0} {
            return -code error "PNG file is of an unsupported filter type"
        }
        if {$interlace != 0} {
            # Would need to unpack and repack to do interlaced
            return -code error "Interlaced PNG is not supported"
        }

        if {$palette ne ""} {
            # Transform the palette into a PDF Indexed color space
            binary scan $palette H* PaletteHex
            set PaletteLen [expr {[string length $palette] / 3 - 1}]
            set paletteX "\[ /Indexed /DeviceRGB "
            append paletteX $PaletteLen " < "
            append paletteX $PaletteHex
            append paletteX " > \]"
        }

        set    xobject "<<\n/Type /XObject\n"
        append xobject "/Subtype /Image\n"
        append xobject "/Width $width\n/Height $height\n"

        switch $color {
            0 { # Grayscale
                append xobject "/ColorSpace /DeviceGray\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 1 /BitsPerComponent $depth /Columns $width>>\n"
            }
            2 { # RGB
                append xobject "/ColorSpace /DeviceRGB\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 3 /BitsPerComponent $depth /Columns $width>>\n"
            }
            3 { # Palette
                append xobject "/ColorSpace $paletteX\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 1 /BitsPerComponent $depth /Columns $width>>\n"
            }
            4 { # Gray + alpha
                return -code error "PNG with alpha channel is not supported"
            }
            6 { # RGBA
                # TODO:
                # Either unpack the data stream, remove the alpha and repack.
                # Or, figure out a PDF colorspace that can understand RGBA
                return -code error "PNG with alpha channel is not supported"
            }
        }

        append xobject "/Length [string length $img_data] >>\n"
        append xobject "stream\n"
        append xobject $img_data
        append xobject "\nendstream\n"
        append xobject "endobj\n\n"
        set images($id) [list $width $height $xobject]
    }

    method putImage {id x y args} {
        variable images
        foreach {width height xobject} $images($id) {break}

        $self Trans $x $y x y
        set w $width
        set h $height
        set wfix 0
        set hfix 0
        foreach {arg value} $args {
            switch -- $arg {
                "-width"  {set w [pdf4tcl::getPoints $value]; set wfix 1}
                "-height" {set h [pdf4tcl::getPoints $value]; set hfix 1}
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
            set y [expr {$y-$h}]
        }
        $self Pdfoutcmd "q"
        $self Pdfoutcmd $w 0 0 $h $x $y "cm"
        $self Pdfout "/$id Do\nQ\n"
    }

    method addRawImage {img_data args} {
        variable images
        # Determine the width and height of the image, which is
        # a list of lists(rows).
        set width [llength [lindex $img_data 0]]
        set height [llength $img_data]

        set id ""
        foreach {arg value} $args {
            switch -- $arg {
                "-id"     {set id $value}
            }
        }
        if {$id eq ""} {
            set id [$self GetImageId]
        }

        set    xobject "<<\n/Type /XObject\n"
        append xobject "/Subtype /Image\n"
        append xobject "/Width $width\n/Height $height\n"
        append xobject "/ColorSpace /DeviceRGB\n"
        append xobject "/BitsPerComponent 8>>\n"
        append xobject "stream\n"

        # Iterate on each row of the image data.
        set img ""
        foreach rawRow $img_data {
            # Remove spaces and # characters
            regsub -all -- {((\#)|( ))} $rawRow {} row
            # Convert data to binary format and
            # add to data stream.
            append img [binary format H* $row]
        }

        append xobject $img
        append xobject "\nendstream\n"
        append xobject "endobj\n\n"

        set images($id) [list $width $height $xobject]
        return $id
    }

    method putRawImage {img_data x y args} {
        # Determine the width and height of the image, which is
        # a list of lists(rows).
        set width [llength [lindex $img_data 0]]
        set height [llength $img_data]

        $self Trans $x $y x y
        set w $width
        set h $height
        set wfix 0
        set hfix 0
        foreach {arg value} $args {
            switch -- $arg {
                "-width"  {set w [pdf4tcl::getPoints $value]; set wfix 1}
                "-height" {set h [pdf4tcl::getPoints $value]; set hfix 1}
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
            set y [expr {$y-$h}]
        }
        $self Pdfoutcmd "q"
        $self Pdfoutcmd $w 0 0 $h $x $y "cm"
        $self Pdfoutcmd "BI"
        $self Pdfoutn   "/W [Nf $width]"
        $self Pdfoutn   "/H [Nf $height]"
        $self Pdfoutn   "/CS /RGB"
        $self Pdfoutn   "/BPC 8"
        $self Pdfoutcmd "ID"

        # Iterate on each row of the image data.
        foreach rawRow $img_data {
            # Remove spaces and # characters
            regsub -all -- {((\#)|( ))} $rawRow {} row
            # Convert data to binary format and
            # add to data stream.
            $self Pdfout [binary format H* $row]
        }

        $self Pdfout    \n
        $self Pdfoutcmd "EI"
        $self Pdfoutcmd "Q"
    }

    #######################################################################
    # Helper fuctions
    #######################################################################

    # helper function: mask parentheses and backslash
    proc CleanText {in} {
        return [string map {( \\( ) \\) \\ \\\\} $in]
    }

    # helper function: return current object id
    method GetOid {} {
        return $pdf(pdf_obj)
    }

    # helper function: return next object id (without incrementing)
    method NextOid {} {
        set oid [$self GetOid]
        return [expr {$oid+1}]
    }

    # helper function: increment object id and return new value
    method IncrOid {} {
        incr pdf(pdf_obj)
        return $pdf(pdf_obj)
    }

    # helper function: set xref of current oid to current out_pos
    method StoreXref {} {
        set oid $pdf(pdf_obj)
        set pdf(xref,$oid) $pdf(out_pos)
    }

    # helper function for formatting floating point numbers
    proc Nf {n} {
        # Up to 3 decimals
        set num [expr {round($n * 1000.0) / 1000.0}]
        # Remove surplus decimals
        return [string trimright [string trimright $num "0"] "."]
    }
}

# vim: tw=0
