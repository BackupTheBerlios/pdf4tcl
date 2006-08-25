#!/bin/env tclsh
#
# A file for experiments

set ::auto_path [concat [list [pwd]] $::auto_path]
package require pdf4tcl
package require snit

snit::type printexp {
    component pdf
    delegate method * to pdf

    delegate option -margin to pdf
    delegate option -paper  to pdf

    option -cpl      -default 80
    option -cpln     -default 5
    option -headsize -default 8
    option -file     -default exp.pdf

    variable width
    variable height
    variable hoy
    variable foy
    variable fontsize
    variable linesize
    variable nlines
    variable ox1
    variable ox2
    variable oy
    variable page

    constructor {args} {
        install pdf using pdf4tcl::pdf4tcl %AUTO% \
                -landscape 1 -paper a4 -margin 15mm
        $self configurelist $args
        $self StartPrint
    }

    method StartPrint {} {
        # Page size
        foreach {width height} [$pdf getDrawableArea] break

        # Header metrics
        $pdf setFont $options(-headsize) Courier
        set headoffset [expr {$options(-headsize) + [$pdf getFontMetric bboxy]}]
        set hoy $headoffset
        set foy [expr {$height - $options(-headsize) + $headoffset}]

        # Figure out font size from number of chars per line
        set charwidthHead [$pdf getCharWidth "0"]
        set charwidth [expr {$width / 2.0 / ($options(-cpl) + $options(-cpln) + 1)}]
        set fontsize [expr {$options(-headsize) * $charwidth / $charwidthHead}]
        $pdf setFont $fontsize

        # Text metrics
        set linesize  $fontsize
        set offset    [expr {$fontsize + [$pdf getFontMetric bboxy]}]
        set charwidth [$pdf getCharWidth "0"]
        set nlinesf [expr {($height - 2 * $options(-headsize)) / $linesize}]
        # Number of lines per page
        set nlines  [expr {int($nlinesf - 1.0)}]
        set nlines 66
        # Offsets to starting points in both subpages.
        set ox1 $charwidth
        set ox2 [expr {$width / 2.0 + $charwidth}]
        set oy  [expr {($nlinesf - $nlines) / 2.0 * $linesize + \
                                     $offset + $headoffset}]
        # Reset current page
        set page 0
    }

    # Start a new page
    method newPage {} {
        $pdf startPage
        incr page

        # Draw borders
        $pdf setStrokeColor 0 0 0
        $pdf setFillColor 0.0 0.0 0.0
        $pdf setLineStyle 0.5
        # Outer border
        $pdf rectangle 0 $options(-headsize) \
                $width [expr {$height - 2 * $options(-headsize)}]
        # Center line
        $pdf line [expr {$width / 2.0}] $options(-headsize) \
                [expr {$width / 2.0}] [expr {$height - $options(-headsize)}]

        # Header
        $pdf setFont $options(-headsize) Courier
        $pdf drawTextAt 0 $hoy "Header Text Left"
        $pdf drawTextAt [expr {$width / 2.0}] $hoy \
                "Header Text C Page $page" -align center
        $pdf drawTextAt $width $hoy "Header Text Right" -align right
        # Footer
        $pdf drawTextAt 0 $foy "Footer Text Left"
        $pdf drawTextAt [expr {$width / 2.0}] $foy \
                "Footer Text C" -align center
        $pdf drawTextAt $width $foy "Footer Text Right" -align right

        # Normal font
        $pdf setFont $fontsize Courier
    }

    # Produce one page
    method onePage {} {

        $self newPage

        # Dummy strings
        set longstr  [string repeat "MifjgqIo" 100]
        set strcpl   [string range $longstr 0 [expr {$options(-cpl) - 1}]]
        set strcpl10 [string range $longstr 0 [expr {$options(-cpl) + 9}]]

        # Text
        for {set line 0} {$line < $nlines} {incr line} {
            if {$line % 2 == 0} {
                $pdf setBgColor 1.0 0.5 0.5
            } else {
                $pdf setBgColor 0.5 1.0 0.5
            }
            $pdf text $line -x $ox1 -y [expr {$oy + $line * $linesize}]
            $pdf text "hejsan" -fill 1
            $pdf text "hoppsan" -fill "0.5 0.5 1.0"
            $pdf text "Miffo"
            #$pdf drawTextAt $ox2 [expr {$oy + $line * $linesize}] "Hejsan" -fill 1
            $pdf drawTextAt $ox2 [expr {$oy + $line * $linesize}] $strcpl -fill 1
        }
    }

    # Finish a print job
    method endPrint {} {
        $pdf write -file $options(-file)
        $pdf destroy
        $self destroy
    }
}


# Start a new print job
proc StartPrint {} {
    # Settings
    set ::pr(cpl) 80
    set ::pr(cpln) 5
    set ::pr(headsize) 8
    set ::pr(paper) a4
    set ::pr(margin) 15mm

    # Create object
    set ::pr(obj) [pdf4tcl::new %AUTO% -paper $::pr(paper) -landscape 1 \
                       -margin $::pr(margin)]

    # Page size
    foreach {::pr(w) ::pr(h)} [$::pr(obj) getDrawableArea] break

    # Header metrics
    $::pr(obj) setFont $::pr(headsize) Courier
    set headoffset [expr {$::pr(headsize) + [$::pr(obj) getFontMetric bboxy]}]
    set ::pr(hoy) $headoffset
    set ::pr(foy) [expr {$::pr(h) - $::pr(headsize) + $headoffset}]

    # Figure out font size from number of chars per line
    set charwidthHead [$::pr(obj) getCharWidth "0"]
    set charwidth [expr {$::pr(w) / 2.0 / ($::pr(cpl) + $::pr(cpln) + 1)}]
    set ::pr(fontsize) [expr {$::pr(headsize) * $charwidth / $charwidthHead}]
    $::pr(obj) setFont $::pr(fontsize)

    # Text metrics
    set ::pr(linesize)  $::pr(fontsize)
    set offset    [expr {$::pr(fontsize) + [$::pr(obj) getFontMetric bboxy]}]
    set charwidth [$::pr(obj) getCharWidth "0"]
    set nlinesf [expr {($::pr(h) - 2 * $::pr(headsize)) / $::pr(linesize)}]
    # Number of lines per page
    set ::pr(nlines)  [expr {int($nlinesf - 1.0)}]
    # Offsets to starting points in both subpages.
    set ::pr(ox1) $charwidth
    set ::pr(ox2) [expr {$::pr(w) / 2.0 + $charwidth}]
    set ::pr(oy)  [expr {($nlinesf - $::pr(nlines)) / 2.0 * $::pr(linesize) + \
                             $offset + $headoffset}]
    # Reset current page
    set ::pr(page) 0
}

# Start a new page
proc NewPage {} {
    $::pr(obj) startPage
    incr ::pr(page)

    # Draw borders
    $::pr(obj) setStrokeColor 0 0 0
    $::pr(obj) setFillColor 0.0 0.0 0.0
    $::pr(obj) setLineStyle 0.5
    # Outer border
    $::pr(obj) rectangle 0 $::pr(headsize) \
        $::pr(w) [expr {$::pr(h) - 2 * $::pr(headsize)}]
    # Center line
    $::pr(obj) line [expr {$::pr(w) / 2.0}] $::pr(headsize) \
            [expr {$::pr(w) / 2.0}] [expr {$::pr(h) - $::pr(headsize)}]

    # Header
    $::pr(obj) setFont $::pr(headsize) Courier
    $::pr(obj) drawTextAt 0 $::pr(hoy) "Header Text Left"
    $::pr(obj) drawTextAt [expr {$::pr(w) / 2.0}] $::pr(hoy) \
        "Header Text C Page $::pr(page)" -align center
    $::pr(obj) drawTextAt $::pr(w) $::pr(hoy) "Header Text Right" -align right
    # Footer
    $::pr(obj) drawTextAt 0 $::pr(foy) "Footer Text Left"
    $::pr(obj) drawTextAt [expr {$::pr(w) / 2.0}] $::pr(foy) \
        "Footer Text C" -align center
    $::pr(obj) drawTextAt $::pr(w) $::pr(foy) "Footer Text Right" -align right

    # Normal font
    $::pr(obj) setFont $::pr(fontsize) Courier
}

# Produce one page
proc OnePage {} {

    NewPage

    # Dummy strings
    set longstr  [string repeat "MifjgqIo" 100]
    set strcpl   [string range $longstr 0 [expr {$::pr(cpl) - 1}]]
    set strcpl10 [string range $longstr 0 [expr {$::pr(cpl) + 9}]]

    # Text
    for {set line 0} {$line < $::pr(nlines)} {incr line} {
        if {$line % 2 == 0} {
            $::pr(obj) setBgColor 1.0 0.5 0.5
        } else {
            $::pr(obj) setBgColor 0.5 1.0 0.5
        }
        $::pr(obj) text $line -x $::pr(ox1) -y [expr {$::pr(oy) + $line * $::pr(linesize)}]
        $::pr(obj) text "hejsan" -fill 1
        $::pr(obj) text "hoppsan" -fill "0.5 0.5 1.0"
        $::pr(obj) text "Miffo"
        $::pr(obj) drawTextAt $::pr(ox2) [expr {$::pr(oy) + $line * $::pr(linesize)}] $strcpl -fill 1
    }
}

# Finish a print job
proc EndPrint {} {
    $::pr(obj) write -file exp.pdf
    $::pr(obj) destroy
}

set prev [clock clicks] ; puts "Ping"

#StartPrint
#OnePage
#OnePage
#EndPrint
#set new [clock clicks] ; puts "Ping [expr {$new - $prev}]" ; set prev $new

set print [printexp %AUTO%]
set new [clock clicks] ; puts "Ping [expr {$new - $prev}]" ; set prev $new
$print onePage
set new [clock clicks] ; puts "Ping [expr {$new - $prev}]" ; set prev $new
$print onePage
set new [clock clicks] ; puts "Ping [expr {$new - $prev}]" ; set prev $new
$print endPrint
set new [clock clicks] ; puts "Ping [expr {$new - $prev}]" ; set prev $new
