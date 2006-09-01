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
    option -headleft -default "Header Text Left"
    option -headright -default "Header Text Right"
    option -headnpages -default "10"
    option -file     -default exp.pdf

    variable width
    variable height
    variable hoy
    variable fontsize
    variable linesize
    variable nlines
    variable ox1
    variable ox2
    variable oy
    variable page

    constructor {args} {
        set tmp(-file) $options(-file)
        catch {array set tmp $args}
        install pdf using pdf4tcl::pdf4tcl %AUTO% \
                -landscape 1 -paper a4 -margin 15mm -file $tmp(-file)
        $self configurelist $args
        $self StartPrint
    }
    destructor {
        catch {$pdf destroy}
    }

    method StartPrint {} {
        # Page size
        foreach {width height} [$pdf getDrawableArea] break

        # Header metrics
        $pdf setFont $options(-headsize) Courier
        set headoffset [expr {$options(-headsize) + [$pdf getFontMetric bboxy]}]
        set hoy $headoffset

        # Figure out font size from number of chars per line
        set charwidthHead [$pdf getCharWidth "0"]
        set charwidth [expr {$width / 2.0 / ($options(-cpl) + $options(-cpln) + 1)}]
        set fontsize [expr {$options(-headsize) * $charwidth / $charwidthHead}]
        $pdf setFont $fontsize

        # Text metrics
        set linesize  $fontsize
        set offset    [expr {$fontsize + [$pdf getFontMetric bboxy]}]
        set charwidth [$pdf getCharWidth "0"]
        set nlinesf [expr {($height - $options(-headsize)) / $linesize}]
        # Number of lines per page
        set nlines  [expr {int($nlinesf - 1.0)}]
        #set nlines 66
        # Offsets to starting points in both subpages.
        set ox1 $charwidth
        set ox2 [expr {$width / 2.0 + $charwidth}]
        set oy  [expr {($nlinesf - $nlines) / 2.0 * $linesize + \
                                     $offset + $options(-headsize)}]

        # Reset current page
        set page 0
    }
    method getNLines {} {
        return $nlines
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
                $width [expr {$height - $options(-headsize)}]
        # Center line
        $pdf line [expr {$width / 2.0}] $options(-headsize) \
                [expr {$width / 2.0}] $height

        # Header
        $pdf setFont $options(-headsize) Courier
        $pdf drawTextAt 0 $hoy $options(-headleft)
        $pdf drawTextAt [expr {$width / 2.0}] $hoy \
                "Page $page of $options(-headnpages)" -align center
        $pdf drawTextAt $width $hoy $options(-headright) -align right

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
            #$pdf drawTextAt $ox2 [expr {$oy + $line * $linesize}] $strcpl -fill 1
            $pdf text $strcpl -x $ox2 -y [expr {$oy + $line * $linesize}] -fill 1
        }
    }

    # Finish a print job
    method endPrint {} {
        $pdf finish
        $pdf destroy
        $self destroy
    }
}

    set prev [clock clicks] ; puts "Ping"
set print [printexp %AUTO% -cpl 150]
set nlines [$print getNLines]
#puts $nlines
$print configure -headnpages [expr {1000 / $nlines}]
    set new [clock clicks] ; puts "Ping [expr {$new - $prev}]" ; set prev $new
$print onePage
    set new [clock clicks] ; puts "Ping [expr {$new - $prev}]" ; set prev $new
$print onePage
    set new [clock clicks] ; puts "Ping [expr {$new - $prev}]" ; set prev $new
$print endPrint
    set new [clock clicks] ; puts "Ping [expr {$new - $prev}]" ; set prev $new


# Another experiment
pdf4tcl::new apa -file experiment.pdf
apa startPage
apa setTextPosition 10 10
for {set t 70} {$t < 90} {incr t} {
    set size [expr {$t / 10.0}]
    apa setFont $size Courier
    set txt "Courier $size"
    set w [apa getCharWidth $txt] 
    apa drawText "$txt is $w wide"
}
apa destroy
