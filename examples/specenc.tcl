package require pdf4tcl

proc example_specenc {} {
pdf4tcl::LoadBaseTrueTypeFont BaseArial "arial.ttf"
#Subset is a list of unicodes:
for {set f 0} {$f<128} {incr f} {lappend subset $f}
lappend subset 178 946

pdf4tcl::CreateFont_SpecEnc BaseArial MyArial $subset 
pdf4tcl::new mypdf -paper a4 -compress 1
mypdf startPage
mypdf setFont 16 MyArial
set txt "sin\u00B2\u03B2 + cos\u00B2\u03B2 = 1"
mypdf text $txt -x 50 -y 100
mypdf write -file font_spec.pdf
mypdf destroy
}

example_specenc

