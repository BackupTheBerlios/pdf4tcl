lappend auto_path pkg
package require pdf4tcl

interp alias {} method {} snit::method ::pdf4tcl::pdf4tcl
source contrib/table.tcl

#
# Create a simple table
#
pdf4tcl::new mypdf

mypdf startPage
mypdf setFont 12 Helvetica-BoldOblique
mypdf setTextPosition 100 100

set str "<TR><TD>1</TD><TD><font size=20 color=#ff0000>2</TD><TD>3</TD></TR><TR><TD><font color=#553399>x</TD><TD>y</TD></TR>"
set str [mypdf html2table $str]
mypdf create_pdf_table $str


mypdf write -file mp.pdf
mypdf destroy
