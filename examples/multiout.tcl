#This file must be sourced in cp1251 encoding.
package require pdf4tcl

proc multipdf {args} {
global G_pdfobjs
foreach obj $G_pdfobjs {
    $obj {*}$args
    }
}

proc testmulti {} {
global G_pdfobjs
pdf4tcl::LoadBaseTrueTypeFont BaseArial "arial.ttf"
pdf4tcl::CreateFont BaseArial MyArial cp1251 
pdf4tcl::LoadBaseType1Font BaseType1 "a010013l.afm" "a010013l.pfb"
pdf4tcl::CreateFont BaseType1 MyType1 cp1251 
set G_pdfobjs [list one two three]

foreach obj $G_pdfobjs {
    pdf4tcl::new $obj -paper a4 -compress 1
    }

multipdf startPage
multipdf setFont 20 MyArial
multipdf text "יצףךוםדרשחץתפûגאןנמכהז‎ÿקסלטעüב‏" -x 50 -y 50 -bg #CACACA
multipdf text "ÉÖÓÊÅÍÃØÙÇÕÚÔÛÂÀÏÐÎËÄÆÝ‗×ÑÌÈÒÜÁÞ" -x 50 -y 100 -bg #CACACA

multipdf setFont 20 MyType1
multipdf text "יצףךוםדרשחץתפûגאןנמכהז‎ÿקסלטעüב‏" -x 50 -y 150 -bg #CACACA
multipdf text "ÉÖÓÊÅÍÃØÙÇÕÚÔÛÂÀÏÐÎËÄÆÝ‗×ÑÌÈÒÜÁÞ" -x 50 -y 200 -bg #CACACA

multipdf setFillColor #6A6A6A
multipdf setFont 16 Courier
multipdf text "This is text for testing purposes." -bg #8A8A8A -x 100 -y 370

multipdf setFont 20 MyType1
multipdf setFillColor #20FA20
multipdf text "Skewed. Òוךסע ןמה ףדכמל הכÿ ןנמגונךט." -bg #000000 -x 200 -y 420 -xangle 10 -yangle 15 -angle 25

foreach obj $G_pdfobjs {
    $obj write -file $obj.pdf 
    $obj destroy
    }
}

testmulti

