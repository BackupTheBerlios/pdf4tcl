# Makefile for pdf4tcl

VERSION = 08p

# TOOL paths
TCLSH = tclsh
NAGELFAR = nagelfar -encoding iso8859-1
ESKIL    = eskil

all: doc

# Documentation
doc : pdf4tcl.html pdf4tcl.n web/mypdf.pdf

pdf4tcl.html pdf4tcl.n : pdf4tcl.man mkdoc.tcl
	./mkdoc.tcl

checkdoc:
	@egrep 'method [a-z]' pdf4tcl.man | grep '\[call' | egrep -v 'method configure' | sed 's/[]["]/ /g' | sed 's/ arg / /g' | sed 's/  */ /g' | sed 's/call objectName/ /g' | sort > _docmeth
	@egrep 'method [a-z]' pdf4tcl.tcl | sed 's/[{}]/ /g' | sed 's/  */ /g' | sort > _srcmeth
	@$(ESKIL) -block _srcmeth _docmeth
	@rm _srcmeth _docmeth

web/mypdf.pdf: mkweb.tcl web/index.html
	./mkweb.tcl

# Helpers

#metrics:
#	tclsh tools/extract-metrics.tcl metrics.tcl

# Tests
test: cleancc
	$(TCLSH) tests/all.tcl

pdf4tcl_h.syntax : pdf4tcl.tcl pdf4tcl.syntax
	$(NAGELFAR) -header pdf4tcl_h.syntax pdf4tcl.tcl -filter '*Unknown variable*'

check: pdf4tcl_h.syntax
	$(NAGELFAR) pdf4tcl_h.syntax pdf4tcl.tcl -filter '*Unknown variable*'

bench: cleancc
	$(TCLSH) bench/all.tcl

# Code coverage tests
SRCFILES = pdf4tcl.tcl
IFILES   = $(SRCFILES:.tcl=.tcl_i)
LOGFILES = $(SRCFILES:.tcl=.tcl_log)
MFILES   = $(SRCFILES:.tcl=.tcl_m)

%.tcl_i: %.tcl
	@$(NAGELFAR) -instrument $<

instrument: $(IFILES)
	@rm -f $(LOGFILES)

$(LOGFILES): $(IFILES)
	@$(TCLSH) tests/all.tcl

%.tcl_m: %.tcl_log 
	@$(NAGELFAR) -markup $*.tcl

icheck: $(MFILES)
	@for i in $(SRCFILES) ; do $(ESKIL) -noparse $$i $${i}_m & done

cleancc:
	@rm -f $(LOGFILES) $(IFILES) $(MFILES)
#----------------------------------------------------------------
# Packaging/Releasing
#----------------------------------------------------------------

release: doc
	@\rm -f pdf4tcl.tar.gz
	@ln -s pkg pdf4tcl$(VERSION)
	@tar -zcvhf pdf4tcl.tar.gz --exclude=.svn pdf4tcl$(VERSION)
	@\rm -f pdf4tcl$(VERSION)
	@cp pdf4tcl.tar.gz pdf4tcl`date +%Y%m%d`.tar.gz
	@mv pdf4tcl.tar.gz pdf4tcl$(VERSION).tar.gz
