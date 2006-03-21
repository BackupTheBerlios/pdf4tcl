# Makefile for pdf4tcl
#
# $Id$

NAGELFAR = nagelfar
ESKIL    = eskil

# Documentation
doc : pdf4tcl.html pdf4tcl.n

pdf4tcl.html pdf4tcl.n : pdf4tcl.man mkdoc.tcl
	mkdoc.tcl

# Tests
test: cleancc
	tclsh tests/all.tcl

check:
	nagelfar pdf4tcl.tcl -filter '*Unknown variable*'

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
	@tclsh tests/all.tcl

%.tcl_m: %.tcl_log 
	@$(NAGELFAR) -markup $*.tcl

icheck: $(MFILES)
	@for i in $(SRCFILES) ; do $(ESKIL) -noparse $$i $${i}_m & done

cleancc:
	@rm -f $(LOGFILES) $(IFILES) $(MFILES)
