# Makefile for pdf4tcl
#
# $Id$

# TOOL paths
NAGELFAR = nagelfar
ESKIL    = eskil

all: doc

# Documentation
doc : pdf4tcl.html pdf4tcl.n

pdf4tcl.html pdf4tcl.n : pdf4tcl.man mkdoc.tcl
	mkdoc.tcl

checkdoc:
	@egrep 'method [a-z]' pdf4tcl.man | grep '\[call' | egrep -v 'method configure' | sed 's/[][]/ /g' | sort > docmeth
	@egrep 'method [a-z]' pdf4tcl.tcl | sort > srcmeth
	@eskil -block srcmeth docmeth

# Helpers

metrics:
	tclsh tools/extract-metrics.tcl metrics.tcl

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
