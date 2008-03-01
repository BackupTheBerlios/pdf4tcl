# Makefile for pdf4tcl
#
# $Id$

VERSION = 03
#TCLSH = $(HOME)/tcl/bin/tclsh8.5
#TCLSH = $(HOME)/tcl/install/bin/tclsh8.5
TCLSH = tclsh

# TOOL paths
NAGELFAR = nagelfar -encoding iso8859-1
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
	$(TCLSH) tests/all.tcl

check:
	$(NAGELFAR) pdf4tcl.tcl -filter '*Unknown variable*'

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
#----------------------------------------------------------------
# Packaging/Releasing
#----------------------------------------------------------------

release: doc
	@\rm -f pdf4tcl.tar.gz
	@tar -zcvf pdf4tcl.tar.gz pdf4tcl.tcl pkgIndex.tcl metrics.tcl \
		glyphnames.tcl pdf4tcl.man pdf4tcl.html licence.terms
	@cp pdf4tcl.tar.gz pdf4tcl`date +%Y%m%d`.tar.gz
	@mv pdf4tcl.tar.gz pdf4tcl$(VERSION).tar.gz
