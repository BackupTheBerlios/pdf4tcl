# Makefile for pdf4tcl
#
# $Id$

# TOOL paths
NAGELFAR = nagelfar
ESKIL    = eskil

all: links doc

# Export
links: pkg/pkgIndex.tcl pkg/pdf4tcl.tcl pkg/metrics.tcl pkg/glyphnames.tcl

pkg/pkgIndex.tcl:
	ln -fs ../pkgIndex.tcl pkg
pkg/pdf4tcl.tcl:
	ln -fs ../pdf4tcl.tcl pkg
pkg/metrics.tcl:
	ln -fs ../metrics.tcl pkg
pkg/glyphnames.tcl:
	ln -fs ../glyphnames.tcl pkg


# Documentation
doc : pdf4tcl.html pdf4tcl.n

pdf4tcl.html pdf4tcl.n : pdf4tcl.man mkdoc.tcl
	mkdoc.tcl

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
