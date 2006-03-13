# Makefile for pdf4tcl
#
# $Id$

test:
	tclsh tests/all.tcl

check:
	nagelfar pdf4tcl.tcl
