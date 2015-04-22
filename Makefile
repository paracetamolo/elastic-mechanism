# Copyright (C) 2014, Marco Stronati, All Rights Reserved.
# This file is distributed under the terms of the
# GNU General Public License version 3 or later.

NAME:=elastic
TARGETS:=evaluation_paris.ml evaluation_predictive.ml evaluation_gowalla.ml
SOURCES:=$(wildcard *.ml)
BIN_NATIVE:=$(patsubst %.ml, %.native, $(TARGETS))
BIN_DEBUG:=$(patsubst %.ml, %.d.byte, $(TARGETS))
BIN_PROFILE:=$(patsubst %.ml, %.p.native, $(TARGETS))

OCAMLBUILD := ocamlbuild -use-ocamlfind -classic-display

all: native

native:
	$(OCAMLBUILD) $(BIN_NATIVE)

debug:
	$(OCAMLBUILD) $(BIN_DEBUG)

profile:
	$(OCAMLBUILD) $(BIN_PROFILE)

doc:
	$(OCAMLBUILD) elastic.docdir/index.html

clean:
	$(OCAMLBUILD) -clean
