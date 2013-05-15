#########################################################################
#                                                                       #
#                   XML Schema Validator Generator                      #
#                                                                       #
#            Samuel Colin (SafeRiver)                                   #
#            Manuel Maarek (SafeRiver)                                  #
#                                                                       #
#   Copyright 2012, ANSSI and SafeRiver.                                #
#                                                                       #
#########################################################################

# $Id: Makefile 1691 2012-05-31 09:47:08Z maarek $

.PHONY: clean
default:: bin objs_cmx
all:: objs doc bin
clean::

config.mk:
	@echo "Create a configuration file 'config.mk'"
	@echo "See the template file 'config.mk.tmpl'"
	@exit 1

-include config.mk

WARNING_OPTIONS = -w +a -warn-error +a -strict-sequence -noautolink -cclib -lcamlstr
WARNING_ANNOT_OPTIONS = -annot -w +a -warn-error +a -strict-sequence -noautolink -cclib -lcamlstr
OCAMLOPT_WARN := $(OCAMLOPT) $(WARNING_OPTIONS)
OCAMLC_WARN = $(OCAMLC) $(WARNING_ANNOT_OPTIONS)

# Sous-répertoires, en ordre quelconque
dir:= lib
-include $(dir)/Rules.mk
dir:= xml
-include $(dir)/Rules.mk
dir:= xsd
-include $(dir)/Rules.mk
dir:= re
-include $(dir)/Rules.mk
dir:= generator
-include $(dir)/Rules.mk
dir:= val
-include $(dir)/Rules.mk
dir:= validator
-include $(dir)/Rules.mk

-include Rules.mk

GENERATOR_LIBS = \
  str.cmxa \

GENERATOR_FILES = \
  lib/error.mli \
  lib/error.ml \
  xsd/xsd.mli \
  xsd/lxsd.mli \
  lib/lib.mli \
  lib/lib.ml \
  lib/utf8.mli \
  lib/utf8.ml \
  lib/file_in.mli \
  lib/file_in.ml \
  lib/file_out.mli \
  lib/file_out.ml \
  lib/stringdata.mli \
  lib/stringdata.ml \
  xml/xml.mli \
  xml/xml.ml \
  lib/message.mli \
  lib/message_debug.ml \
  xml/xml_val.mli \
  xml/xml_val.ml \
  xml/xml_charsets.mli \
  xml/xml_charsets.ml \
  re/xml_re.mli \
  re/xml_re.ml \
  xml/xml_ns.mli \
  xml/xml_ns.ml \
  xml/xml_lexer.mli \
  xml/xml_lexer.ml \
  xml/xml_lib.mli \
  xml/xml_lib.ml \
  xml/xml_parser.mli \
  xml/xml_parser.ml \
  xsd/xsd_val.mli \
  xsd/xsd_val.ml \
  re/val_re.mli \
  re/val_re.ml \
  xsd/xsd_lib.mli \
  xsd/xsd_lib.ml \
  xsd/xsd_lineariser.mli \
  xsd/xsd_lineariser.ml \
  xsd/xsd_transducer.mli \
  xsd/xsd_transducer.ml \
  xsd/code_generator.mli \
  xsd/code_generator.ml \
  generator/generator.mli \
  generator/generator.ml \

GENERATOR_ML_FILES = $(patsubst %.mli,,$(GENERATOR_FILES))
GENERATOR_CMX = $(GENERATOR_ML_FILES:.ml=.cmx)

GENERATOR_BIN=bin/$(GENERATOR_NAME).bin
GENERATOR_LIB_DIR=bin/$(GENERATOR_NAME)_lib

VALIDATOR_COMMON_FILES = \
  xml/xml.mli \
  xml/xml.ml \
  lib/error.mli \
  lib/error.ml \
  xsd/lxsd.mli \
  lib/lib.mli \
  lib/lib.ml \
  lib/utf8.mli \
  lib/utf8.ml \
  lib/file_in.mli \
  lib/file_in.ml \
  lib/stringdata.mli \
  lib/stringdata.ml \
  lib/message.mli \
  lib/message_debug.ml \
  lib/message_silent.ml \
  xml/xml_val.mli \
  xml/xml_val.ml \
  re/xml_re.mli \
  re/xml_re.ml \
  xml/xml_ns.mli \
  xml/xml_ns.ml \
  xml/xml_lexer.mli \
  xml/xml_lexer.ml \
  xml/xml_parser.mli \
  xml/xml_parser.ml \
  xml/xml_lib.mli \
  xml/xml_lib.ml \
  re/val_re.mli \
  re/val_re.ml \
  val/val_parser.mli \
  val/val_parser.ml \
  val/xsval.mli \
  validator/validator.mli \
  validator/validator.ml \

bin: $(GENERATOR_BIN)
$(GENERATOR_BIN): $(GENERATOR_CMX) $(CMX_FILES)
	mkdir -p bin
	mkdir -p $(GENERATOR_LIB_DIR)
	$(OCAMLOPT_WARN) $(INCLUDES) -o $@ $(GENERATOR_LIBS) $(GENERATOR_CMX)
	cp $(VALIDATOR_COMMON_FILES) $(GENERATOR_LIB_DIR)

clean::
	rm -f $(GENERATOR_BIN)
	rm -fr $(GENERATOR_LIB_DIR)

.PHONY: doc
doc: objs_cmdoc \
  doc/dot/modules.pdf \
  doc/dot/modules-reduced.pdf \
  doc/dot/types.pdf \
  doc/dot/types-reduced.pdf \
  doc/dot/modules-gen-reduced.pdf \
  doc/dot/modules-val-reduced.pdf \
  doc/html/index.html \
  doc/pdf/doc.pdf \


doc/html/index.html: $(CMDOC_FILES)
	mkdir -p doc/html
	$(OCAMLDOC) -html -sort -t "XML Schema Validator Generator" -d doc/html $(CMDOC_LOADS)

doc/pdf/doc.tex :  $(CMDOC_FILES)
	mkdir -p doc/pdf
	$(OCAMLDOC) -latex -sort -t "XML Schema Validator Generator" -o $@ $(CMDOC_LOADS)


doc/pdf/doc.pdf : doc/pdf/doc.tex
ifeq ($(strip $(RUBBER)),)
	cd doc/pdf && $(PDFLATEX) doc.tex && $(PDFLATEX) doc.tex && $(PDFLATEX) doc.tex
else
	cd doc/pdf && $(RUBBER) --pdf doc.tex
endif

doc/dot/modules.dot: $(CMDOC_FILES) ranks.dot
	mkdir -p doc/dot
	$(OCAMLDOC) -dot -dot-include-all -o $@ $(CMDOC_LOADS)
	sed -e "/rankdir = TB ;/r ranks.dot" $@

doc/dot/modules-reduced.dot: $(CMDOC_FILES) ranks.dot
	mkdir -p doc/dot
	$(OCAMLDOC) -dot -dot-include-all -dot-reduce -o $@ $(CMDOC_LOADS)
	sed -e "/rankdir = TB ;/r ranks.dot" $@

doc/dot/types.dot: $(CMDOC_FILES) ranks.dot
	mkdir -p doc/dot
	$(OCAMLDOC) -dot -dot-include-all -dot-types -o $@ $(CMDOC_LOADS)
	sed -e "/rankdir = TB ;/r ranks.dot" $@

doc/dot/types-reduced.dot: $(CMDOC_FILES) ranks.dot
	mkdir -p doc/dot
	$(OCAMLDOC) -dot -dot-include-all -dot-reduce -dot-types -o $@ $(CMDOC_LOADS)
	sed -e "/rankdir = TB ;/r ranks.dot" $@

doc/dot/modules-gen-reduced.dot: $(GENERATOR_FILES) objs_cmo ranks-gen.dot
	mkdir -p doc/dot
	$(OCAMLDOC) -I lib -I xml -I xsd -I re -I generator -no-stop -inv-merge-ml-mli -dot -dot-include-all -dot-reduce -o $@ $(GENERATOR_FILES)
	sed -e "/rankdir = TB ;/r ranks-gen.dot" $@

doc/dot/modules-val-reduced.dot: $(VALIDATOR_COMMON_FILES) objs_cmo ranks-val.dot
	mkdir -p doc/dot
	$(OCAMLDOC) -I lib -I xml -I val -I xsd -I re -I validator -no-stop -inv-merge-ml-mli -dot -dot-include-all -dot-reduce -o $@ $(VALIDATOR_COMMON_FILES)
	sed -e "/rankdir = TB ;/r ranks-val.dot" $@


.PHONY: depend
depend: $(DEPEND)


