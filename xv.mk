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

# $Id: xv.mk 1700 2012-06-04 13:59:59Z maarek $

.PHONY: default all clean

default:: xv
all:: objs bin
clean::

config.mk:
	@echo "Create a configuration file 'config.mk'"
	@echo "See the template file 'config.mk.tmpl'"
	@exit 1

-include config.mk

WARNING_OPTIONS = -w +1..30 -warn-error +a
WARNING_DEBUG_OPTIONS = -g -annot -w +a -warn-error +a
OCAMLOPT_WARN := $(OCAMLOPT) $(WARNING_OPTIONS)
OCAMLC_WARN = $(OCAMLC) $(WARNING_DEBUG_OPTIONS)

# Sous-répertoires, en ordre quelconque
dir:= lib
-include $(dir)/Rules.mk
dir:= xml
-include $(dir)/Rules.mk
dir:= re
-include $(dir)/Rules.mk
dir:= canonizer
-include $(dir)/Rules.mk
dir:= xv
-include $(dir)/Rules.mk
dir:= xsd
include $(dir)/Rules.mk
dir:= generator
include $(dir)/Rules.mk

-include Rules.mk

# XML validator

XV_LIBS = \
  str.cmxa \

XV_CMX = \
  lib/error.cmx \
  lib/lib.cmx \
  lib/file_in.cmx \
  lib/file_out.cmx \
  lib/utf8.cmx \
  lib/stringdata.cmx \
  xml/xml.cmx \
  lib/message_debug.cmx \
  xml/xml_val.cmx \
  re/xml_re.cmx \
  xml/xml_ns.cmx \
  xml/xml_lexer.cmx \
  xml/xml_lib.cmx \
  xml/xml_parser.cmx \
  xv/xml_validator.cmx \

XV_BIN=bin/$(XV_NAME).bin

.PHONY: xv
xv: $(XV_BIN)

bin:: xv

$(XV_BIN): $(XV_CMX)
	mkdir -p bin
	$(OCAMLOPT_WARN) $(INCLUDES) -o $@ $(XV_LIBS) $(XV_CMX)

clean::
	rm -f $(XV_BIN)


.PHONY: depend
depend: $(DEPEND)


