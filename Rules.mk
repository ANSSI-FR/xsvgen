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

# $Id: Rules.mk 1691 2012-05-31 09:47:08Z maarek $

.SUFFIXES:
.SUFFIXES: .mli .ml .cmi .cmo .cmx .cmdoc

# Règles générales

%.cmo: %.ml
	$(OCAMLC_WARN) $(INCLUDES) -c $<

%.cmi: %.mli
	$(OCAMLOPT_WARN) $(INCLUDES) -c $<

%.cmx : %.ml
	$(OCAMLOPT_WARN) $(INCLUDES) -c $<

%.cmdoc :
	$(OCAMLDOC) -no-stop -inv-merge-ml-mli $(INCLUDES) $(DOC_SOURCE_FILES) -dump $@


%depend.mk: | config.mk
	$(OCAMLDEP) $(INCLUDES) $(MLI_$(@D)) $(ML_$(@D)) > $@
	echo "$(@D)/$(@D).cmdoc: $(CMI_$(@D))" >> $@

lib/message.cmx: lib/message_silent.cmx lib/message_debug.cmx
CMX_FILES:= $(CMX_FILES) lib/message_silent.cmx lib/message_debug.cmx

lib/message_silent.cmx: lib/stringdata.cmx lib/file_in.cmx lib/file_out.cmx
lib/message_silent.cmx: lib/message.cmi lib/message_silent.ml
	cd lib && ln -f -s message_silent.ml message.ml
	$(OCAMLOPT_WARN) $(INCLUDES_lib) -c lib/message.ml
	mv lib/message.cmx lib/message_silent.cmx
	mv lib/message.o lib/message_silent.o
	rm lib/message.ml

lib/message_debug.cmx: lib/message_silent.cmx # to avoid parallel clash
lib/message_debug.cmx: lib/stringdata.cmx lib/file_in.cmx lib/file_out.cmx
lib/message_debug.cmx: lib/message.cmi lib/message_debug.ml
	cd lib && ln -f -s message_debug.ml message.ml
	$(OCAMLOPT_WARN) $(INCLUDES_lib) -c lib/message.ml
	mv lib/message.cmx lib/message_debug.cmx
	mv lib/message.o lib/message_debug.o
	rm lib/message.ml

lib/message.cmo: lib/message_silent.cmo lib/message_debug.cmo
CMO_FILES:= $(CMO_FILES) lib/message_silent.cmo lib/message_debug.cmo

lib/message_silent.cmo: lib/stringdata.cmo lib/file_in.cmo lib/file_out.cmo
lib/message_silent.cmo: lib/message.cmi lib/message_silent.ml
	cd lib && ln -f -s message_silent.ml message.ml
	$(OCAMLC_WARN) $(INCLUDES_lib) -c lib/message.ml
	mv lib/message.cmo lib/message_silent.cmo
	mv lib/message.annot lib/message_silent.annot
	rm lib/message.ml

lib/message_debug.cmo: lib/message_silent.cmo # to avoid parallel clash
lib/message_debug.cmo: lib/stringdata.cmo lib/file_in.cmo lib/file_out.cmo
lib/message_debug.cmo: lib/message.cmi lib/message_debug.ml
	cd lib && ln -f -s message_debug.ml message.ml
	$(OCAMLC_WARN) $(INCLUDES_lib) -c lib/message.ml
	mv lib/message.cmo lib/message_debug.cmo
	mv lib/message.annot lib/message_debug.annot
	rm lib/message.ml

CLEAN:= $(CLEAN) lib/message_silent.cmi lib/message_silent.cmo lib/message_silent.cmx lib/message_silent.o lib/message_silent.annot lib/message_debug.cmi lib/message_debug.cmo lib/message_debug.cmx lib/message_debug.o lib/message_debug.annot

# Directives

.PHONY: objs objs_cmi objs_cmo objs_cmx objs_cmdoc
objs: objs_cmi objs_cmo objs_cmx
objs_cmi: $(CMI_FILES)
objs_cmo: objs_cmi $(CMO_FILES)
objs_cmx: objs_cmi $(CMX_FILES)
objs_cmdoc: $(CMDOC_FILES)



# Règles pour la documentation


%.pdf: %.dot
	dot -Tpdf $< > $@

clean::
	rm -f $(CLEAN)

.SECONDARY: $(CLEAN)
