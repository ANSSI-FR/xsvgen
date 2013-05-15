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

# $Id: subRules.mk 1342 2012-03-27 09:22:06Z maarek $

# Prendre en compte le fait qu'on est dans un sous-répertoire
sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

# Sous-répertoires, s'il y en a, en ordre quelconque
# dir:= $(d)/foo
#include $(dir)/Rules.mk

include $(d)/depend.mk

# Variables locales

include $(d)/variables.mk

CMI_$(d):= $(MLI_$(d):.mli=.cmi)
CMO_$(d):= $(ML_$(d):.ml=.cmo)
ANNOT_$(d):= $(ML_$(d):.ml=.annot)
CMX_$(d):= $(ML_$(d):.ml=.cmx)
O_$(d):= $(ML_$(d):.ml=.o)

CLEAN:= $(CLEAN) $(CMI_$(d)) $(CMO_$(d)) $(CMX_$(d)) $(O_$(d)) $(ANNOT_$(d)) $(d)/depend.mk $(d)/$(d).cmdoc
DEPEND:=$(DEPEND) $(d)/depend.mk
# Règles locales

$(d)/depend.mk: $(MLI_$(d)) $(ML_$(d))

$(CMI_$(d)): INCLUDES:= $(INCLUDES_$(d))
$(CMO_$(d)): INCLUDES:= $(INCLUDES_$(d))
$(CMX_$(d)): INCLUDES:= $(INCLUDES_$(d))
$(d)/depend.mk: INCLUDES:= $(INCLUDES_$(d))

$(d)/$(d).cmdoc: INCLUDES:= $(INCLUDES_$(d))
$(d)/$(d).cmdoc: DOC_SOURCE_FILES:= $(MLI_$(d)) $(ML_$(d))
$(d)/$(d).cmdoc: $(MLI_$(d)) $(ML_$(d))

# Ajout des fichiers à la liste globale
MLI_FILES:= $(MLI_FILES) $(MLI_$(d))
ML_FILES:= $(ML_FILES) $(ML_$(d))
CMI_FILES:= $(CMI_FILES) $(CMI_$(d))
CMO_FILES:= $(CMO_FILES) $(CMO_$(d))
ANNOT_FILES:= $(ANNOT_FILES) $(ANNOT_$(d))
CMX_FILES:= $(CMX_FILES) $(CMX_$(d))
O_FILES:= $(O_FILES) $(O_$(d))
CMDOC_FILES:= $(CMDOC_FILES) $(d)/$(d).cmdoc
CMDOC_LOADS:= $(CMDOC_LOADS) -load $(d)/$(d).cmdoc

# Et on remonte
d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
