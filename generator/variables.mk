MLI_$(d):= $(d)/generator.mli
ML_$(d):=$(MLI_$(d):.mli=.ml)

INCLUDES_$(d):= -I $(d) -I lib -I xml -I xsd
