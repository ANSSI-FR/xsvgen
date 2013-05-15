MLI_$(d):= $(d)/validator.mli
ML_$(d):=$(MLI_$(d):.mli=.ml)

INCLUDES_$(d):= -I $(d) -I lib -I xml -I val -I xsd
