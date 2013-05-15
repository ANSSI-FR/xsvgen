MLI_$(d):= \
  $(d)/xml_validator.mli \

ML_$(d):=$(MLI_$(d):.mli=.ml)

INCLUDES_$(d):= -I $(d) -I lib -I xml
