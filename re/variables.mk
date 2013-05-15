MLI_$(d):= \
  $(d)/xml_re.mli \
  $(d)/val_re.mli \

ML_$(d):=$(MLI_$(d):.mli=.ml)

INCLUDES_$(d):= -I $(d) -I lib -I xml
