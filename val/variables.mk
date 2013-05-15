ML_$(d):= \
  $(d)/val_parser.ml \

MLI_$(d):= \
  $(d)/xsval.mli \
  $(ML_$(d):.ml=.mli)


INCLUDES_$(d):= -I $(d) -I lib -I xml -I re -I xsd
