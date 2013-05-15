ML_$(d):= \
  $(d)/xsd_val.ml \
  $(d)/xsd_datatypes.ml \
  $(d)/xsd_lib.ml \
  $(d)/xsd_transducer.ml \
  $(d)/xsd_lineariser.ml \
  $(d)/code_generator.ml \

MLI_$(d):= \
  $(d)/xsd.mli \
  $(d)/lxsd.mli \
  $(ML_$(d):.ml=.mli)


INCLUDES_$(d):= -I $(d) -I lib -I xml -I re
