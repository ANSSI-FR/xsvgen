ML_$(d):=\
 $(d)/lib.ml \
 $(d)/error.ml \
 $(d)/file_in.ml \
 $(d)/file_out.ml \
 $(d)/stringdata.ml \
 $(d)/utf8.ml \

MLI_$(d):= \
  $(d)/message.mli \
  $(ML_$(d):.ml=.mli)

INCLUDES_$(d):=-I $(d) -I xsd -I xml
