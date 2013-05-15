MLI_$(d):= \
  $(d)/meta_lib.mli \

ML_$(d):= \
  $(MLI_$(d):.mli=.ml) \
  $(d)/meta_xml_re.ml \
  $(d)/meta_val_re.ml \


INCLUDES_$(d):= -I $(d) -I lib -I xml -I xsd
