MLI_$(d):= \
  $(d)/xml.mli \
  $(d)/xml_lib.mli \
  $(d)/xml_val.mli \
  $(d)/xml_charsets.mli \
  $(d)/xml_ns.mli \
  $(d)/xml_lexer.mli \
  $(d)/xml_parser.mli \

ML_$(d):=$(MLI_$(d):.mli=.ml)

INCLUDES_$(d):= -I $(d) -I lib -I re
