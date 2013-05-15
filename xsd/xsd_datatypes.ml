(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd_datatypes.ml 1453 2012-04-24 15:55:52Z maarek $ *)

(** {1 XSD datatypes library} *)

(** {b String} primitive simple type *)
(* #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF] *)
let range_st_string =
  Utf8.union
    [ Utf8.range_of_int 0x9;
      Utf8.range_of_int 0xA;
      Utf8.range_of_int 0xD;
      Utf8.range_of_ints 0x20 0xD7FF;
      Utf8.range_of_ints 0xE000 0xFFFD;
      Utf8.range_of_ints 0x10000 0x10FFFF ]

(** {b boolean} primitive simple type *)
(* true|false|1|0 *)
let st_boolean =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "true|false|1|0")

(** {b decimal} primitive simple type *)
let st_decimal =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "(\\+|-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)")

(** {b float} primitive simple type *)
let st_float =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "(\\+|-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|-)?[0-9]+)?|(\\+|-)?INF|NaN")

(** {b double} primitive simple type *)
let st_double =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "(\\+|-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|-)?[0-9]+)?|(\\+|-)?INF|NaN")

(** {b duration} primitive simple type *)
let st_duration =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "-?P((([0-9]+Y([0-9]+M)?([0-9]+D)?|([0-9]+M)([0-9]+D)?|([0-9]+D))(T(([0-9]+H)([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?|([0-9]+M)([0-9]+(\\.[0-9]+)?S)?|([0-9]+(\\.[0-9]+)?S)))?)|(T(([0-9]+H)([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?|([0-9]+M)([0-9]+(\\.[0-9]+)?S)?|([0-9]+(\\.[0-9]+)?S))))")

(** {b dateTime} primitive simple type *)
let st_dateTime =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "-?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])T(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|(24:00:00(\\.0+)?))(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")

(** {b time} primitive simple type *)
let st_time =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|(24:00:00(\\.0+)?))(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")

(** {b date} primitive simple type *)
let st_date =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "-?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")

(** {b gYearMonth} primitive simple type *)
let st_gYearMonth =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "-?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")

(** {b gYear} primitive simple type *)
let st_gYear =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "-?([1-9][0-9]{3,}|0[0-9]{3})(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")

(** {b gMonthDay} primitive simple type *)
let st_gMonthDay =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "--(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")

(** {b gDay} primitive simple type *)
let st_gDay =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "---(0[1-9]|[12][0-9]|3[01])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")

(** {b gMonth} primitive simple type *)
let st_gMonth =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "--(0[1-9]|1[0-2])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")

(** {b hexBinary} primitive simple type *)
let st_hexBinary =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "([0-9a-fA-F]{2})*")

(** {b base64Binary} primitive simple type *)
let st_base64Binary =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "((([A-Za-z0-9+/] ?){4})*(([A-Za-z0-9+/] ?){3}[A-Za-z0-9+/]|([A-Za-z0-9+/] ?){2}[AEIMQUYcgkosw048] ?=|[A-Za-z0-9+/] ?[AQgw] ?= ?=))?")

(** {b anyURI} primitive simple type *)
let st_anyURI =
  Utf8.sub
    range_st_string
    Xml_charsets.range_S

(** {b NMTOKEN} non primitive simple type *)
let st_NMTOKEN =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "\\c+")

(** {b Name} non primitive simple type *)
let st_Name =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "\\i\\c*")


(** {b NCName} non primitive simple type *)
let st_NCName =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "[\\i-[:]][\\c-[:]]*")


(** {b integer} non primitive simple type *)
let st_integer =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "[\\-+]?[0-9]+")


(** {b yearMonthDuration} non primitive simple type *)
let st_yearMonthDuration =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "[^DT]*")

(** {b dayTimeDuration} non primitive simple type *)
let st_dayTimeDuration =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "[^YM]*(T.*)?")

(** Extra range for explicit timestamp *)
let st_explicit_timestamp =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       ".*(Z|(\\+|-)[0-9][0-9]:[0-9][0-9])")

(** XML's values for attribute {b xml:lang}, as defined in the XSD non
    primitive type {b language}. *)
let xml_language =
  Xsd_lib.read_pattern
    (Stringdata.of_string
       "[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*")
