(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: meta_val_re.ml 1365 2012-04-03 15:23:30Z maarek $ *)

let (_ : unit) =
  Meta_lib.print_header "val_re"

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_string"
    (Meta_lib.xsd_re_of_range Xsd_datatypes.range_st_string)

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_boolean"
    Xsd_datatypes.st_boolean

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_decimal"
    Xsd_datatypes.st_decimal

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_float"
    Xsd_datatypes.st_float


let (_ : unit) =
  Meta_lib.print_test_charset
    "st_double"
    Xsd_datatypes.st_double


let (_ : unit) =
  Meta_lib.print_test_charset
    "st_duration"
    Xsd_datatypes.st_duration

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_dateTime"
    Xsd_datatypes.st_dateTime

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_time"
    Xsd_datatypes.st_time

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_date"
    Xsd_datatypes.st_date

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_gYearMonth"
    Xsd_datatypes.st_gYearMonth

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_gYear"
    Xsd_datatypes.st_gYear

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_gMonthDay"
    Xsd_datatypes.st_gMonthDay

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_gDay"
    Xsd_datatypes.st_gDay

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_gMonth"
    Xsd_datatypes.st_gMonth

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_hexBinary"
    Xsd_datatypes.st_hexBinary

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_base64Binary"
    Xsd_datatypes.st_base64Binary


let (_ : unit) =
  Meta_lib.print_test_charset
    "st_anyURI"
    (Meta_lib.xsd_re_of_range Xsd_datatypes.st_anyURI)


let (_ : unit) =
  Meta_lib.print_test_charset
    "st_NMTOKEN"
    Xsd_datatypes.st_NMTOKEN

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_Name"
    Xsd_datatypes.st_Name

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_NCName"
    Xsd_datatypes.st_NCName


let (_ : unit) =
  Meta_lib.print_test_charset
    "st_integer"
    Xsd_datatypes.st_integer

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_yearMonthDuration"
    Xsd_datatypes.st_yearMonthDuration

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_dayTimeDuration"
    Xsd_datatypes.st_dayTimeDuration

let (_ : unit) =
  Meta_lib.print_test_charset
    "st_explicit_timestamp"
    Xsd_datatypes.st_explicit_timestamp





