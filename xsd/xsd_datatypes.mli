(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd_datatypes.mli 1365 2012-04-03 15:23:30Z maarek $ *)

val range_st_string : Utf8.range
val st_boolean : Xsd.regexp
val st_decimal : Xsd.regexp
val st_float : Xsd.regexp
val st_double : Xsd.regexp
val st_duration : Xsd.regexp
val st_dateTime : Xsd.regexp
val st_time : Xsd.regexp
val st_date : Xsd.regexp
val st_gYearMonth : Xsd.regexp
val st_gYear : Xsd.regexp
val st_gMonthDay : Xsd.regexp
val st_gDay : Xsd.regexp
val st_gMonth : Xsd.regexp
val st_hexBinary : Xsd.regexp
val st_base64Binary : Xsd.regexp
val st_anyURI : Utf8.range
val st_NMTOKEN : Xsd.regexp
val st_Name : Xsd.regexp
val st_NCName : Xsd.regexp
val st_integer : Xsd.regexp
val st_yearMonthDuration : Xsd.regexp
val st_dayTimeDuration : Xsd.regexp
val st_explicit_timestamp : Xsd.regexp
val xml_language : Xsd.regexp
