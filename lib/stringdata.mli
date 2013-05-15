(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: stringdata.mli 1662 2012-05-23 16:09:36Z maarek $ *)




(* Strings *)
type t

exception E_at of t * Error.t

val of_in_channel : in_channel -> File_in.t -> t
val of_string : string -> t
val to_string : t -> string
val to_int : t -> int option
val to_float : t -> float option
val loc : t -> int
val string_loc : t -> string
val next_utf8 : t -> (Utf8.enc * t) option
val next : t -> (char * t) option option
val nexts : t -> t -> t option
val empty : t -> bool
val sub : t -> t -> t
val empty_sub : t -> t -> bool
val comp : t -> t -> int
val eq : t -> t -> bool
val match_regexp : t -> t -> bool
val match_regexp_from : t -> t -> t option
val byte_length : t -> int
val utf8_length : t -> int
val mark_reference : t -> t -> t
val mark_data : t -> t
val mark_skip : t -> t
val clear_marks : t -> t
val whitespace_replace : t -> t
val whitespace_collapse : t -> t
val url_match : t -> t -> bool
val to_code_escaped_string : t -> string




(* *)
val string_debug : t -> string
