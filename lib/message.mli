(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: message.mli 1665 2012-05-24 09:47:08Z maarek $ *)

(** {1 Message handling library}

This module comes in two versions:
 - a debug version which prints the messages,
 - a silent version which does not print any message and simply raise
 exception when appropriate.  *)


(** Exception for modules which depend on {!Message}. *)
exception E of Error.t

val set_log : Format.formatter -> unit

val error : Error.t -> 'a
val error_at : Stringdata.t -> Error.t -> 'a
val warning : Error.t -> unit
val warning_at : Stringdata.t -> Error.t -> unit

(** Information *)
type info =
  | INFO_file_XML_valid of File_in.t
  | INFO_file_XML_invalid
  | INFO_file_XML_notwf
  | INFO_validating_element of Stringdata.t
  | INFO_testing_element of Stringdata.t
  | INFO_valid_element of Stringdata.t
  | INFO_invalid_element of Stringdata.t
  | INFO_fetching_td of int
  | INFO_fetching_ed of int
  | INFO_fetching_ad of int
  | INFO_fetching_agd of int
  | INFO_fetching_mgd of int
  | INFO_document_valid
  | INFO_pattern of Stringdata.t
  | INFO_XSD_primary_file of File_in.t
  | INFO_XSD_secondary_file of File_in.t
  | INFO_validator_configuration of Xml.configuration

val info : info -> unit
val info_at : Stringdata.t -> info -> unit

val valid_exit_code : int
val default_error_exit_code : int
val error_exit_code : Error.t -> int

val warning_cvc : Lxsd.cvc -> unit
val warning_cvc_at : Stringdata.t -> Lxsd.cvc -> unit

val info_cvc : Lxsd.cvc -> unit
val info_cvc_at : Stringdata.t -> Lxsd.cvc -> unit

module Pr_debug : sig
  val fp_utf8_range : Format.formatter -> Utf8.range -> unit
  val hex_string_of_string : string -> string
  val string_hex_of_string : string -> string
  val string_of_lexeme : Xml.lexeme -> string
end

