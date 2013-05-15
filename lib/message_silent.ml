(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: message_silent.ml 1665 2012-05-24 09:47:08Z maarek $ *)

(** {1 Message handling library, silent version} *)


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

let valid_exit_code =
  0

let default_error_exit_code = 1

let error_exit_code = function
  | _ -> default_error_exit_code




exception E of Error.t

type mtype =
  | Error
  | Warning
  | Info
  | Debug
  | Todo

let log_formatter_setup = ref false


let set_log _ =
  if !log_formatter_setup
  then
    raise (Error.E Error.A_double_log_setting)
  else
    begin
      log_formatter_setup := true
    end

let error_fun err =
  raise (E err)


let error err =
  error_fun err

let error_at _ err =
  error_fun err

let warning _ =
  ()

let warning_at _ _ =
  ()

let info _ =
  ()

let info_at _ _ =
  ()

let warning_cvc _ =
  ()

let warning_cvc_at _ _ =
  ()

let info_cvc _ =
  ()

let info_cvc_at _ _ =
  ()


module Pr_debug = struct
  let fp_utf8_range _ _ =
    ()
  let hex_string_of_string _ =
    String.copy ""
  let string_hex_of_string _ =
    String.copy ""
  let string_of_lexeme _ =
    String.copy ""
end
