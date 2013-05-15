(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: val_parser.mli 1455 2012-04-25 07:20:02Z maarek $ *)

(** Alias type for validation result *)
type vresult = ((Lxsd.cvc * Stringdata.t option) list,
                (Lxsd.cvc * Stringdata.t option) list) Lib.result

val validate_schema :
    Xml.expanded_tree -> vresult
