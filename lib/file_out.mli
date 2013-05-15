(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: file_out.mli 1501 2012-05-02 09:42:38Z maarek $ *)


type extension =
  | ML
  | Sh
  | Log

type t

type prepared

val of_string : bool -> string -> t

val prepare : t -> prepared

val to_formatter : prepared -> extension -> Format.formatter

val fprintf : Format.formatter -> prepared -> unit

val close_opened_files : unit -> unit
