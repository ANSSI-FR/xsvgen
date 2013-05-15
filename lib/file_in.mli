(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: file_in.mli 1499 2012-05-02 09:20:22Z maarek $ *)


type extension =
  | ML
  | Bin
  | Xsd
  | Xml

type t

val of_string : string -> extension list -> t

val to_string : t -> string

val to_in_channel : t -> extension list -> in_channel

val close_opened_files : unit -> unit
