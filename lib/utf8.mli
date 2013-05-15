(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: utf8.mli 1453 2012-04-24 15:55:52Z maarek $ *)

(** {1 UTF-8} *)

(** Unicode character *)
type t = Charcode of int

(** UTF-8 encoding (on 1 to 4 bytes) *)
type enc =
  | Byte1 of char
  | Byte2 of char * char
  | Byte3 of char * char * char
  | Byte4 of char * char * char * char

(** {2 UTF-8 encoding ranges} *)


(** One byte range parametrized by the next byte ranges *)
type 'a char_range =
  | R of char * char * 'a

(** One byte encoding continuation *)
type byte1 =
  | B1

(** Two bytes encoding continuation *)
type byte2 =
  | B2 of byte1 char_range list

(** Three bytes encoding continuation *)
type byte3 =
  | B3 of byte2 char_range list

(** Four bytes encoding continuation *)
type byte4 =
  | B4 of byte3 char_range list

(** UTF-8 encoding range *)
type range =
    { byte1 : byte1 char_range list; (** ranges for 1-byte characters *)
      byte2 : byte2 char_range list; (** ranges for 2-bytes characters *)
      byte3 : byte3 char_range list; (** ranges for 3-bytes characters *)
      byte4 : byte4 char_range list  (** ranges for 4-bytes characters *) }




val compare_byte : char -> char -> int
val test_enc : enc -> bool
val next : ('a -> (char * 'a) option) -> 'a -> (enc * 'a) option
val range_of_enc : enc -> range
val range_of_enc_range : enc -> enc -> range
val sub : range -> range -> range
val empty : range
val full : range
val compl : range -> range
val union : range list -> range
val test_single_byte_range : range -> bool
val enc_of_int : int -> enc
val range_of_int : int -> range
val range_of_ints : int -> int -> range
val string_of_enc : enc -> string
