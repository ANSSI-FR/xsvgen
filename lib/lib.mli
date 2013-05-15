(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lib.mli 1503 2012-05-02 10:35:08Z maarek $ *)

type ('a,'b) either =
  | A of 'a
  | B of 'b

type ('a,'b) result =
  | OK of 'a
  | KO of 'b

val opt_eq : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
val opt_compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int
val list_of_opt : 'a option -> 'a list
val list_mem_duplicates : ('a -> 'a -> bool) -> 'a list -> bool
val list_find : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b option
val list_assoc : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option
val list_find_remove : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b option * 'a list
val list_partial_map : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
val list_partition : ('a -> bool) -> ('a -> 'b) -> ('a -> 'c) -> 'a list -> 'b list * 'c list
val list_mem : ('a -> bool) -> 'a list -> bool
val list_next_remove : ('a -> 'b option) list -> 'a list -> 'b list * 'a list
val list_next_all_remove : ('a -> 'b option) list -> 'a list ->
  ('b list * 'a list, ('a -> 'b option) list) result
val list_next_none_or_one_remove : ('a -> 'b option) list -> 'a list -> 'b option * 'a list
val list_next_one_remove : ('a -> 'b option) list -> 'a list -> ('b * 'a list, unit) result

val list_next_list_wrap : ('a -> 'b option) -> 'a list -> ('b * 'a list) option

val list_next_choice_remove : ('a list -> ('b * 'a list) option) list -> 'a list -> ('b * 'a list) option

val list_flatten_opt : 'a option list -> 'a list
val list_assocs : ('a -> 'a -> bool) -> 'a list -> ('a * 'b) list -> 'b list
val list_opt_applies : 'a -> ('a -> 'b option) list -> 'b option


val opt_apply : ('a -> 'b) -> 'a option -> 'b option
val opt_test : ('a -> bool) -> 'a option -> bool
val opts_test : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
val opt_bool : 'a option -> bool
val opt_default : 'a -> 'a option -> 'a

val choice : ('a -> 'b option) list -> 'a -> 'b option


val list_make : int -> 'a -> 'a list
val list_make_fold : int -> ('a -> 'a) -> 'a -> 'a

val list_result_combine : ('a list, 'a list) result -> ('a list, 'a list) result -> ('a list, 'a list) result
val list_result_labelise : ('a list, 'a list) result -> 'a list -> ('a list, 'a list) result
val list_test_all : ('a -> ('b list, 'b list) result) list -> 'a -> ('b list, 'b list) result
val list_test_choice : 'b -> ('a -> ('b list, 'b list) result) list -> 'a -> ('b list, 'b list) result

val string_of_char : char -> string


module Map_int : Map.S with type key = int

val map_int_of_list : (Map_int.key * 'a) list -> 'a Map_int.t

val compare_float : float -> float -> int option

