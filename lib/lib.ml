(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lib.ml 1557 2012-05-05 14:25:26Z maarek $ *)

(** {1 General library} *)

type ('a,'b) either =
  | A of 'a
  | B of 'b

type ('a,'b) result =
  | OK of 'a
  | KO of 'b

let opt_eq eq o1 o2 =
  match o1,o2 with
  | None, None ->
      true
  | (Some _), None | None, (Some _) ->
      false
  | (Some v1), (Some v2) ->
      eq v1 v2

let opt_compare compare o1 o2 =
  match o1,o2 with
  | None, None -> 0
  | None, (Some _) -> -1
  | (Some _), None -> 1
  | (Some v1), (Some v2) ->
      compare v1 v2

let list_of_opt = function
  | None -> []
  | Some a -> [a]


(** This function might raise any exception [eq] might raise.
*)
let list_mem_duplicates eq l =
  let rec mem = function
    | [] | [_] -> false
    | e1::(e2::_ as t) ->
        if eq e1 e2
        then true
        else mem t
  in
  mem l


(** [list_find ok get l] returns the value returned by the function
    [get] when applied to the first element from list [l] accepted by
    the test function [ok]. Otherwise, returns
    [None]. Tail-recursive.
    This function might raise any exception [ok] or [get] might raise.
*)
let list_find ok get l =
  let rec find = function
    | [] -> raise Not_found
    | h::l -> if ok h then get h else find l in
  try Some (find l) with
  | Not_found -> None

(** [list_assoc eq x l] returns the element associated in list [l] to
    the first equal to [x] according to the equality function
    [eq]. Otherwise return [None]. {!list_find} specialisation.
    This function might raise any exception [eq] might raise.
*)
let list_assoc eq x l =
  list_find
    (fun (a,_) -> eq a x)
    (fun (_,b) -> b)
    l

(** [list_find_remove ok get l] returns a pair composed of
    - the value returned by the function [get] when applied to the
      first element from list [l] matching the test function [ok],
    - the list [l] without the matching element.
    Otherwise, returns [None]. Tail-recursive.
    This function might raise any exception [ok] or [get] might raise.
*)
let list_find_remove ok get l =
  let rec find_remove cont = function
    | [] -> cont (None,[])
    | h::t ->
        if ok h
        then cont ((Some (get h)), t)
        else find_remove (fun (e,r) -> cont (e,(h::r))) t
  in
  find_remove (fun x -> x) l

(** This function might raise any exception [ok] or [f] might raise.
*)
let list_partial_map ok f l =
  let rec partial_map cont = function
    | [] -> cont []
    | h::t ->
        if ok h
        then partial_map (fun r -> cont ((f h)::r)) t
        else partial_map (fun r -> cont r) t
  in
  partial_map (fun x -> x) l


(** Partition a given list according to a test function and map each
    element depending on its partition. *)
let list_partition test f1 f2 l =
  let rec partition cont = function
    | [] -> cont ([],[])
    | h::t ->
        if test h
        then partition (fun (l1,l2) -> cont (((f1 h)::l1),l2)) t
        else partition (fun (l1,l2) -> cont (l1,((f2 h)::l2))) t
  in
  partition (fun x -> x) l

(** This function might raise any exception [ok] might raise.
*)
let list_mem ok l =
  let rec mem = function
    | [] -> false
    | h::t ->
        if ok h
        then true
        else mem t
  in
  mem l


(** This function might raise any exception the functions of list [l] might
    raise.
*)
let list_opt_applies e l =
  let rec opt_applies = function
    | [] -> None
    | apply::t ->
        match apply e with
        | None -> opt_applies t
        | Some _ as r -> r in
  opt_applies l


(** This function might raise any exception the functions of list [app_l] might
    raise.
*)
let list_opt_list_applies l app_l =
  let rec opt_applies = function
    | [] -> None
    | apply::t ->
        match apply l with
        | None -> opt_applies t
        | Some _ as r -> r in
  opt_applies app_l

(** This function might raise any exception the functions of list
    [opt_applies] might raise.
*)
let list_next_remove opt_applies l =
  let rec next_remove cont = function
    | [] -> cont ([],[])
    | h::t as l ->
        match list_opt_applies h opt_applies with
        | None ->
            cont ([],l)
        | Some h' ->
            next_remove
              (fun (result_l,remainder_l) ->
                cont (h'::result_l,remainder_l))
              t
  in
  next_remove (fun x -> x) l


(** This function might raise any exception the functions of list
    [opt_applies] might raise.
*)
let list_next_none_or_one_remove opt_applies = function
  | [] ->
      None,[]
  | h::t as l ->
      begin
        match list_opt_applies h opt_applies with
        | None ->
            None,l
        | Some h' ->
            (Some h'),t
      end

(** [list_next_one_remove opt_applies l] attempts to apply one of the
    functions of the list [opt_applies] to the first element of the list
    [l]. Returns either [OK] with the value and the remainder of the list
    or [KO ()].
    This function might raise any exception the functions of list
    [opt_applies] might raise.
*)
let list_next_one_remove opt_applies = function
  | [] ->
      (* nothing next, but one of the choices expected *)
      KO ()
  | h::t ->
      begin
        match list_opt_applies h opt_applies with
        | None ->
            (* something next, but not matching any of the choices *)
            KO ()
        | Some h' ->
            (* something next and matching the choices *)
            OK (h', t)
      end

(** This function might raise any exception the function [f] might raise.
*)
let list_next_list_wrap f = function
  | [] -> None
  | h::t ->
      begin
        match f h with
        | None -> None
        | Some h' -> Some (h',t) 
      end



(** This function might raise any exception the functions of list
    [opt_applies] might raise.
*)
let list_next_choice_remove opt_applies = function
  | [] ->
      (* nothing next, but one of the choices expected *)
      None
  | l ->
      begin
        match list_opt_list_applies l opt_applies with
        | None ->
            (* something next, but not matching any of the choices *)
            None
        | Some (rl,l') ->
            (* something next and matching the choices *)
            Some (rl,l')
      end


(** NOT USED (but list_next_all_remove)
    This function might raise any exception the functions of list
    [l] might raise.
*)
let list_opt_applies_remove e l  =
  let rec opt_applies cont = function
    | [] -> None
    | apply::t ->
        match apply e with
        | None ->
            opt_applies
              (function None -> None
                | Some (r,rl) -> cont (Some (r,(apply::rl))))
              t
        | Some r -> cont (Some (r,t)) in
  opt_applies (fun x -> x) l

(** NOT USED
    This function might raise any exception the functions of list
    [opt_applies] might raise.
*)
let list_next_all_remove opt_applies l =
  let finished r = function
    | [] ->
        (* nothing next and nothing expected *)
        OK r
    | app_l ->
        (* nothing next, but something expected *)
        KO app_l in
  let rec next_remove cont app_l = function
    | [] ->
        cont (finished ([],[]) app_l)
    | h::t as l ->
        match list_opt_applies_remove h app_l with
        | None ->
            cont (finished ([],l) app_l)
        | Some (h',app_l') ->
            next_remove
              (function
                | OK (result_l,remainder_l) ->
                    cont (OK ((h'::result_l),remainder_l))
                | KO _ as r -> r)
              app_l'
              t
  in
  next_remove (fun x -> x) opt_applies l

let list_flatten_opt l =
  let rec flatten_opt cont = function
    | [] -> cont []
    | None::t ->
        flatten_opt (fun x -> cont x)  t
    | (Some v)::t ->
        flatten_opt (fun r -> cont (v::r)) t in
  flatten_opt (fun x -> x) l

(* @raise Not_Found
   This function might raise any exception the function [eq] might
   raise
*)
let list_assocs eq keys l =
  let rec assocs cont = function
    | [],_ -> cont []
    | _,[] -> raise Not_found
    | keys,((key,v)::t) ->
        begin
          match list_find_remove (eq key) (fun x -> x) keys with
          | None,_ -> assocs (fun x -> cont x) (keys,t)
          | Some _,keys' ->  assocs (fun r -> cont (v::r)) (keys',t)
        end
  in
  assocs (fun x -> x) (keys,l)


let opt_apply f = function
  | None -> None
  | Some a -> Some (f a)

let opt_test f = function
  | None -> false
  | Some a -> f a

let opts_test f opt_1 opt_2 =
  match opt_1,opt_2 with
  | None,None -> true
  | (Some v_1),(Some v_2) ->
      f v_1 v_2
  | _,_ -> false


let opt_bool = function
  | None -> false
  | Some _ -> true

let opt_default default = function
  | None -> default
  | Some v -> v

(** This function might raise any exception the functions of list [l]
    might raise.
*)
let choice l content =
  let rec apply_choice = function
    | [] -> None
    | f::funs ->
        begin
          match f content with
          | None -> apply_choice funs
          | Some _ as r -> r
        end
  in
  apply_choice l

(** [list_make n elem] makes a list composed of [n] times the element [elem].
    Not tail-recursive.
 *)
let list_make n elem =
  let rec aux = function
    | 0 -> []
    | i -> elem :: (aux (i-1))
  in
  aux n


(** [list_make_fold n constr] makes a list composed of [n] times
    the pattern described by the function [constr] with element [elem] as
    a base.
    [list_make_fold n constr elem = constr elem (constr elem (... (constr elem elem)))]
    Not tail-recursive.
 *)
let list_make_fold n constr elem =
  let rec aux = function
    | 0 -> elem
    | i -> constr (aux (i-1))
  in
  aux n

(** {2 Functions for manipulating list-results} *)

(** Combine two results with a list describing a success or a failure. *)
let list_result_combine result1 result2 =
  match result1 with
  | OK rules ->
      begin
        match result2 with
        | OK l -> OK (rules @ l)
        | KO l -> KO l
      end
  | KO rules ->
      begin
        match result2 with
          | OK _ -> KO rules
          | KO l -> KO (rules @ l)
      end

let list_result_labelise result l =
  match result with
  | OK rules ->
      OK (rules @ l)
  | KO rules ->
      KO (rules @ l)

(** [list_test_all tests value] returns the combined result of all the
    test functions [tests] applied on [value].
    - If the tests succeed, then return [OK l] where [l] is the set of
      succeeding rules;
    - If the tests fail, then return [KO l] where [l] is the set of
      failed rules.
    This function might raise any exception the functions of list [l]
    might raise.
 *)
let list_test_all tests value =
  let rec aux cont = function
    | [] -> cont (OK [])
    | tfun::tfuns ->
        aux
          (fun r -> cont (list_result_combine (tfun value) r))
          tfuns
  in
  aux (fun x -> x) tests

(** [list_test_choice rule tests value] returns the combined result of
    the first of the test functions [tests] matching [value].
    - If one test succeed, then return [OK l] where [l] is the set of
      succeeding rules of the matching test plus the given rule [rule];
    - If all tests fail, then return [KO \[rule\]].
    This function might raise any exception the functions of list [tests]
    might raise.
*)
let list_test_choice rule tests value =
  let rec apply_choice = function
    | [] -> KO [rule]
    | tfun::tfuns ->
        begin
          match tfun value with
          | KO _ -> apply_choice tfuns
          | OK l -> OK (rule::l)
        end
  in
  apply_choice tests

let string_of_char c =
  String.make 1 c



exception Repeated_map_entry

module Map_int = Map.Make (struct type t = int let compare = compare end)

let map_int_of_list l =
  let m_empty = Map_int.empty in
  List.fold_left
    (fun m (i,e) ->
      if Map_int.mem i m
      then
        raise Repeated_map_entry
      else
        Map_int.add i e m)
    m_empty
    l





(** Compare floating points numbers. Return [None] if the floating
    points numbers are incomparable. *)
let compare_float f1 f2 =
  if (classify_float f1) = FP_nan || (classify_float f2) = FP_nan
  then
    None
  else
    Some (compare f1 f2)


