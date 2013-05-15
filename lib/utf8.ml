(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: utf8.ml 1695 2012-06-04 08:36:39Z maarek $ *)


(** {1 UTF-8} *)

(** {2 UTF-8 types} *)

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

(** {2 UTF-8 character encoding}

Restricted to the subset of UTF-8 characters accepted by XML 1.0 or XML 1.1.
 *)

(** Check if the given byte is a continuation byte in an UTF-8
    sequence. *)
let continuation_byte c =
  let n = Char.code c in
  n < 0xc0 && n >= 0x80

(** Check if the given byte is a first byte in a 1-byte UTF-8
     sequence. *)
let length_1_first_byte c =
  let n = Char.code c in
  n < 0x80 && n >= 0x1

(** Check if the given byte is a first byte in a 2-bytes UTF-8
     sequence. *)
let length_2_first_byte c =
  let n = Char.code c in
  n < 0xe0 && n >= 0xc0

(** Check if the given byte is a first byte in a 3-bytes UTF-8
     sequence. *)
let length_3_first_byte c =
  let n = Char.code c in
  n < 0xf0 && n >= 0xe0

(** Check if the given byte is a first byte in a 4-bytes UTF-8
     sequence. *)
let length_4_first_byte c =
  let n = Char.code c in
  n < 0xf8 && n >= 0xf0

(** Make an UTF-8 character out of the list of significant digits
    given. For 2-, 3- or 4-bytes encodings, the UTF-8 character should
    have a code higher than [0x80], [0x800] or [0x10000].
 *)
let make_char l =
  let u =
    Charcode (List.fold_left
            (fun leftpart rightpart -> (leftpart lsl 6) lor rightpart)
            0x0
            l) in
  if match List.length l with
  | 2 -> u >= Charcode 0x80
  | 3 -> u >= Charcode 0x800
  | 4 -> u >= Charcode 0x10000
  | _ -> true
  then Some u
  else None

(** Compare two given bytes of two UTF-8 encodings of characters,
    assuming that they either are the first bytes of the encoding or the
    preceding bytes of their encodings are identical. *)
let compare_byte c1 c2 =
  let rank c =
    if length_1_first_byte c
    then 1
    else if length_2_first_byte c
    then 2
    else if length_3_first_byte c
    then 3
    else if length_4_first_byte c
    then 4
    else 5
  in
  let rank_comp = compare (rank c1) (rank c2) in
  if rank_comp = 0
  then compare c1 c2
  else rank_comp

(** [test_enc enc] Test if a given UTF-8 encoding [enc] is a
    valid UTF-8 character.

    A valid UTF-8 character is composed of a sequence of 1 to 4
    bytes. The sequence of byte is checked for coherence (with tests
    {!length_1_first_byte}, {!length_2_first_byte},
    {!length_3_first_byte} {!length_4_first_byte},
    {!continuation_byte} accordingly). The significant digits
    of the encoding are checked by {!make_char}.
*)
let test_enc = function
  | Byte1 c1 ->
      length_1_first_byte c1 &&
      let char_opt =
        make_char [ Char.code c1 ] in
      Lib.opt_bool char_opt
  | Byte2 (c1,c2) ->
      length_2_first_byte c1 &&
      continuation_byte c2 &&
      let char_opt =
        make_char [ Char.code c1 land 0x1f;
                    Char.code c2 land 0x3f ] in
      Lib.opt_bool char_opt
  | Byte3 (c1,c2,c3) ->
      length_3_first_byte c1 &&
      continuation_byte c2 &&
      continuation_byte c3 &&
      let char_opt =
        make_char [ Char.code c1 land 0x0f;
                    Char.code c2 land 0x3f;
                    Char.code c3 land 0x3f ] in
      Lib.opt_bool char_opt
  | Byte4 (c1,c2,c3,c4) ->
      length_4_first_byte c1 &&
      continuation_byte c2 &&
      continuation_byte c3 &&
      continuation_byte c4 &&
      let char_opt =
        make_char [ Char.code c1 land 0x07;
                    Char.code c2 land 0x3f;
                    Char.code c3 land 0x3f;
                    Char.code c4 land 0x3f ] in
      Lib.opt_bool char_opt


(** {2 UTF-8 encoding ranges} *)


(** Return the previous char *)
let prev_char c =
  let n = (Char.code c) - 1 in
  let c' =
    try Char.chr n with
    | Invalid_argument _ -> c in
  c'

(** Return the next char *)
let next_char c =
  let n = (Char.code c) + 1 in
  let c' =
    try Char.chr n with
    | Invalid_argument _ -> c in
  c'

(** [split_range_before c r] splits range [r] into two ranges, the
    splitting line is before character [c] ([c] is included in the
    second returned range).
    @raise Error.E [Error.UTF8_range_split_invalid]
 *)
let split_range_before c = function
  | R(c1,c2,sub) ->
      if (c1 < c) && (c <= c2)
      then
        R(c1,(prev_char c),sub),R(c,c2,sub)
      else
        raise (Error.E Error.UTF8_range_split_invalid)

(** [split_range_after c r] splits range [r] into two ranges, the
    splitting line is after character [c] ([c] is included in the
    first returned range).
    @raise Error.E [Error.UTF8_range_split_invalid]
*)
let split_range_after c = function
  | R(c1,c2,sub) ->
      if (c1 <= c) && (c < c2)
      then
        R(c1,c,sub),R((next_char c),c2,sub)
      else
        raise (Error.E Error.UTF8_range_split_invalid)


let rec string_of_single_range_list = function
    [] -> ""
  | R(c1,c2,_)::t ->
      (String.make 1 c1) ^ "-" ^ (String.make 1 c2) ^ " " ^
      string_of_single_range_list t

(** Merge contiguous ranges. *)
let merge_ranges r_l =
  let rec aux cont = function
    | ([] | [_]) as l -> cont l
    | (R(r1_c1,r1_c2,r1_sub) as r1)::
      (R(r2_c1,r2_c2,r2_sub) as r2)::
      r_t ->
        if r1_sub = r2_sub && r1_c2 = prev_char r2_c1
        then
          aux (fun r -> cont r) (R(r1_c1,r2_c2,r1_sub)::r_t)
        else
          aux (fun r -> cont (r1::r)) (r2::r_t)
  in
  aux (fun x -> x) r_l

(** Perform a byte-n ranges subtraction. [sub_char_ranges sub_fun
    empty_test rl_1 rl_2] subtracts the byte-n ranges [rl_2] from the
    byte-n ranges [rl_1], [sub_fun] is the subtraction function for
    the next level of ranges (byte-(n-1)) and [empty_test] is a test
    of empty ranges for this next level of ranges.

    Invariant: ranges are sorted not overlapping
    @raise Error.E [Error.UTF8_range_split_invalid]
    @raise Error.E [Error.UTF8_range_split_invalid]
    [sub_char_ranges] might also raise whatever exceptions [sub_fun]
    and [empty_test] might raise
*)
let sub_char_ranges sub_fun empty_test rl_1 rl_2 =
  let rec aux cont = function
    | [],_r2s -> (* r2s have no effects*)
        cont []
    | r1s,[] -> (* we keep r1s *)
        cont (r1s)
    | (((R(r1_c1,r1_c2,r1_sub) as r1_h)::r1_t) as r1_l),
      (((R(r2_c1,r2_c2,r2_sub) as r2_h)::r2_t) as r2_l) ->
        if r1_c2 < r2_c1 (* r1 before r2, we keep r1 *)
        then
          aux (fun rs -> cont (r1_h::rs)) (r1_t, r2_l)
        else if r1_c1 > r2_c2 (* r1 after r2, r2 has no effect *)
        then
          aux cont (r1_l, r2_t)
        else
          (* r1 and r2 are overlapping *)
          if r1_c1 > r2_c1 (* r1 starts after r2,
                              the beginning of r2 has no effect *)
          then
            aux cont (r1_l, R(r1_c1,r2_c2,r2_sub)::r2_l)
          else if r1_c1 < r2_c1 (* r1 starts before r2,
                                   we keep the beginning of r1 *)
          then
            let r1_1,r1_2 = split_range_before r2_c1 r1_h in
            aux (fun rs -> cont (r1_1::rs)) ((r1_2::r1_t),r2_l)
          else if r1_c2 < r2_c2 (* r1 ends before r2, we split r2 *)
          then
            let r2_1,r2_2 = split_range_after r1_c2 r2_h in
            aux cont (r1_l,(r2_1::r2_2::r2_t))
          else if r1_c2 > r2_c2 (* r1 ends after r2, we split r1 *)
          then
            let r1_1,r1_2 = split_range_after r2_c2 r1_h in
            aux cont ((r1_1::r1_2::r1_t),r2_l)
          else
            (* r1 and r2 are equal, we apply sub_fun and
               return the range only if the sub is not empty *)
            let sub' = sub_fun r1_sub r2_sub in
            if empty_test sub'
            then
              aux cont (r1_t,r2_t)
            else
              aux
                (fun rs -> cont (R(r1_c1,r1_c2,sub')::rs))
                (r1_t,r2_t)
  in
  let rl' = aux (fun x -> x) (rl_1,rl_2) in
  merge_ranges rl'

(** Subtraction function for byte-1's next level of ranges. Byte1's
    next level has no ranges. *)
let sub_fun_byte1 B1 B1 = B1

(** Test function for empty byte-1's next level of ranges. Byte1's
    next level has no ranges therefore it is always empty. *)
let empty_test_byte1 B1 = true

(** Subtraction function for byte-2's next level of ranges.
    @raise Error.E [Error.UTF8_range_split_invalid]
    @raise Error.E [Error.UTF8_range_split_invalid]
*)
let sub_fun_byte2 (B2 l1) (B2 l2) =
  B2 (sub_char_ranges sub_fun_byte1 empty_test_byte1 l1 l2)

(** Test function for empty byte-2's next level of ranges. *)
let empty_test_byte2 = function
  | B2 [] -> true
  | B2 _ -> false

(** Subtraction function for byte-3's next level of ranges.
    @raise Error.E [Error.UTF8_range_split_invalid]
    @raise Error.E [Error.UTF8_range_split_invalid]
*)
let sub_fun_byte3 (B3 l1) (B3 l2) =
  B3 (sub_char_ranges sub_fun_byte2 empty_test_byte2 l1 l2)

(** Test function for empty byte-3's next level of ranges. *)
let empty_test_byte3 = function
  | B3 [] -> true
  | B3 _ -> false

(** Subtraction function for byte-4's next level of ranges.
    @raise Error.E [Error.UTF8_range_split_invalid]
    @raise Error.E [Error.UTF8_range_split_invalid]
*)
let sub_fun_byte4 (B4 l1) (B4 l2) =
  B4 (sub_char_ranges sub_fun_byte3 empty_test_byte3 l1 l2)

(** Test function for empty byte-4's next level of ranges. *)
let empty_test_byte4 = function
  | B4 [] -> true
  | B4 _ -> false

(** UTF-8 encoding ranges subtraction function.
    @raise Error.E [Error.UTF8_range_split_invalid]
    @raise Error.E [Error.UTF8_range_split_invalid]
*)
let sub
    { byte1 = r1_b1;
      byte2 = r1_b2;
      byte3 = r1_b3;
      byte4 = r1_b4 }
    { byte1 = r2_b1;
      byte2 = r2_b2;
      byte3 = r2_b3;
      byte4 = r2_b4 } =
  { byte1 = sub_char_ranges sub_fun_byte1 empty_test_byte1 r1_b1 r2_b1;
    byte2 = sub_char_ranges sub_fun_byte2 empty_test_byte2 r1_b2 r2_b2;
    byte3 = sub_char_ranges sub_fun_byte3 empty_test_byte3 r1_b3 r2_b3;
    byte4 = sub_char_ranges sub_fun_byte4 empty_test_byte4 r1_b4 r2_b4 }


(** Empty UTF-8 encoding range. *)
let empty =
  { byte1 = [];
    byte2 = [];
    byte3 = [];
    byte4 = [] }


(** Minimum continuation byte *)
let min_continuation_byte = Char.chr 0x80
(** Maximum continuation byte *)
let max_continuation_byte = Char.chr 0xBF

(** Minimum first byte of a 1-byte character *)
let min_1byte_b1 = Char.chr 0x01
(** Maximum first byte of a 1-byte character *)
let max_1byte_b1 = Char.chr 0x7F

(** Minimum first byte of a 2-bytes character *)
let min_2bytes_b1 = Char.chr 0xC0
(** Maximum first byte of a 2-bytes character *)
let max_2bytes_b1 = Char.chr 0xDF

(** Minimum first byte of a 3-bytes character *)
let min_3bytes_b1 = Char.chr 0xE0
(** Maximum first byte of a 3-bytes character *)
let max_3bytes_b1 = Char.chr 0xEF

(** Minimum first byte of a 4-bytes character *)
let min_4bytes_b1 = Char.chr 0xF0
(** Maximum first byte of a 4-bytes character *)
let max_4bytes_b1 = Char.chr 0xF7

let full_continuation_B2 =
  B2 [ R (min_continuation_byte,
          max_continuation_byte,
          B1) ]

let full_continuation_B3 =
  B3 [ R (min_continuation_byte,
          max_continuation_byte,
          full_continuation_B2) ]

let full_continuation_B4 =
  B4 [ R (min_continuation_byte,
          max_continuation_byte,
          full_continuation_B3) ]

(** Full 1-byte characters range. *)
let full_range_byte1 : byte1 char_range list =
  [ R (min_1byte_b1,
       max_1byte_b1,
       B1) ]

(** Full 2-bytes characters range. *)
let full_range_byte2 : byte2 char_range list =
  [ R (min_2bytes_b1,
       max_2bytes_b1,
       full_continuation_B2) ]

(** Full 3-bytes characters range. *)
let full_range_byte3 : byte3 char_range list =
  [ R (min_3bytes_b1,
       max_3bytes_b1,
       full_continuation_B3) ]

(** Full 4-bytes characters range. *)
let full_range_byte4 : byte4 char_range list =
  [ R (min_4bytes_b1,
       max_4bytes_b1,
       full_continuation_B4) ]

(** Full range of XML UTF-8 characters. *)
let full =
  { byte1 = full_range_byte1;
    byte2 = full_range_byte2;
    byte3 = full_range_byte3;
    byte4 = full_range_byte4 }

(** Complement of a given range.
    @raise Error.E [Error.UTF8_range_split_invalid]
    @raise Error.E [Error.UTF8_range_split_invalid]
*)
let compl r =
  sub full r

(** Union of two given ranges.
    @raise Error.E [Error.UTF8_range_split_invalid]
    @raise Error.E [Error.UTF8_range_split_invalid]
*)
let union r_l =
  let rec aux c_r = function
    | [] -> compl c_r
    | h::t ->
        aux (sub c_r h) t
  in
  aux full r_l

(** [next next_fun d] Return the first UTF-8 character of a given data
    structure [d] and its remainder. Function [next_fun] returns the
    next byte of the data structure. The UTF-8 validity test is
    performed by the function {!test_enc}. Return [None] if the string
    data is empty.

    Function [next_fun] is meant to avoid circular dependency between
    {!Utf8} and {!Stringdata}.

    @raise Error.E [Error.UTF8_invalid] if the given structure does not
    start with a valid UTF-8 character
    @raise Error.E [Error.UTF8_first_byte_invalid] if the first byte is
    not a 1-, 2-, 3- or 4-bytes UTF-8 first byte.
    @raise Error.E [Error.UTF8_overlong] if the character could have been
    encoded with less bytes
    It might also raise whatever [next_fun] raises.
  *)
let next next_fun sd =
  match next_fun sd with
  | None ->
      None
  | Some (c1,sd1) ->
      let u,sd' =
        if length_1_first_byte c1
        then
          (Byte1 c1),sd1
        else
          begin
            match next_fun sd1 with
            | None ->
                raise (Error.E Error.UTF8_invalid)
            | Some (c2,sd2) ->
                if length_2_first_byte c1
                then
                  (Byte2 (c1,c2)),sd2
                else
                  begin
                    match next_fun sd2 with
                    | None ->
                        raise (Error.E Error.UTF8_invalid)
                    | Some (c3,sd3) ->
                        if length_3_first_byte c1
                        then
                          (Byte3 (c1,c2,c3)),sd3
                        else
                          begin
                            match next_fun sd3 with
                            | None ->
                                raise (Error.E Error.UTF8_invalid)
                            | Some (c4,sd4) ->
                                if length_4_first_byte c1
                                then
                                  (Byte4 (c1,c2,c3,c4)),sd4
                                else
                                    raise (Error.E Error.UTF8_first_byte_invalid)
                          end
                  end
          end
      in
      if test_enc u
      then Some (u,sd')
      else
        raise (Error.E Error.UTF8_overlong)

(** Return a range including only the given character encoding. *)
let range_of_enc = function
  | Byte1 (c1) ->
    { byte1 =
      [ R
          (c1,c1,B1) ];
      byte2 = [];
      byte3 = [];
      byte4 = [] }
  | Byte2 (c1,c2) ->
    { byte1 = [];
      byte2 =
      [ R
          (c1,c1,
           B2
             [ R (c2,c2,B1) ]) ];
      byte3 = [];
      byte4 = [] }
  | Byte3 (c1,c2,c3) ->
    { byte1 = [];
      byte2 = [];
      byte3 =
      [ R
          (c1,c1,
           B3
             [ R
                 (c2,c2,
                  B2
                    [ R (c3,c3,B1) ]) ]) ];
      byte4 = [] }
  | Byte4 (c1,c2,c3,c4) ->
    { byte1 = [];
      byte2 = [];
      byte3 = [];
      byte4 =
      [ R
          (c1,c1,
           B4
             [ R
                 (c2,c2,
                  B3
                    [ R
                        (c3,c3,
                         B2
                           [ R (c4,c4,B1) ]) ]) ]) ] }


let range_downto c max sub full_sub =
  let c' = next_char c in
  merge_ranges
    ( R (c,c,sub) ::
      (if c < c' && c' <= max
      then
        [ R (c',max,full_sub) ]
      else
        [] ))

let range_downto_enc = function
  | Byte1 c1 ->
      { byte1 = range_downto c1 max_1byte_b1 B1 B1;
        byte2 = full_range_byte2;
        byte3 = full_range_byte3;
        byte4 = full_range_byte4 }
  | Byte2 (c1,c2) ->
      { byte1 = [];
        byte2 =
        range_downto c1 max_2bytes_b1
          (B2 (range_downto c2 max_continuation_byte B1 B1))
          full_continuation_B2;
        byte3 = full_range_byte3;
        byte4 = full_range_byte4 }
  | Byte3 (c1,c2,c3) ->
      { byte1 = [];
        byte2 = [];
        byte3 =
        range_downto c1 max_3bytes_b1
          (B3 (range_downto c2 max_continuation_byte
                 (B2 (range_downto c3 max_continuation_byte B1 B1))
                 full_continuation_B2))
          full_continuation_B3;
        byte4 = full_range_byte4 }
  | Byte4 (c1,c2,c3,c4) ->
      { byte1 = [];
        byte2 = [];
        byte3 = [];
        byte4 =
        range_downto c1 max_4bytes_b1
          (B4 (range_downto c2 max_continuation_byte
                 (B3 (range_downto c3 max_continuation_byte
                        (B2 (range_downto c4 max_continuation_byte B1 B1))
                        full_continuation_B2))
                 full_continuation_B3))
          full_continuation_B4 }



let range_upto min c sub full_sub =
  let c' = prev_char c in
  let r = R (c,c,sub) in
  merge_ranges
    (if c' < c && min <= c'
    then
      R (min,c',full_sub) :: [ r ]
    else
      [ r ] )

let range_upto_enc = function
  | Byte1 c1 ->
      { byte1 = range_upto min_1byte_b1 c1 B1 B1;
        byte2 = [];
        byte3 = [];
        byte4 = [] }
  | Byte2 (c1,c2) ->
      { byte1 = full_range_byte1;
        byte2 =
        range_upto min_2bytes_b1 c1
          (B2 (range_upto min_continuation_byte c2 B1 B1))
          full_continuation_B2;
        byte3 = [];
        byte4 = [] }
  | Byte3 (c1,c2,c3) ->
      { byte1 = full_range_byte1;
        byte2 = full_range_byte2;
        byte3 =
        range_upto min_3bytes_b1 c1
          (B3 (range_upto min_continuation_byte c2
                 (B2 (range_upto min_continuation_byte c3 B1 B1))
                 full_continuation_B2))
          full_continuation_B3;
        byte4 = [] }
  | Byte4 (c1,c2,c3,c4) ->
      { byte1 = full_range_byte1;
        byte2 = full_range_byte2;
        byte3 = full_range_byte3;
        byte4 =
        range_upto min_4bytes_b1 c1
          (B4 (range_upto min_continuation_byte c2
                 (B3 (range_upto min_continuation_byte c3
                        (B2 (range_upto min_continuation_byte c4 B1 B1))
                        full_continuation_B2))
                 full_continuation_B3))
          full_continuation_B4 }




let string_of_range = function
    { byte1 = l1;
      byte2 = l2;
      byte3 = l3;
      byte4 = l4 } ->
        (string_of_single_range_list l1) ^ "," ^
        (string_of_int (List.length l2)) ^ "," ^
        (string_of_int (List.length l3)) ^ "," ^
        (string_of_int (List.length l4))

(** Return a range between two given character encodings. Return the
    empty range if the second character's Unicode code is lower than
    the one of the second.
    @raise Error.E [Error.UTF8_range_split_invalid]
    @raise Error.E [Error.UTF8_range_split_invalid]
 *)
let range_of_enc_range enc1 enc2 =
  if enc1 = enc2
  then
    range_of_enc enc1
  else
    sub
      (range_downto_enc enc1)
      (sub
         (range_downto_enc enc2)
         (range_of_enc enc2))

(** Test whether a range is over single byte encodings only. *)
let test_single_byte_range = function
  | { byte1 = _;
      byte2 = [];
      byte3 = [];
      byte4 = [] } -> true
  | { byte1 = _;
      byte2 = _;
      byte3 = _;
      byte4 = _ } -> false

(** *)
let enc_of_int i =
  if i < 0x01
  then
    raise (Error.E Error.Unicode_code_point_out_of_range)
  else if i < 0x80
  then
    Byte1 (Char.chr i)
  else if i < 0x800
  then
    Byte2 ((Char.chr (i lsr 6 lor 0xC0)),
                (Char.chr (i land 0x3F lor 0x80)))
  else if i < 0x10000
  then
    Byte3 ((Char.chr (i lsr 12 lor 0xE0)),
                (Char.chr (i lsr 6 land 0x3F lor 0x80)),
                (Char.chr (i land 0x3F lor 0x80)))
  else if i < 0x200000
  then
    Byte4 ((Char.chr (i lsr 18 lor 0xF0)),
                (Char.chr (i lsr 12 land 0x3F lor 0x80)),
                (Char.chr (i lsr 6 land 0x3F lor 0x80)),
                (Char.chr (i land 0x3F lor 0x80)))
  else
    raise (Error.E Error.Unicode_code_point_out_of_range)

(** Return a range between two given Unicode codes. Return the empty
    range if the second code is lower than the one of the second. *)
let range_of_ints int1 int2 =
  let enc1 = enc_of_int int1 in
  let enc2 = enc_of_int int2 in
  range_of_enc_range enc1 enc2

(** Return a range including only the given Unicode code. *)
let range_of_int int1 =
  let enc1 = enc_of_int int1 in
  range_of_enc enc1




let string_of_enc = function
  | Byte1 c1 ->
      Lib.string_of_char c1
  | Byte2 (c1,c2) ->
      Lib.string_of_char c1 ^
      Lib.string_of_char c2
  | Byte3 (c1,c2,c3) ->
      Lib.string_of_char c1 ^
      Lib.string_of_char c2 ^
      Lib.string_of_char c3
  | Byte4 (c1,c2,c3,c4) ->
      Lib.string_of_char c1 ^
      Lib.string_of_char c2 ^
      Lib.string_of_char c3 ^
      Lib.string_of_char c4


