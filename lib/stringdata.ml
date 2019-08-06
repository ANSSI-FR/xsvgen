(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: stringdata.ml 1686 2012-05-30 09:00:22Z maarek $ *)

(** {1 String data handling library} *)

type kind =
  | Reference of string * int * int (** Reference's value, reference's name length and accumulative offset in the source*)
  | Chars of int (** Offset in the source *)

type part =
    { p_position : int; (** Position in the expanded data *)
      p_limit : int (** End position in the expanded data *) }

module Parts =
  Map.Make
    (struct
      type t = part
      let compare
          { p_position = p_pos_1;
            p_limit = p_lim_1 }
          { p_position = p_pos_2;
            p_limit = p_lim_2 } =
        if p_pos_1 > p_lim_1 || p_pos_2 > p_lim_2
        then
          raise (Error.E Error.SD_structure)
        else if p_pos_1 = p_pos_2 && p_lim_1 = p_lim_2
        then 0
        else if p_lim_1 <= p_pos_2
        then -1
        else if p_lim_2 <= p_pos_1
        then 1
        else
          raise (Error.E (Error.SD_structure))
    end)





(** Different type of source of a string data *)
type source =
  | In_channel of in_channel * File_in.t (** The source is a file described by its input channel and its name. *)
  | String of string (** The source is a string value. *)


(** The type of string data values. *)
type t =
    { source : source;
      parts : kind Parts.t;
      exp_position : int; (** Position in the expanded string data *)
      loc_position : int; (** Position in the raw string data *)
      exp_limit : int; (** End position in the expanded string data *)
      access : int -> char }

let string_debug = function
    { source = source ;
      parts = parts;
      exp_position = pos;
      loc_position = loc;
      exp_limit = lim;
      access = _ } ->
        Format.sprintf "%s %i-%i @@%i [ %s]"
          (match source with
          | String _ -> "s " | In_channel _ -> "ic" )
          pos
          lim
          loc
          (Parts.fold
             (fun {p_position=p_pos; p_limit=p_lim} kind s ->
               (Format.sprintf "%s%i-%i+%i "
                  (match kind with
                  | Reference (_,_,_) -> "R"
                  | Chars _ -> "C")
                  p_pos
                  p_lim
                  (match kind with
                  | Reference (_,_,i)
                  | Chars i -> i)) ^ s ) parts "")


exception E_at of t * Error.t

(** Return a string data out of a file described by its input channel
    and its name.
    @raise Error.E [Error.IO_input] if the [ic] is not a valid file
    handle (e.g. an already closed file handle)
    @raise Error.E [Error.SD_structure]
 *)
let of_in_channel ic file_name =
  let length =
    try in_channel_length ic with
    | Sys_error _ ->
        raise (Error.E Error.IO_input)
  in
  { source = In_channel (ic, file_name);
    parts = Parts.singleton {p_position=0;p_limit=length} (Chars 0);
    exp_position = 0;
    loc_position = 0;
    exp_limit = length;
    access = (fun i -> try seek_in ic i; input_char ic with
                       | End_of_file ->
                           raise (Error.E Error.SD_structure)
                       | Sys_error _ ->
                           raise (Error.E Error.IO_input)) }


(** Return a string data out of a string value.
    @raise Error.E [Error.SD_structure]
 *)
let of_string s =
  let length = String.length s in
  { source = String (s);
    parts = Parts.singleton {p_position=0;p_limit=length} (Chars 0);
    exp_position = 0;
    loc_position = 0;
    exp_limit = length;
    access = (fun i -> try s.[i] with
                       | Invalid_argument _ ->
                           raise (Error.E Error.SD_structure) ) }


(** Returns the substring corresponding to a characters part of a
    string data. *)
let chars_part_to_string (pos,lim) (p_pos,p_lim) offset source =
  let len = (min lim p_lim) - (max pos p_pos) in
  let srcoff = (max pos p_pos) - offset in
  begin
    match source with
    | String s ->
        begin
          try String.sub s srcoff len with
          | Invalid_argument _ ->
              raise (Error.E Error.SD_structure)
        end
    | In_channel (ic,_) ->
        begin
          try
            seek_in ic srcoff;
            really_input_string ic len;
          with
          | End_of_file
          | Invalid_argument _ ->
              raise (Error.E Error.SD_structure)
          | Sys_error _ ->
              raise (Error.E Error.IO_input)
        end
  end


(** Return a string value out of a given string data.
    @raise Error.E [Error.SD_structure]
    @raise Error.E [Error.IO_input]
 *)
let to_string = function
  | { source = source;
      parts = parts;
      exp_position = pos;
      loc_position = _;
      exp_limit = lim;
      access = _ } ->
        Parts.fold
          (fun { p_position=p_pos; p_limit=p_lim } kind s2 ->
            if pos >= p_lim || lim < p_pos
            then
              s2
            else
              begin
                let s1 =
                  match kind with
                  | Reference (src,_,_) ->
                      begin
                        let len = (min lim p_lim) - (max pos p_pos) in
                        try
                          String.sub src 0 len
                        with
                        | Invalid_argument _ ->
                            raise (Error.E Error.SD_structure)
                      end
                  | Chars offset ->
                      chars_part_to_string
                        (pos,lim)
                        (p_pos,p_lim)
                        offset
                        source
                in
                s2 ^ s1
              end)
          parts
          ("")

(** Return an integer value out of a given string data, return [None]
    if the given string data is not a valid representation of an integer,
    or if the integer represented exceeds the range of integers
    representable in OCaml's type [int]. *)
let to_int sd =
  let s = to_string sd in
  let s' =
    try
      if s.[0] = '+'
      then
        String.sub s 1 ((String.length s) -1)
      else
        s
    with
    | Invalid_argument _ ->
        s
  in
  try
    Some (int_of_string s')
  with
  | Failure _ -> None

(** Return a float value out of a given string data, return [None] if
    the given string is not a valid representation of a float. *)
let to_float sd =
  let s = to_string sd in
  try
    Some (float_of_string s)
  with
  | Failure _ -> None



(** Get the string data part corresponding to a given position *)
let get_part parts pos =
  let part,rest =
    Parts.partition
      (fun { p_position = p_pos; p_limit = p_lim } _ ->
        pos >= p_pos && pos < p_lim)
      parts
  in
  if Parts.cardinal part = 1
  then
    (Parts.choose part),rest
  else
    raise (Error.E Error.SD_structure)


(** Get the last limit in a given string data *)
let get_last_lim parts =
  try
    begin
      match Parts.max_binding parts with
      | { p_position = _; p_limit = p_lim},
        (Reference (_,_,offset) | Chars offset) ->
          Some (p_lim - offset)
    end
  with
  | Not_found -> None

(** Get the location corresponding to a given position *)
let get_loc parts pos =
  match snd (fst (get_part parts pos)) with
  | Reference (_,_,offset)
  | Chars offset ->
      pos - offset


(** Return the location of a given string data.
*)
let loc = function
  | { source = _;
      parts = _;
      exp_position = _;
      loc_position = loc;
      exp_limit = _;
      access = _ } ->
        loc

(** Return a string describing the location of a given string data. It
    might have raised [Invalid_argument] save for "character %i in %s"
    that seems well-formed.
    @raise Error.E [Error.F_wrong_printing_file_extension]
*)
let string_loc = function
  | { source = source;
      parts = _;
      exp_position = _;
      loc_position = loc;
      exp_limit = _;
      access = _ } ->
        let in_loc =
          match source with
          | String _ -> "string"
          | In_channel (_,in_file) ->
               (File_in.to_string in_file)
        in
        Format.sprintf "%s %i"
          in_loc
          loc

(** Access the character at a given position of a string data according
    to its parts
    @raise Error.E [Error.SD_structure]
 *)
let part_access parts access pos =
  let part,_ = get_part parts pos in
  match part with
  | { p_position = p_pos; p_limit = _ },
    Reference (s,_,_) ->
      begin
        try s.[pos - p_pos] with
        | Invalid_argument _ ->
            raise (Error.E Error.SD_structure)
      end
  | _,Chars offset ->
      access (pos - offset)

(** Return the first byte of a given string data and its remainder as
    a string data value. Return [None] if the string data is
    empty. This function does not check the encoding of the byte, it
    is meant to be passed as argument to the {!Utf8.next}
    function.
    @raise Error.E [Error.IO_input] because of [access]
    @raise Error.E [Error.SD_structure] because of [access]
*)
let next_fun = function
  | { source = source;
      parts = parts;
      exp_position = pos;
      loc_position = loc;
      exp_limit = lim;
      access = access } ->
        if pos = lim
        then
          None
        else
          let c =
            part_access parts access pos in
          let loc' =
            match fst (get_part parts pos) with
            | {p_position=_;p_limit=p_lim},Reference (_,len,_) ->
                if pos + 1 = p_lim
                then
                  loc + len
                else
                  loc
            | _,Chars _ ->
                loc + 1 in
          Some (c,
                { source = source;
                  parts = parts;
                  exp_position = pos + 1;
                  loc_position = loc';
                  exp_limit = lim;
                  access = access })

(** Return the first UTF-8 character of a given string data and its
    remainder as a string data value. Return [None] if the string data
    is empty. This function checks the validity of the UTF-8 character
    via the function {!Utf8.next}.
    @raise Error.E [Error.IO_input] because of [access]
    @raise Error.E [Error.SD_structure] because of [access]
    @raise Error.E [Error.UTF8_invalid]
    @raise Error.E [Error.UTF8_first_byte_invalid]
    @raise Error.E [Error.UTF8_overlong]
*)
let next_utf8 sd =
  Utf8.next next_fun sd


(** Return the first ASCII character of a given string data and its
    remainder as a string data value. Return [Some (None)] if the
    string data starts with a non-ascii character (a valid 2-, 3- or
    4-bytes UTF-8 character). Return [None] if the string data is
    empty. This function checks the validity of the ASCII character by
    checking if it is a valid 1-byte UTF-8 character via the function
    {!Utf8.next}.

    @raise Error.E [Error.IO_input] because of [access]
    @raise Error.E [Error.SD_structure] because of [access]
    @raise Error.E [Error.UTF8_invalid]
    @raise Error.E [Error.UTF8_first_byte_invalid]
    @raise Error.E [Error.UTF8_overlong]
*)
let next sd =
  match next_utf8 sd with
  | None -> None
  | Some ((Utf8.Byte1 c),sd') -> Some ((Some (c,sd')))
  | Some ((Utf8.Byte2 _ |Utf8.Byte3 _ |Utf8.Byte4 _),_) ->
      Some (None)

(** Test whether a given string data is empty. *)
let empty = function
  | { source = _;
      parts = _;
      exp_position = pos;
      loc_position = _;
      exp_limit = lim;
      access = _ } ->
        pos = lim


(** Return the subtraction of two given string data.
    @raise Error.E [Error.SD_subtraction_sources] if the two positions do not originate from the same source.
    @raise Error.E [Error.SD_subtraction_limits] if the two positions do not originate from the same string data.
    @raise Error.E [Error.SD_subtraction_positions] if the first position does not precede the second position.
    @raise Error.E [Error.SD_structure]
 *)
let sub
    { source = source1;
      parts = parts1;
      exp_position = pos1;
      loc_position = loc1;
      exp_limit = _;
      access = access1 }
    { source = source2;
      parts = parts2;
      exp_position = pos2;
      loc_position = loc2;
      exp_limit = _;
      access = _ } =
  if source1 == source2
  then
    if (get_last_lim parts1) >= (get_last_lim parts2)
        (* In most of the cases the limits should be equals but
           comments and CDSect ending a data may mean that the last
           limit in parts2 is lower than the one of part1. An
           alternative with a strict = would require to include the
           skipped parts in the types kind/parts.  *)
    then
      if loc1 <= loc2 || (loc1 = loc2 && pos1 <= pos2)
      then
        if Parts.cardinal parts1 <= Parts.cardinal parts2
        then
          let parts2' =
            Parts.filter
              (fun {p_position=p_pos;p_limit=p_lim} _ ->
                pos1 < p_lim && pos2 > p_pos )
              parts2
          in
          { source = source1;
            parts = parts2';
            exp_position = pos1;
            loc_position = loc1;
            exp_limit = pos2;
            access = access1 }
        else
          raise (Error.E Error.SD_structure)
      else
      raise (Error.E Error.SD_subtraction_positions)
    else
      raise (Error.E Error.SD_subtraction_limits)
  else
    raise (Error.E Error.SD_subtraction_sources)


(** Test whether the sub string data between two given positions is an
    empty string data.
    @raise Error.E [Error.SD_subtraction_sources] if the two positions do not originate from the same source.
 *)
let empty_sub
    { source = source1;
      parts = _;
      exp_position = pos1;
      loc_position = _;
      exp_limit = _;
      access = _ }
    { source = source2;
      parts = _;
      exp_position = pos2;
      loc_position = _;
      exp_limit = _;
      access = _ } =
  if source1 == source2
  then
    pos1 = pos2
  else
    raise (Error.E Error.SD_subtraction_sources)


(** Compare two string data values, returning [0] if they are equal,
    [-1] if the first is less than the second, and [1] if the first is
    greater than the second. The comparaison is based on the UTF-8
    byte comparaison function {!Utf8.compare_byte}. *)
let comp
    { source = source1;
      parts = parts1;
      exp_position = pos1;
      loc_position = _;
      exp_limit = lim1;
      access = access1 }
    { source = source2;
      parts = parts2;
      exp_position = pos2;
      loc_position = _;
      exp_limit = lim2;
      access = access2 } =
  if
    source1 == source2 &&
    pos1 = pos2 &&
    lim1 = lim2 &&
    Parts.cardinal parts1 <= Parts.cardinal parts2
  then
     0
  else
    begin
      let same_length = compare (lim1 - pos1) (lim2 - pos2) in
      let rec comp p1 p2 =
        if p1 = lim1 || p2 = lim2
        then same_length
        else
          match Utf8.compare_byte
              (part_access parts1 access1 p1)
              (part_access parts2 access2 p2) with
          | 0 -> comp (p1 + 1) (p2 + 1)
          | r -> r
      in
      comp pos1 pos2
    end

(** Test whether two string data values are equal. *)
let eq sd1 sd2 =
  match comp sd1 sd2 with
  | 0 -> true
  | _ -> false

(** Check if a given string data is UTF-8 valid and return it. *)
let check_utf8 sd =
  let rec aux sd1 =
    match next_utf8 sd1 with
      None -> sd
    | Some (_,sd2) ->
        aux sd2 in
  aux sd


(** [nexts start_sd sd] tests if a given string data [sd] starts with
    the same characters as another string data [start_sd] and returns
    the remainder. Test whether [start_sd] is UTF-8 valid. Return
    [None] if the test fails. *)
let nexts start_sd_unchecked sd =
  (* We first check if start_sd is UTF-8 valid *)
  let start_sd = check_utf8 start_sd_unchecked in
  let { source = _;
        parts = _;
        exp_position = start_pos;
        loc_position = _;
        exp_limit = start_lim;
        access = _ } = start_sd in
  let { source = source;
        parts = parts;
        exp_position = pos;
        loc_position = loc;
        exp_limit = lim;
        access = access } = sd in
  let start_len = start_lim - start_pos in
  let len = lim - pos in
  if start_len <= len
  then
    let sd_head = { source = source;
                    parts = parts;
                    exp_position = pos;
                    loc_position = loc;
                    exp_limit = pos + start_len;
                    access = access }
    in
    if eq start_sd sd_head
    then
      let new_pos = pos + start_len in
      let loc_shift =
        Parts.fold (fun {p_position=p_pos;p_limit=p_lim} k i ->
          if pos < p_lim && new_pos >= p_pos
          then
            match k with
            | Reference (_,len,_) ->
                if new_pos > p_lim
                then
                  i + len
                else
                  i
            | Chars _ ->
                i + (min new_pos p_lim) - (max pos p_pos)
          else i) parts 0 in
      Some { source = source;
             parts = parts;
             exp_position = new_pos;
             loc_position = loc + loc_shift;
             exp_limit = lim;
             access = access }
    else
      None
  else
    None


(** [match_regexp re_sd sd] tests whether a given string data [sd]
    matches exactly a given regular expression represented in a string
    data [re_sd]. The regular expression must not include the
    character ['\001'], this should never be the case because it is
    not a XML-UTF-8 valid character. *)
let match_regexp re_sd =
  let delim = "\001" in
  let re =
    try Str.regexp (delim ^ "\\(" ^ to_string re_sd ^ "\\)" ^ delim) with
    | Failure _ ->
        raise (Error.E Error.SD_building_regexp_failed)
  in
  (fun sd ->
    let s = delim ^ to_string sd ^ delim in
    let len = String.length s in
    let b =
      Str.string_match re s 0 in
    b && (Str.match_beginning () = 0) && (Str.match_end () = len))


(** Get a position within given parts and between two positions. *)
let get_existing_pos min max parts =
  let parts' =
    Parts.filter
      (fun {p_position=p_pos; p_limit=p_lim} _ ->
        min < p_lim && max >= p_pos) parts in
  try
    let {p_position=_;p_limit=p_lim},_ =
      Parts.max_binding parts' in
    if max > p_lim
    then Some p_lim
    else Some max
  with
  | Not_found ->
      None

(** [match_regexp_from re_sd sd] tests whether a given string data
    [sd] matches a given regular expression represented in a string
    data [re_sd]. The regular expression should:
    - be a repeating pattern as it gets applied repeatedly
    - should not match a pattern longer than 511 bytes.

    The function does not check the regular expression. Return the
    string data following the last matching part and [None] if the
    beginning of the string data does not match the regular
    expression. *)

let match_regexp_from re_sd =
  let re_repeat =
    try Str.regexp (to_string re_sd) with
    | Failure _ ->
        raise (Error.E Error.SD_building_regexp_failed)
  in
  let rec aux ({ source = source;
                 parts = parts;
                 exp_position = pos;
                 loc_position = loc;
                 exp_limit = lim;
                 access = access } as sd) =
    begin
      match
        get_existing_pos
          pos
          (if lim >= pos + 511
          then pos + 511
          else lim)
          parts
      with
      | None -> sd
      | Some lim' ->
        let sd' = { source = source;
                    parts = parts;
                    exp_position = pos;
                    loc_position = loc;
                    exp_limit = lim';
                    access = access } in
      let s = to_string sd' in
      let b = Str.string_match re_repeat s 0 in
      let start_at_beginning = Str.match_beginning () = 0 in
      let shift = Str.match_end () in
      let next_pos = pos + shift in
      let next_loc = loc + shift in
      let match_non_empty = next_pos > pos in
      if b && start_at_beginning && match_non_empty
      then
        let next_sd = { source = source;
                        parts = parts;
                        exp_position = next_pos;
                        loc_position = next_loc;
                        exp_limit = lim;
                        access = access } in
        let matched_sd = sub sd next_sd in
        let (_ : t) =
          (* the string matches but we need to check if the characters are
             UTF-8 valid *)
          check_utf8 matched_sd in
        aux next_sd
      else
        sd
    end
  in
  (fun sd ->
    let sd' = aux sd in
  if empty_sub sd' sd
  then None
  else Some sd')


(** Return the length in bytes of a given string data. *)
let byte_length = function
  | { source = _;
      parts = _;
      exp_position = pos;
      loc_position = _;
      exp_limit = lim;
      access = _ } ->
        lim - pos

(** Return the length in UTF-8 characters of a given string data.  *)
let utf8_length sd =
  let rec aux i sd1 =
    match next_utf8 sd1 with
    | None ->
        i
    | Some (_,sd2) ->
        aux (i+1) sd2
  in
  aux 0 sd

(** Update the offset of a data description. *)
let shift_kind shift = function
  | Reference (s,len,offset) ->
      Reference (s,len,offset + shift)
  | Chars (offset) ->
      Chars (offset + shift)

(** Type of marks applicable to a string data *)
type mark =
  | Mark_skip
  | Mark_reference of string
  | Mark_data


(** Function applying a given mark to a given string data. *)
let mark_fun mark { source = source;
                    parts = parts;
                    exp_position = pos;
                    loc_position = loc;
                    exp_limit = lim;
                    access = access } =
  let part,rest =
    get_part parts (if pos = lim then pos - 1 else pos) in
  if Parts.exists
      (fun { p_position = p_pos; p_limit = _ } _ ->
        p_pos > pos)
      rest
  then
    raise (Error.E Error.SD_structure);
  let p_pos,p_lim,kind,offset =
    match part with
    | { p_position = p_pos; p_limit = p_lim },(Reference (_,_,offset) as kind)
    | { p_position = p_pos; p_limit = p_lim },(Chars offset as kind) ->
        p_pos,p_lim,kind,offset
  in
  let parts',shift =
    match mark with
    | Mark_skip ->
        let len = pos - p_pos in
        (Parts.add
           { p_position=pos - len; p_limit=p_lim - len }
           (Chars (offset - len))
           rest),(- len)
    | Mark_reference s ->
        let shift = (String.length s) - (pos - p_pos) in
        let p' =
          Parts.add
            { p_position=p_pos; p_limit=pos + shift }
            (Reference (s, (pos - p_pos), offset))
            rest in
        (Parts.add
           { p_position=pos + shift; p_limit=p_lim + shift }
           (shift_kind shift kind)
           p'),shift
    | Mark_data ->
        let p' =
          Parts.add { p_position=p_pos; p_limit=pos } kind rest in
        (Parts.add
           { p_position=pos; p_limit=p_lim }
           (Chars offset)
           p'),0
  in
  { source = source;
    parts = parts';
    exp_position = pos + shift;
    loc_position = loc;
    exp_limit = lim + shift;
    access = access }


(** Apply the reference mark to a given string data. *)
let mark_reference refer sd =
  let s_refer = to_string refer in
  mark_fun
    (Mark_reference s_refer)
    sd

(** Apply the data mark to a given string data. *)
let mark_data sd =
  mark_fun
    Mark_data
    sd

(** Apply the skip mark to a given string data. *)
let mark_skip sd =
  mark_fun
    Mark_skip
    sd

(** Clear the marks of a given string data. *)
let clear_marks { source = source;
                  parts = parts;
                  exp_position = pos;
                  loc_position = loc;
                  exp_limit = lim;
                  access = access } =
  let _excluded_parts,included_parts =
    Parts.partition
      (fun { p_position = p_pos; p_limit = p_lim } _ ->
        pos >= p_lim || lim <= p_pos)
      parts
  in
  { source = source;
    parts = included_parts;
    exp_position = pos;
    loc_position = loc;
    exp_limit = lim;
    access = access }


(** Replace (using a string data reference mark) all occurrences of
    #x9 (tab), #xA (line feed) and #xD (carriage return)
    with #x20 (space). *)
let whitespace_replace
    { source = source;
      parts = parts;
      exp_position = pos;
      loc_position = loc;
      exp_limit = lim;
      access = access } =
  let parts' =
    Parts.fold
      (fun {p_position=p_pos; p_limit=p_lim} kind p ->
        let re =
          try Str.regexp "[\r\t\n]+" with
          | Failure _ ->
              raise (Error.E Error.SD_building_regexp_failed)
        in
        match kind with
        | Reference (s,len,offset) ->
            (* we replace the referenced string with a version
               containing only #x20 spaces *)
            let s' =
              Str.global_replace re " " s
            in
            Parts.add
              {p_position=p_pos; p_limit=p_lim}
              (Reference (s',len,offset))
              p
        | Chars offset ->
            let s =
              chars_part_to_string
                (pos,lim)
                (p_pos,p_lim)
                offset
                source
            in
            let split_result =
              Str.bounded_full_split re s 0 in
            let _,p' =
              List.fold_left
                (fun (position,p1) split_elem ->
                  let len,k =
                    match split_elem with
                    | Str.Text chars ->
                        let len = String.length chars in
                        len,(Chars offset)
                    | Str.Delim whites ->
                        let len = String.length whites in
                        let replaces_whites =
                          String.make len ' ' in
                        len,(Reference (replaces_whites,len,offset))
                  in
                  let limit = position + len in
                  limit,
                  (Parts.add
                     {p_position=position; p_limit=limit}
                     k
                     p1))
                (p_pos,p)
                split_result
            in p'
      )
      parts
      Parts.empty
  in
  { source = source;
    parts = parts';
    exp_position = pos;
    loc_position = loc;
    exp_limit = lim;
    access = access }


(** Return the last byte of a given string data and its head as
    a string data value. Return [None] if the string data is
    empty. This function does not check the encoding of the byte, it
    is meant to be passed as argument to the {!Utf8.next}
    function.
    @raise Error.E [Error.IO_input] because of [access]
    @raise Error.E [Error.SD_structure] because of [access]
*)
let prev_fun = function
  | { source = source;
      parts = parts;
      exp_position = pos;
      loc_position = loc;
      exp_limit = lim;
      access = access } ->
        if pos = lim
        then
          None
        else
          let lim' = lim -1 in
          let c =
            part_access parts access lim' in
          Some ({ source = source;
                  parts = parts;
                  exp_position = pos;
                  loc_position = loc;
                  exp_limit = lim';
                  access = access },
                c)



(** Collapse whitespaces: contiguous sequences of #x20's are collapsed
    to a single #x20, and any #x20 at the start or end of the string
    is then removed.  Collapsing performs a replacement first. A new
    string data is returned (localisations are lost).  *)

let whitespace_collapse initial_sd =
  let sd_w_replaced =
    whitespace_replace initial_sd in
  let sd_after_w =
    match match_regexp_from (of_string "[ ]*") sd_w_replaced with
    | None -> sd_w_replaced
    | Some sd' -> sd' in
  let rec prev sd =
    match prev_fun sd with
    | Some (sd' ,' ') -> prev sd'
    | None
    | Some (_, _) -> sd
  in
  let sd_after_before_w = prev sd_after_w in
  let s = to_string sd_after_before_w in
  let re =
    try Str.regexp "[ ]+" with
    | Failure _ ->
        raise (Error.E Error.SD_building_regexp_failed)
  in
  let split_result =
    Str.bounded_full_split re s 0 in
  let s' =
    List.fold_left
      (fun str split_elem ->
          match split_elem with
          | Str.Text chars ->
              str ^ chars
          | Str.Delim _ ->
              str ^ " ")
      ""
      split_result
  in
  of_string s'


(** [url_match sd url] tests whether a string data [sd] (containing
    the quoted name of a file) matches an URL (string data [url]). *)
let url_match quoted_sd url =
  let quoted_s = to_string quoted_sd in
  let url_s = to_string url in
  let base_s =
    try
      let i = String.rindex url_s '/' in
      String.sub url_s (i + 1) (String.length url_s - (i + 1))
    with
    | Not_found
    | Invalid_argument _ -> url_s
  in
  (* we add quotes as we compare with a quoted file name *)
  "'" ^ base_s ^ "'" = quoted_s


let to_code_escaped_string sd =
  let s = String.escaped (to_string sd) in
  let len = String.length s in
  let rec calc_len i ri =
    if i >= len
    then ri
    else if s.[i] >= '\032' && s.[i] <= '\126'
    then calc_len (i + 1) (ri + 1)
    else calc_len (i + 1) (ri + 4)
  in
  let rlen = calc_len 0 0 in
  let rs = String.make rlen ' ' in
  let rec pr_char i ri =
    if i >= len
    then rs
    else if s.[i] >= '\032' && s.[i] <= '\126'
    then
      begin
        rs.[ri] <- s.[i];
        pr_char (i + 1) (ri + 1)
      end
    else
      begin
        let n =
          "\\" ^ (Format.sprintf "%03i" (Char.code s.[i])) in
            String.blit n 0 rs ri 4;
          pr_char (i + 1) (ri + 4)
      end
  in
  pr_char 0 0
