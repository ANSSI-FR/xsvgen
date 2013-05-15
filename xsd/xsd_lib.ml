(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd_lib.ml 1686 2012-05-30 09:00:22Z maarek $ *)

(** {1 XSD library} *)

(** Return a string of a given value. *)
let to_string sd =
  "'" ^ Stringdata.to_string sd ^ "'"


(** {2 XSD modules map} *)

module M_composite =
  Map.Make
    (struct
      type t = Xsd.composition
      let compare
          (Xsd.Import (ns_opt_1,sloc_1))
          (Xsd.Import (ns_opt_2,sloc_2)) =
        match Stringdata.comp sloc_1 sloc_2 with
        | 0 ->
            if Lib.opts_test Stringdata.eq ns_opt_1 ns_opt_2
            then 0
            else Message.error_at sloc_1
                Error.GEN_imports_mismatch
        | n -> n
    end)


(** {2 Operations on raw values} *)

let to_ncname xml_lexer_is_ncname sd =
  if xml_lexer_is_ncname sd
  then Xsd.Ncname sd
  else Message.error_at sd
      (Error.XSDT_constructions_expected "NCname")

let to_id sd =
  Xsd.Id sd

let to_uri sd =
  if Xml_re.test_uri sd
  then
    Xsd.Uri sd
  else
    Message.error_at sd
      Error.XMLNS_XSDT_uri_not_well_formed


(** {2 Generic reading library} *)

let check_xsd_version = function
  | None ->
      Message.warning Error.XSDT_version_missing
  | Some version ->
      if Stringdata.eq Xsd_val.version_1_1 version
      then ()
      else
        if Stringdata.eq Xsd_val.version_1_0 version
        then
          Message.warning_at version Error.XSDT_version_1_0_not_supported
        else
          Message.warning_at version Error.XSDT_version_not_supported

let read_whiteSpace sd =
  let ws =
    Lib.list_opt_applies
      sd
      [ (fun v ->
        if Stringdata.eq Xsd_val.v_w_preserve v
        then Some Xsd.W_preserve
        else None);
        (fun v ->
          if Stringdata.eq Xsd_val.v_w_replace v
          then Some Xsd.W_replace
          else None);
        (fun v ->
          if Stringdata.eq Xsd_val.v_w_collapse v
          then Some Xsd.W_collapse
          else None) ]
  in
  match ws with
  | None ->
      Message.error_at sd
        (Error.XSDT_constructions_expected
           ("value " ^ to_string Xsd_val.v_w_preserve ^
            ", " ^ to_string Xsd_val.v_w_replace ^
            " or " ^ to_string Xsd_val.v_w_collapse))
  | Some w ->
      w

let read_boolean sd =
  let b =
    Lib.list_opt_applies
      sd
      [ (fun v ->
        if
          Stringdata.eq Xsd_val.v_true v ||
          Stringdata.eq Xsd_val.v_true_1 v
        then Some true
        else None);
        (fun v ->
          if
            Stringdata.eq Xsd_val.v_false v ||
            Stringdata.eq Xsd_val.v_false_0 v
          then Some false
          else None) ]
  in
  match b with
  | None ->
      Message.error_at sd
        (Error.XSDT_constructions_expected
           ("value " ^ to_string Xsd_val.v_true ^
            ", " ^ to_string Xsd_val.v_false ^
            ", " ^ to_string Xsd_val.v_true_1 ^
            " or " ^ to_string Xsd_val.v_false_0))
  | Some b ->
      b


let read_form = function
  | None ->
      None
  | Some sd ->
      if Stringdata.eq sd Xsd_val.v_qualified
      then
        Some Xsd.Qualified
      else if Stringdata.eq sd Xsd_val.v_unqualified
      then
        Some Xsd.Unqualified
      else
        Message.error_at sd
          (Error.XSDT_constructions_expected
             ("value " ^ to_string Xsd_val.v_qualified ^
              " or " ^ to_string Xsd_val.v_unqualified))

let read_use sd =
  let sd' = Stringdata.whitespace_collapse sd in
  if Stringdata.eq Xsd_val.v_optional sd'
  then Xsd.Optional
  else if Stringdata.eq Xsd_val.v_required sd'
  then Xsd.Required
  else if Stringdata.eq Xsd_val.v_prohibited sd'
  then Xsd.Prohibited
  else
    Message.error_at sd
      (Error.XSDT_constructions_expected
         ("value " ^ to_string Xsd_val.v_optional ^
          ", " ^ to_string Xsd_val.v_prohibited ^
          " or " ^ to_string Xsd_val.v_required))


let read_nc_variety sd =
  let sd' = Stringdata.whitespace_collapse sd in
  if Stringdata.eq Xsd_val.v_hhany sd'
  then Xsd.Any
  else if Stringdata.eq Xsd_val.v_hhother sd'
  then Xsd.Not
  else Xsd.Enumeration


let read_nc_list_item target sd =
  if Stringdata.eq Xsd_val.v_hhtargetNamespace sd
  then Some target
  else if Stringdata.eq Xsd_val.v_hhlocal sd
  then None
  else if Val_re.test_st_anyURI sd
  then Some (Some (Xsd.Uri sd))
  else
    Message.error_at sd
      (Error.XSDT_constructions_expected
         ("URI, value " ^ to_string Xsd_val.v_hhtargetNamespace ^
          " or " ^ to_string Xsd_val.v_hhlocal))

let read_processContents sd =
  let sd' = Stringdata.whitespace_collapse sd in
  if Stringdata.eq Xsd_val.v_strict sd'
  then Xsd.Strict
  else if Stringdata.eq Xsd_val.v_skip sd'
  then Xsd.Skip
  else if Stringdata.eq Xsd_val.v_lax sd'
  then Xsd.Lax
  else
    Message.error_at sd
      (Error.XSDT_constructions_expected
         ("value " ^ to_string Xsd_val.v_strict ^
          ", " ^ to_string Xsd_val.v_skip ^
          " or " ^ to_string Xsd_val.v_lax))


(** {2 Regular expression} *)

(** {3 Target regular expression type} *)

(** OCaml's Str internal regular expression syntax (minus group numbers) *)
type re_syntax =
  | Char of char
  | CharClass of charset * bool  (* true = complemented, false = normal *)
  | Seq of re_syntax list
  | Alt of re_syntax * re_syntax
  | Star of re_syntax
  | Plus of re_syntax
  | Option of re_syntax
  | Group of re_syntax
and charset =
  | Charset of range list
and range =
  | Range of char * char

(** {3 Predefined [Str] regular expressions} *)

(** Empty [Str] regular expression ([\[^\000-\255\]]) *)
let empty_str_re =
  Group
    (CharClass ((Charset [Range ('\000','\255')]),true))

(** {3 Predefined ranges} *)

let range_wildcard =
  Utf8.sub
    (Utf8.sub
       Utf8.full
       (Utf8.range_of_enc (Utf8.Byte1 '\n')))
    (Utf8.range_of_enc (Utf8.Byte1 '\r'))

let range_spaces =
  Utf8.union
    [ (Utf8.range_of_enc (Utf8.Byte1 ' '));
      (Utf8.range_of_enc (Utf8.Byte1 '\t'));
      (Utf8.range_of_enc (Utf8.Byte1 '\n'));
      (Utf8.range_of_enc (Utf8.Byte1 '\r')) ]


(** Specialisation function of {!Utf8.next}
    @raise Error.RE_UTF8_invalid (never because the lexer should already have noticed the invalid UTF-8 character) *)
let utf8_next sd =
  try Stringdata.next_utf8 sd with
  | Error.E Error.UTF8_invalid ->
      Message.error_at sd Error.RE_UTF8_invalid


(** {3 Parsing of XSD regular expressions} *)

let range_of_block sd = function
  | s ->
      Message.error_at sd (Error.RE_block_unknown s)


let rec next_block s sd =
  match Stringdata.next sd with
  | Some (Some (('a'..'z'|'A'..'Z'|'0'..'9'|'-') as c,sd')) ->
      next_block (s ^ (String.make 1 c)) sd'
  | None | Some _ ->
      if s = ""
      then
        Message.error_at sd (Error.RE_syntax "IsBlock")
      else
        (range_of_block sd s),sd

let next_charprop_descr sd =
  Message.warning Error.RE_LIM_charprop_not_implemented;
  match Stringdata.next sd with
  | Some (Some ('L',sd1)) ->
      begin
        match Stringdata.next sd1 with
        | Some (Some ('u',sd2)) -> Utf8.full,sd2
        | Some (Some ('l',sd2)) -> Utf8.full,sd2
        | Some (Some ('t',sd2)) -> Utf8.full,sd2
        | Some (Some ('m',sd2)) -> Utf8.full,sd2
        | Some (Some ('o',sd2)) -> Utf8.full,sd2
        | None | Some _ -> Utf8.full,sd1
      end
  | Some (Some ('M',sd1)) ->
      begin
        match Stringdata.next sd1 with
        | Some (Some ('n',sd2)) -> Utf8.full,sd2
        | Some (Some ('c',sd2)) -> Utf8.full,sd2
        | Some (Some ('e',sd2)) -> Utf8.full,sd2
        | Some _ | None -> Utf8.full,sd1
      end
  | Some (Some ('N',sd1)) ->
      begin
        match Stringdata.next sd1 with
        | Some (Some ('d',sd2)) -> Utf8.full,sd2
        | Some (Some ('l',sd2)) -> Utf8.full,sd2
        | Some (Some ('o',sd2)) -> Utf8.full,sd2
        | None | Some _ -> Utf8.full,sd1
      end
  | Some (Some ('P',sd1)) ->
      begin
        match Stringdata.next sd1 with
        | Some (Some ('c',sd2)) -> Utf8.full,sd2
        | Some (Some ('d',sd2)) -> Utf8.full,sd2
        | Some (Some ('s',sd2)) -> Utf8.full,sd2
        | Some (Some ('e',sd2)) -> Utf8.full,sd2
        | Some (Some ('i',sd2)) -> Utf8.full,sd2
        | Some (Some ('f',sd2)) -> Utf8.full,sd2
        | Some (Some ('o',sd2)) -> Utf8.full,sd2
        | None | Some _ -> Utf8.full,sd1
      end
  | Some (Some ('Z',sd1)) ->
      begin
        match Stringdata.next sd1 with
        | Some (Some ('s',sd2)) -> Utf8.full,sd2
        | Some (Some ('l',sd2)) -> Utf8.full,sd2
        | Some (Some ('p',sd2)) -> Utf8.full,sd2
        | None | Some _ -> Utf8.full,sd1
      end
  | Some (Some ('S',sd1)) ->
      begin
        match Stringdata.next sd1 with
        | Some (Some ('m',sd2)) -> Utf8.full,sd2
        | Some (Some ('c',sd2)) -> Utf8.full,sd2
        | Some (Some ('k',sd2)) -> Utf8.full,sd2
        | Some (Some ('o',sd2)) -> Utf8.full,sd2
        | None | Some _ -> Utf8.full,sd1
      end
  | Some (Some ('C',sd1)) ->
      begin
        match Stringdata.next sd1 with
        | Some (Some ('c',sd2)) -> Utf8.full,sd2
        | Some (Some ('f',sd2)) -> Utf8.full,sd2
        | Some (Some ('o',sd2)) -> Utf8.full,sd2
        | Some (Some ('n',sd2)) -> Utf8.full,sd2
        | None | Some _ -> Utf8.full,sd1
      end
  | Some (Some ('I',sd1)) ->
      begin
        match Stringdata.next sd1 with
        | Some (Some ('s',sd2)) ->
            next_block "" sd2
        | None | Some _ ->
            Message.error_at sd (Error.RE_syntax "IsBlock")
      end
  | None | Some _ ->
      Message.error_at sd (Error.RE_syntax "charProp")


let next_charprop sd =
  match Stringdata.next sd with
  | Some (Some ('{',sd1)) ->
      let range,sd2 =
        next_charprop_descr sd1 in
      begin
        match Stringdata.next sd2 with
        | Some (Some ('}',sd3)) ->
            range,sd3
        | None | Some _ ->
            Message.error_at sd (Error.RE_syntax "charProp")
      end
  | None | Some _ ->
      Message.error_at sd (Error.RE_syntax "charProp")

let next_char_escape sd =
  match Stringdata.next sd with
  | Some (Some ('\\',sd')) ->
      begin
        match Stringdata.next sd' with
        | None ->
            Message.error_at sd (Error.RE_syntax "charEsc")
        | Some (Some ('n',sd'')) ->
            (Utf8.Byte1 '\n'),sd''
        | Some (Some ('r',sd'')) ->
            (Utf8.Byte1 '\r'),sd''
        | Some (Some ('t',sd'')) ->
            (Utf8.Byte1 '\t'),sd''
        | Some (Some ('\\' as c,sd''))
        | Some (Some ('|' as c,sd''))
        | Some (Some ('.' as c,sd''))
        | Some (Some ('?' as c,sd''))
        | Some (Some ('*' as c,sd''))
        | Some (Some ('+' as c,sd''))
        | Some (Some ('(' as c,sd''))
        | Some (Some (')' as c,sd''))
        | Some (Some ('{' as c,sd''))
        | Some (Some ('}' as c,sd''))
        | Some (Some ('-' as c,sd''))
        | Some (Some ('[' as c,sd''))
        | Some (Some (']' as c,sd''))
        | Some (Some ('^' as c,sd'')) ->
            (Utf8.Byte1 c),sd''
        | Some _ ->
            Message.error_at sd' (Error.RE_syntax "charEsc")
      end
  | None | Some _ ->
      Message.error_at sd (Error.RE_syntax "charEsc")

let next_class_escape sd =
  match Stringdata.next sd with
  | Some (Some ('\\',sd')) ->
      begin
        match Stringdata.next sd' with
        | None ->
            Message.error_at sd (Error.RE_syntax "classEsc")
        | Some (Some ('p',sd'')) ->
            let range,sd3 =
              next_charprop sd'' in
            range,sd3
        | Some (Some ('P',sd'')) ->
            let range,sd3 =
              next_charprop sd'' in
            (Utf8.compl range),sd3
        | Some (Some ('s',sd'')) ->
            range_spaces,sd''
        | Some (Some ('S',sd'')) ->
            (Utf8.compl range_spaces),sd''
        | Some (Some ('i',sd'')) ->
            (Xml_charsets.range_NameStartChar_mDisc),sd''
        | Some (Some ('I',sd'')) ->
            (Utf8.compl Xml_charsets.range_NameStartChar_mDisc),sd''
        | Some (Some ('c',sd'')) ->
            (Xml_charsets.range_NameChar_mDisc),sd''
        | Some (Some ('C',sd'')) ->
            (Utf8.compl Xml_charsets.range_NameChar_mDisc),sd''
        | Some (Some ('d',sd'')) ->
            Message.warning Error.RE_LIM_charprop_not_implemented;
            Utf8.full,sd''
        | Some (Some ('D',sd'')) ->
            Message.warning Error.RE_LIM_charprop_not_implemented;
            Utf8.full,sd''
        | Some (Some ('w',sd'')) ->
            Message.warning Error.RE_LIM_charprop_not_implemented;
            Utf8.full,sd''
        | Some (Some ('W',sd'')) ->
            Message.warning Error.RE_LIM_charprop_not_implemented;
            Utf8.full,sd''
        | Some _ ->
            Message.error_at sd' (Error.RE_syntax "classEsc")
      end
  | None | Some _ ->
      Message.error_at sd (Error.RE_syntax "classEsc")


let next_normalchar sd =
  match Stringdata.next sd with
  | Some (Some (')',_)) -> None,sd
  | None
  | Some (Some (('.'|'\\'|'?'|'*'|'+'|'{'|'}'|'('|'|'|'['|']'),_)) ->
      Message.error_at sd (Error.RE_syntax "normalChar")
  | _ ->
      match utf8_next sd with
      | None ->
          Message.error_at sd (Error.RE_syntax "normalChar")
      | Some (u,sd') ->
          Some ((Xsd.RE_atom (Utf8.range_of_enc u))),sd'

let rec next_quantity_min_or_range i s sd =
  match Stringdata.next sd with
  | Some (Some ('0'..'9' as c,sd')) ->
      next_quantity_min_or_range i (s ^ (String.make 1 c)) sd'
  | Some (Some ('}',sd')) ->
      if s = ""
      then
        (Xsd.RE_min i),sd'
      else
        begin
          try (Xsd.RE_range (i,int_of_string s)),sd' with
          | Failure _ (* "int_of_string" *) ->
              Message.error_at sd' (Error.RE_syntax "quantity_min_or_range")
        end
  | None | Some _ ->
      Message.error_at sd (Error.RE_syntax "quantity_min_or_range")

let rec next_quantity s sd =
  match Stringdata.next sd with
  | Some (Some ('0'..'9' as c,sd')) ->
      next_quantity (s ^ (String.make 1 c)) sd'
  | Some (Some ('}',sd')) ->
      begin
        try (Xsd.RE_exact (int_of_string s)),sd' with
        | Failure _ (* "int_of_string" *) ->
            Message.error_at sd' (Error.RE_syntax "quantity")
      end
  | Some (Some (',',sd')) ->
      begin
        let i =
          try int_of_string s with
          | Failure _ (* "int_of_string" *) ->
              Message.error_at sd' (Error.RE_syntax "quantity") in
        next_quantity_min_or_range i "" sd'
      end
  | None | Some _ ->
      Message.error_at sd (Error.RE_syntax "quantity")


let next_quantifier sd =
  match Stringdata.next sd with
  | Some (Some ('?',sd')) ->
      (Some Xsd.RE_none_or_one),sd'
  | Some (Some ('+',sd')) ->
      (Some Xsd.RE_one_or_more),sd'
  | Some (Some ('*',sd')) ->
      (Some Xsd.RE_none_or_more),sd'
  | Some (Some ('{',sd')) ->
      let q,sd'' = next_quantity "" sd' in
      (Some (Xsd.RE_quantity q)),sd''
  | None | Some _ -> None,sd


let next_singlechar sd =
  match Stringdata.next sd with
  | Some (Some ('\\',_)) ->
      next_char_escape sd
  | _ ->
      match utf8_next sd with
      | Some (c,sd') ->
          (c,sd')
      | _ ->
          Message.error_at sd (Error.RE_syntax "singleChar")

let is_class_escape sd =
  match Stringdata.next sd with
  | Some (Some (('p'|'P'|'s'|'S'|'i'|'I'|'c'|'C'|'d'|'D'|'w'|'W'),_)) -> true
  | _ -> false

let next_chargroup_part sd =
  match Stringdata.next sd with
  | Some (Some ('\\',sd')) when is_class_escape sd' ->
      next_class_escape sd
  | Some _ ->
      let c1,sd1 =
        next_singlechar sd in
      begin
        match Stringdata.next sd1 with
        | Some (Some ('-',sd2)) ->
            let c2,sd3 =
              next_singlechar sd2 in
            (Utf8.range_of_enc_range c1 c2),sd3
        | _ ->
            (Utf8.range_of_enc c1),sd1
      end
  | None ->
      Message.error_at sd (Error.RE_syntax "charGroupPart")


let rec next_chargroup sd =
  let rec next_one_chargroup range_l sd_aux =
    match Stringdata.next sd_aux with
    | Some (Some ((']'|'-'),_)) ->
        begin
          match range_l with
          | [] ->
              Message.error_at sd_aux (Error.RE_syntax "charGroup")
          | range_l ->
              (Utf8.union range_l),sd_aux
        end
    | Some _ ->
        let range,sd_aux' =
          next_chargroup_part sd_aux in
        next_one_chargroup (range::range_l) sd_aux'
    | None ->
        Message.error_at sd_aux (Error.RE_syntax "charGroup") in
  next_one_chargroup [] sd

and next_group_descr sd =
  let range,sd' =
    match Stringdata.next sd with
    | Some (Some ('^',sd1)) ->
        let range,sd2 =
          next_chargroup sd1 in
        (Utf8.compl range),sd2
    | Some _ ->
        next_chargroup sd
    | None ->
        Message.error_at sd (Error.RE_syntax "charGroup")
  in
  match Stringdata.next sd' with
  | Some (Some ('-',sd2)) ->
      let range_sub,sd3 =
        next_group sd2 in
      (Utf8.sub range range_sub),sd3
  | _ ->
      range,sd'

and next_group sd =
  match Stringdata.next sd with
  | Some (Some ('[',sd1)) ->
      let range,sd2 =
        next_group_descr sd1 in
      begin
        match Stringdata.next sd2 with
        | Some (Some (']',sd3)) ->
            range,sd3
        | None | Some _ ->
            Message.error_at sd (Error.RE_syntax "charClassExpr")
      end
  | None | Some _ ->
      Message.error_at sd (Error.RE_syntax "charClassExpr")


let rec next_atom sd =
  match Stringdata.next sd with
  | Some (Some ('(',sd1)) ->
      let regexp,sd2 = next_regexp [] sd1 in
      begin
        match Stringdata.next sd2 with
        | Some (Some (')',sd3)) ->
            (Some (Xsd.RE_atom_re regexp)),sd3
        | _ ->
            Message.error_at sd2 (Error.RE_syntax "next_piece")
      end
  | Some (Some ('.',sd')) ->
      (Some (Xsd.RE_atom range_wildcard)),sd'
  | Some (Some ('\\',sd')) when is_class_escape sd' ->
      let range,sd'' =
        next_class_escape sd in
      (Some (Xsd.RE_atom range)),sd''
  | Some (Some ('\\',_)) ->
      let c,sd' =
        next_char_escape sd in
      (Some (Xsd.RE_atom (Utf8.range_of_enc c))),sd'
  | Some (Some ('[',_)) ->
      let range,sd' =
        next_group sd in
      (Some (Xsd.RE_atom range)),sd'
  | None | Some (Some ('|',_)) ->
      None,sd
  | Some _ ->
      next_normalchar sd

and next_piece sd =
  match next_atom sd with
  | None,_ -> None,sd
  | (Some a),sd' ->
      let q,sd'' = next_quantifier sd' in
      (Some (Xsd.RE_piece (a,q))),sd''

and next_branch l sd =
  match next_piece sd with
  | (Some piece),sd' ->
      next_branch (piece::l) sd'
  | None,_ ->
      (Xsd.RE_branch (List.rev l)),sd

and next_regexp l sd =
  match Stringdata.next sd with
  | Some (Some ('|',sd')) ->
      if l = []
      then
        Message.error_at sd (Error.RE_syntax "regExp")
      else
        let branch,sd'' = next_branch [] sd' in
        next_regexp (branch::l) sd''
  | Some _ ->
      if l = []
      then
        let branch,sd'' = next_branch [] sd in
        next_regexp (branch::l) sd''
      else
        Xsd.RE_regexp(List.rev l),sd
  | None ->
      Xsd.RE_regexp(List.rev l),sd

let read_pattern sd : Xsd.regexp =
  let regexp,sd' =
    next_regexp [] sd in
  if Stringdata.empty sd'
  then regexp
  else Message.error_at sd' (Error.RE_syntax "regExp")

(** {3 Transformation from XSD's to OCaml's regular expressions} *)

let range_of_range : 'a Utf8.char_range -> range = function
  | Utf8.R (c1,c2,_) ->
      Range (c1,c2)

let charclass_of_range range : re_syntax =
  CharClass (Charset ([range_of_range range]),false)

let charset_of_range l =
  Charset (List.map range_of_range l)

let str_re_of_byte1_ranges = function
  | [] ->
      empty_str_re
  | l ->
      CharClass
        ((charset_of_range l),false)

let str_re_of_ranges str_re_of_range l : re_syntax =
  let opt =
    List.fold_left
      (fun str_re_opt range ->
        let str_re_2 : re_syntax = str_re_of_range range in
        match str_re_opt with
        | None ->
            Some str_re_2
        | Some str_re ->
            Some (Alt (str_re,str_re_2)))
      None
      l
  in
  match opt with
  | None -> empty_str_re
  | Some re -> re

let str_re_of_byte2_range = function
  | Utf8.R (_,_,(Utf8.B2 byte1_ranges)) as range->
      Group (Seq ( (charclass_of_range range) ::
                   [str_re_of_byte1_ranges byte1_ranges] ))

let str_re_of_byte2_ranges l =
  str_re_of_ranges str_re_of_byte2_range l

let str_re_of_byte3_range = function
  | Utf8.R (_,_,(Utf8.B3 byte2_ranges)) as range->
      Group (Seq ( (charclass_of_range range) ::
                   [str_re_of_byte2_ranges byte2_ranges] ))

let str_re_of_byte3_ranges l =
  str_re_of_ranges str_re_of_byte3_range l


let str_re_of_byte4_range = function
  | Utf8.R (_,_,(Utf8.B4 byte3_ranges)) as range->
      Group (Seq ( (charclass_of_range range) ::
                   [str_re_of_byte3_ranges byte3_ranges] ))

let str_re_of_byte4_ranges l =
  str_re_of_ranges str_re_of_byte4_range l


let str_re_of_single_byte_range = function
  | { Utf8.byte1 = l;
      Utf8.byte2 = [];
      Utf8.byte3 = [];
      Utf8.byte4 = [] } ->
        str_re_of_byte1_ranges l
  | { Utf8.byte1 = _;
      Utf8.byte2 = _;
      Utf8.byte3 = _;
      Utf8.byte4 = _ } ->
        Message.error (Error.A_function "Xsd_lib.str_re_of_single_byte_range")

let alt (re1,re2) =
  if re1 = empty_str_re
  then re2
  else if re2 = empty_str_re
  then re1
  else Alt(re1,re2)

let str_re_of_range = function
  | { Utf8.byte1 = l1;
      Utf8.byte2 = l2;
      Utf8.byte3 = l3;
      Utf8.byte4 = l4 } ->
        alt ((str_re_of_byte1_ranges l1),
             (alt ((str_re_of_byte2_ranges l2),
                   (alt ((str_re_of_byte3_ranges l3),
                         (str_re_of_byte4_ranges l4))))))


let group_range range =
  match range with
  | Char _ | CharClass _ | Group _ as g -> g
  | Seq _ | Alt _ | Star _ | Plus _
  | Option _ ->
      Group range

let quantity_range range re_quantity =
  let range' = group_range range in
  match re_quantity with
  | Xsd.RE_range (min,max) ->
      Seq [Seq (Lib.list_make min range');
           Lib.list_make_fold
             (max-min)
             (fun sub -> Option (Seq [range';sub]))
             (Seq []) ]
  | Xsd.RE_min min ->
      Seq [Seq (Lib.list_make min range');
           Star range']
  | Xsd.RE_exact n ->
      Seq (Lib.list_make n range')

let rec str_re_of_regexp (Xsd.RE_regexp re_regexp) : re_syntax =
  let rec aux = function
    | [] ->
        Seq []
    | [h] ->
        str_re_of_branch h
    | h::t ->
        Alt ((str_re_of_branch h),(aux t))
  in
  aux re_regexp

and str_re_of_branch : Xsd.re_branch -> re_syntax = function
  | Xsd.RE_branch l ->
    Seq (List.map str_re_of_piece l)

and str_re_of_piece : Xsd.re_piece -> re_syntax = function
  | Xsd.RE_piece (atom,None) ->
      str_re_of_atom atom
  | Xsd.RE_piece (atom,Some Xsd.RE_none_or_one) ->
      Option (str_re_of_atom atom)
  | Xsd.RE_piece (atom,Some Xsd.RE_one_or_more) ->
      Plus (str_re_of_atom atom)
  | Xsd.RE_piece (atom,Some Xsd.RE_none_or_more) ->
      Star (str_re_of_atom atom)
  | Xsd.RE_piece (atom,Some (Xsd.RE_quantity re_quantity)) ->
      quantity_range (str_re_of_atom atom) re_quantity

and str_re_of_atom : Xsd.re_atom -> re_syntax = function
  | Xsd.RE_atom range ->
      if Utf8.test_single_byte_range range
      then
        str_re_of_single_byte_range range
      else
        Group (str_re_of_range range)
  | Xsd.RE_atom_re re ->
      Group (str_re_of_regexp re)

(** {3 Printing of OCaml regular expressions} *)


(** Sort characters of a range list to move special [Str] characters:
   - [-] (code 045) is moved at the end;
   - [\]] (code 093) is moved to in front;
   - [^] (code 094) is moved to the end or to the penultimate position
     if [-] is present.
Note that the character [^] could never appear in front as it is
either preceded by a range or, if it is a single character of range
list, it is printed by {!single_string_of_char} (see
{!string_of_re_syntax}).

 *)
let sort_str_chars_charset range_list =
  let filter_str_chars (char045,char093,char094) = function
    | Range('\045','\045') -> (true   ,char093,char094),None
    | Range('\045',c     ) -> (true   ,char093,char094),Some (Range('\046',c))
    | Range(c     ,'\045') -> (true   ,char093,char094),Some (Range(c,'\044'))
    | Range('\093','\093') -> (char045,true   ,char094),None
    | Range('\093','\094') -> (char045,true   ,true   ),None
    | Range('\093',c     ) -> (char045,true   ,true   ),Some (Range('\095',c))
    | Range(c     ,'\093') -> (char045,true   ,char094),Some (Range(c,'\092'))
    | Range('\094',c     ) -> (char045,char093,true   ),Some (Range('\095',c))
    | Range(c     ,'\094') -> (char045,char093,true   ),Some (Range(c,'\092'))
    | Range(_,_) as r      -> (char045,char093,char094),Some r
  in
  let rec aux charset (char045,char093,char094) = function
    | [] ->
        if charset = [] && not char093 && char094 && char045
        then
          [Range ('-','-'); Range('^','^')]
        else
          (if char093 then [Range (']',']')] else []) @
          charset @
          (if char094 then [Range ('^','^')] else []) @
          (if char045 then [Range ('-','-')] else [])
    | h::t ->
        let (char045',char093',char094'),opt =
          filter_str_chars (char045,char093,char094) h in
        aux
          (match opt with None -> charset | Some r -> r :: charset)
          (char045',char093',char094')
          t
  in
  aux [] (false,false,false) range_list

let test_single_char_only = function
  | [Range(c1,c2)] ->
      (c1 = c2)
  | [] | _::_::_ -> false

let rec string_of_re_syntax = function
  | Char c -> String.make 1 c
  | CharClass ((Charset range_list), true) ->
      let l = sort_str_chars_charset range_list in
      "[^" ^
      (List.fold_left (fun s r -> s ^ (string_of_range r) ) "" l) ^
      "]"
  | CharClass ((Charset range_list), false) ->
      let l = sort_str_chars_charset range_list in
      if test_single_char_only l
      then
        List.fold_left (fun s r -> s ^ (single_string_of_range r) ) "" l
      else
        "[" ^
        (List.fold_left (fun s r -> s ^ (string_of_range r) ) "" l) ^
        "]"
  | Seq [] -> ""
  | Seq (re::re_l) ->
      (string_of_re_syntax (group_range re)) ^ (string_of_re_syntax (Seq re_l))
  | Alt (re_1, re_2) ->
      (string_of_re_syntax re_1) ^ "\\|" ^ (string_of_re_syntax re_2)
  | Star re -> (string_of_re_syntax (group_range re)) ^ "*"
  | Plus re -> (string_of_re_syntax (group_range re)) ^ "+"
  | Option re -> (string_of_re_syntax (group_range re)) ^ "?"
  | Group (Group re) -> string_of_re_syntax (Group re)
  | Group re -> "\\(" ^ (string_of_re_syntax re) ^ "\\)"
and string_of_range = function
  | Range (c1,c2) ->
      if c1 = c2
      then (string_of_char c1)
      else (string_of_char c1) ^ "-" ^ (string_of_char c2)
and single_string_of_range = function
  | Range (c1,c2) ->
      if c1 = c2
      then single_string_of_char c1
      else Message.error  (Error.A_function "Xsd_lib.single_string_of_range")
and string_of_char = function
  | '\n' -> "\n"
  | '\t' -> "\t"
  | '$' | '^' | '.' | '*' | '+' | '?' as c ->
      String.make 1 c
  | c -> Str.quote (String.make 1 c)
and single_string_of_char = function
  | '\n' -> "\n"
  | '\t' -> "\t"
  | '$' | '^' | '.' | '*' | '+' | '?' | '[' | ']' as c ->
      "\\" ^ (String.make 1 c)
  | c -> Str.quote (String.make 1 c)

let regexp_of_regexp ( regexp : Xsd.regexp) : Lxsd.regexp =
  let str_re = str_re_of_regexp regexp in
  let sd_re = Stringdata.of_string (string_of_re_syntax str_re) in
  Message.info (Message.INFO_pattern sd_re);
  (* Testing whether the Str regexp string compiles,
     if it fails an error is emitted by Stringdata. *)
  let _ = Stringdata.match_regexp sd_re in
  Lxsd.RegExp sd_re


(** {2 Reading values} *)


(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Failure ["int_of_string"]
    @raise Error.E [Error.XSDT_constructions_expected]
*)
let to_posint sd =
  match Stringdata.to_int sd with
  | Some i -> i
  | None ->
      Message.error_at sd
        (Error.XSDT_constructions_expected "positive integer")


(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Failure ["int_of_string"]
    @raise Error.E [Error.XSDT_constructions_expected]
*)
let to_posint_unbounded sd =
  if Stringdata.eq sd Xsd_val.v_unbounded
  then
    Xsd.Unbounded
  else
    try
      (* We use int_of_string as "+*" is not allowed here contrary
         to XSD's integers *)
      Xsd.Occur (int_of_string (Stringdata.to_string sd))
    with
      Failure _ ->
        Message.error_at sd
          (Error.XSDT_constructions_expected
             ("positive integer or " ^
              to_string Xsd_val.v_unbounded))



let to_nonNegativeInteger sd =
  let i =
    match Stringdata.to_int sd with
    | Some i -> i
    | None ->
        Message.error_at sd
          (Error.XSDT_constructions_expected "non negative integer")
  in
  if i > 0
  then i
  else
    Message.error_at sd
      (Error.XSDT_constructions_expected "non negative integer")



(** {2 Default facet details} *)

(** Empty item facets record. *)
let empty_item_facets : Lxsd.item_facets =
  { Lxsd.f_length = None;
    Lxsd.f_pattern = [];
    Lxsd.f_enumeration = None;
    Lxsd.f_value = None }

(** Empty list facets record. *)
let empty_list_facets : Lxsd.list_facets =
  { Lxsd.f_length = None;
    Lxsd.f_pattern = [];
    Lxsd.f_enumeration = None;
    Lxsd.f_value = None }

(** Empty whitespace item facet. *)
let empty_item_whitespace =
  Lxsd.W_preserve

(** Empty whitespace list facet. *)
let empty_list_whitespace =
  Lxsd.W_collapse

(** {2 Built-in facet details} *)


(** Get the facet details of a built-in simple type. *)
let get_built_in_st_details = function
  | Lxsd.STD_anySimpleType
  | Lxsd.STD_anyAtomicType ->
      { Lxsd.lstd_whitespace = empty_item_whitespace;
        Lxsd.lstd_members = [Lxsd.LSTD_atomic empty_item_facets] }
  | Lxsd.STD_primitive_type primitive_type ->
      begin
        match primitive_type with
        | Lxsd.PT_string ->
            { Lxsd.lstd_whitespace = Lxsd.W_preserve;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern = [Lxsd.RE_primitive_type primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.PT_boolean ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern = [Lxsd.RE_primitive_type primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.PT_decimal ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern = [Lxsd.RE_primitive_type primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.PT_float ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern = [Lxsd.RE_primitive_type primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.PT_double ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern = [Lxsd.RE_primitive_type primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.PT_duration
        | Lxsd.PT_dateTime
        | Lxsd.PT_time
        | Lxsd.PT_date
        | Lxsd.PT_gYearMonth
        | Lxsd.PT_gYear
        | Lxsd.PT_gMonthDay
        | Lxsd.PT_gDay
        | Lxsd.PT_gMonth ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern = [Lxsd.RE_primitive_type primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.PT_hexBinary ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern = [Lxsd.RE_primitive_type primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.PT_base64Binary ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern = [Lxsd.RE_primitive_type primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.PT_anyURI ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern = [Lxsd.RE_primitive_type primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.PT_QName ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern = [Lxsd.RE_primitive_type primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.PT_NOTATION ->
            Message.error Error.XSDL_XSV_LIM_notation_not_implemented
      end
  | Lxsd.STD_non_primitive_type non_primitive_type ->
      begin
        match non_primitive_type with
        | Lxsd.NPT_normalizedString ->
            { Lxsd.lstd_whitespace = Lxsd.W_replace;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.NPT_token
        | Lxsd.NPT_language
        | Lxsd.NPT_NMTOKEN
        | Lxsd.NPT_Name
        | Lxsd.NPT_NCName
        | Lxsd.NPT_ID
        | Lxsd.NPT_IDREF
        | Lxsd.NPT_ENTITY
        | Lxsd.NPT_yearMonthDuration
        | Lxsd.NPT_dayTimeDuration
        | Lxsd.NPT_dateTimeStamp ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.NPT_NMTOKENS ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_list ({ Lxsd.f_length =
                                  Some (Lxsd.Bounds ((Some 1),None));
                                  Lxsd.f_pattern = [];
                                  Lxsd.f_enumeration = None;
                                  Lxsd.f_value = None },
                                { Lxsd.f_length = None;
                                  Lxsd.f_pattern =
                                  [Lxsd.RE_non_primitive_type Lxsd.NPT_NMTOKEN];
                                  Lxsd.f_enumeration = None;
                                  Lxsd.f_value = None }) ] }
        | Lxsd.NPT_IDREFS ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_list ({ Lxsd.f_length =
                                  Some (Lxsd.Bounds ((Some 1),None));
                                  Lxsd.f_pattern = [];
                                  Lxsd.f_enumeration = None;
                                  Lxsd.f_value = None },
                                { Lxsd.f_length = None;
                                  Lxsd.f_pattern =
                                  [Lxsd.RE_non_primitive_type Lxsd.NPT_IDREF];
                                  Lxsd.f_enumeration = None;
                                  Lxsd.f_value = None }) ] }
        | Lxsd.NPT_ENTITIES ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_list ({ Lxsd.f_length =
                                  Some (Lxsd.Bounds ((Some 1),None));
                                  Lxsd.f_pattern = [];
                                  Lxsd.f_enumeration = None;
                                  Lxsd.f_value = None },
                                { Lxsd.f_length = None;
                                  Lxsd.f_pattern =
                                  [Lxsd.RE_non_primitive_type Lxsd.NPT_ENTITY];
                                  Lxsd.f_enumeration = None;
                                  Lxsd.f_value = None }) ] }
        | Lxsd.NPT_integer ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value = None } ] }
        | Lxsd.NPT_nonPositiveInteger ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([],
                          [Lxsd.Inclusive (Stringdata.of_string "0")])) } ] }
        | Lxsd.NPT_negativeInteger ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([],
                          [Lxsd.Inclusive (Stringdata.of_string "-1")])) } ] }
        | Lxsd.NPT_long ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([Lxsd.Inclusive
                             (Stringdata.of_string
                                "-9223372036854775808")],
                          [Lxsd.Inclusive
                             (Stringdata.of_string
                                "9223372036854775807")])) } ] }
        | Lxsd.NPT_int ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([Lxsd.Inclusive
                             (Stringdata.of_string  "-2147483648")],
                          [Lxsd.Inclusive
                             (Stringdata.of_string "2147483647")])) } ] }
        | Lxsd.NPT_short ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([Lxsd.Inclusive
                             (Stringdata.of_string  "-32768")],
                          [Lxsd.Inclusive
                             (Stringdata.of_string "32767")])) } ] }
        | Lxsd.NPT_byte ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([Lxsd.Inclusive
                             (Stringdata.of_string  "-128")],
                          [Lxsd.Inclusive
                             (Stringdata.of_string "127")])) } ] }
        | Lxsd.NPT_nonNegativeInteger ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([Lxsd.Inclusive
                             (Stringdata.of_string  "0")],
                          [])) } ] }
        | Lxsd.NPT_unsignedLong ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([Lxsd.Inclusive
                             (Stringdata.of_string
                                "0")],
                          [Lxsd.Inclusive
                             (Stringdata.of_string
                                "18446744073709551615")])) } ] }
        | Lxsd.NPT_unsignedInt ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([Lxsd.Inclusive
                             (Stringdata.of_string "0")],
                          [Lxsd.Inclusive
                             (Stringdata.of_string "4294967295")])) } ] }
        | Lxsd.NPT_unsignedShort ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([Lxsd.Inclusive
                             (Stringdata.of_string "0")],
                          [Lxsd.Inclusive
                             (Stringdata.of_string "65535")])) } ] }
        | Lxsd.NPT_unsignedByte ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([Lxsd.Inclusive
                             (Stringdata.of_string "0")],
                          [Lxsd.Inclusive
                             (Stringdata.of_string "255")])) } ] }
        | Lxsd.NPT_positiveInteger ->
            { Lxsd.lstd_whitespace = Lxsd.W_collapse;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic
                  { Lxsd.f_length = None;
                    Lxsd.f_pattern =
                    [Lxsd.RE_non_primitive_type non_primitive_type];
                    Lxsd.f_enumeration = None;
                    Lxsd.f_value =
                    Some
                      (Lxsd.Order_int,
                       Lxsd.Bounds
                         ([Lxsd.Inclusive
                             (Stringdata.of_string "1")],
                          [])) } ] }
      end


