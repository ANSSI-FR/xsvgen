(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_lexer.ml 1665 2012-05-24 09:47:08Z maarek $ *)

(** {1 XML lexer} *)

(** {2 Characters} *)

let decl_1_0 =
  { Xml.version = Xml.Version_1_0;
    Xml.encoding = Some Xml.Encoding_UTF8;
    Xml.standalone = None }

let next_decimals =
  Stringdata.match_regexp_from (Stringdata.of_string "[0-9]*")

let next_hex =
  Stringdata.match_regexp_from (Stringdata.of_string "[0-9a-fA-F]*")

(** Match the character set {b CharData} according to the given XML
    declaration. *)
let next_CharData_mDisc = function
  | { Xml.version = Xml.Version_1_0;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_CharData_mDisc_1_0
  | { Xml.version = Xml.Version_1_1;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_CharData_mDisc_1_1
  | { Xml.version = _;
      Xml.encoding = None;
      Xml.standalone = _ } ->
        Message.error (Error.XMLL_encoding_not_supported)

(** Match the character set {b CharData} according to the given XML
    declaration without character ['\]'] to identify ["\]\]>"]. *)
let next_CharData_mDisc_no_square_closing_bracket = function
  | { Xml.version = Xml.Version_1_0;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_CharData_mDisc_no_square_closing_bracket_1_0
  | { Xml.version = Xml.Version_1_1;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_CharData_mDisc_no_square_closing_bracket_1_1
  | { Xml.version = _;
      Xml.encoding = None;
      Xml.standalone = _ } ->
        Message.error (Error.XMLL_encoding_not_supported)

(** Match the character set {b Char} according to the given XML
    declaration without character ['\]'] to identify ["\]\]>"]. *)
let next_Char_mDisc_no_square_closing_bracket = function
  | { Xml.version = Xml.Version_1_0;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_Char_mDisc_no_square_closing_bracket_1_0
  | { Xml.version = Xml.Version_1_1;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_Char_mDisc_no_square_closing_bracket_1_1
  | { Xml.version = _;
      Xml.encoding = None;
      Xml.standalone = _ } ->
        Message.error (Error.XMLL_encoding_not_supported)

(** Match the character set {b Char} according to the given XML
    declaration without character ['-'] to identify ["--"]. *)
let next_Char_mDisc_no_hyphen_minus = function
  | { Xml.version = Xml.Version_1_0;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_Char_mDisc_no_hyphen_minus_1_0
  | { Xml.version = Xml.Version_1_1;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_Char_mDisc_no_hyphen_minus_1_1
  | { Xml.version = _;
      Xml.encoding = None;
      Xml.standalone = _ } ->
        Message.error (Error.XMLL_encoding_not_supported)

(** Match the character set {b Char} according to the given XML
    declaration without character ['?'] to identify ["?>"]. *)
let next_Char_mDisc_no_question_mark = function
  | { Xml.version = Xml.Version_1_0;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_Char_mDisc_no_question_mark_1_0
  | { Xml.version = Xml.Version_1_1;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_Char_mDisc_no_question_mark_1_1
  | { Xml.version = _;
      Xml.encoding = None;
      Xml.standalone = _ } ->
        Message.error (Error.XMLL_encoding_not_supported)

(** Match the character set {b AttValue} surrounded by single quotes
    according to the given XML declaration. *)
let next_AttValue_singlequote_mDisc = function
  | { Xml.version = Xml.Version_1_0;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_AttValue_singlequote_mDisc_1_0
  | { Xml.version = Xml.Version_1_1;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_AttValue_singlequote_mDisc_1_1
  | { Xml.version = _;
      Xml.encoding = None;
      Xml.standalone = _ } ->
        Message.error (Error.XMLL_encoding_not_supported)

(** Match the character set {b AttValue} surrounded by double quotes
    according to the given XML declaration. *)
let next_AttValue_doublequote_mDisc = function
  | { Xml.version = Xml.Version_1_0;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_AttValue_doublequote_mDisc_1_0
  | { Xml.version = Xml.Version_1_1;
      Xml.encoding = Some Xml.Encoding_UTF8;
      Xml.standalone = _ } ->
        Xml_re.next_AttValue_doublequote_mDisc_1_1
  | { Xml.version = _;
      Xml.encoding = None;
      Xml.standalone = _ } ->
        Message.error (Error.XMLL_encoding_not_supported)

(** {2 Generic lexing functions} *)

(** Skip space characters and return the remainder.
    @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
*)
let rec skip_spaces sd =
  match Stringdata.next sd with
  | Some (Some (('\032'|'\009'|'\013'|'\010'),sd')) ->
      skip_spaces sd'
  | None | Some _ ->
      sd

(** Skip space characters and return a notice and the remainder.
    @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
*)
let skip_spaces_with_notice sd =
  match Stringdata.next sd with
  | Some (Some (('\032'|'\009'|'\013'|'\010'),sd')) ->
      true,(skip_spaces sd')
  | None | Some _ ->
      false,sd

(** Skip space characters and return the remainder if a given string
    data starts with at least one space character.

    @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.XMLL_expected "S"]
 *)
let next_space sd =
  match Stringdata.next sd with
  | Some (Some (('\032'|'\009'|'\013'|'\010'),sd')) ->
      skip_spaces sd'
  | None | Some _ ->
      Message.error_at sd (Error.XMLL_expected "S")

(** Check if a given string data starts with the ['='] character and
    return the remainder.

    @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.XMLL_expected ("Eq")]
 *)
let rec next_eq sd =
  match Stringdata.next sd with
  | Some (Some ('=',sd')) ->
      sd'
  | None | Some _ ->
      Message.error_at sd (Error.XMLL_expected "Eq")




(** {2 Lexing QName} *)


(** Get the next unqualified name (without character [':']) starting a
    given string data. Return [None] otherwise.
*)
let next_ncname_fun sd =
  match Stringdata.next_utf8 sd with
  | None ->
      None
  | Some (_,sd') ->
      if Xml_re.test_NameStartChar_NS_mDisc
          (Stringdata.sub sd sd')
      then
        let sd'' =
          match Xml_re.next_NameChar_NS_mDisc sd' with
          | None -> sd'
          | Some sd'' -> sd''
        in
        let name =
          Stringdata.sub sd sd''
        in
        Some (name,sd'')
      else
        None

(** Get the next qualified name starting a given string data. Return
    [None] otherwise. *)
let next_qname_fun sd =
  match next_ncname_fun sd with
  | None -> None
  | Some (ncname1,sd1) ->
      match Stringdata.next sd1 with
      | Some (Some (':',sd2)) ->
          begin
            match next_ncname_fun sd2 with
            | None -> None
            | Some (ncname2,sd3) ->
                Some ({ Xml.prefix = Some ncname1;
                        Xml.local_name = ncname2 }, sd3)
          end
      | _ ->
          Some ({ Xml.prefix = None;
                  Xml.local_name = ncname1 }, sd1)

(** Get the next unqualified name (without character [':']) starting a
    given string data.
    @raise Error.E [Error.XMLL_not_well_formed "Name"] otherwise.
*)
let next_ncname sd =
  match next_ncname_fun sd with
  | None ->
      Message.error_at sd (Error.XMLL_not_well_formed "Name")
  | Some (name,sd') ->
      name,sd'

(** Get the next qualified name starting a given string data.
    @raise Error.E [Error.XMLL_not_well_formed "Name"] otherwise.
 *)
let next_qname sd =
  let ncname1,sd1 = next_ncname sd in
  match Stringdata.next sd1 with
  | Some (Some (':',sd2)) ->
      let ncname2,sd3 = next_ncname sd2 in
      { Xml.prefix = Some ncname1;
        Xml.local_name = ncname2 },
      sd3
  | _ ->
      { Xml.prefix = None;
        Xml.local_name = ncname1 },
      sd1

(** This function assumes that the value referenced is XML valid. *)
let next_EntityRef entities sd =
  let (ncname,sd') = next_ncname sd in
  match Lib.list_assoc Stringdata.eq ncname entities with
  | None ->
      Message.error_at sd (Error.XMLL_entity_reference_unknown)
  | Some i -> i,sd'

let next_CharRef error_status read_fun sd =
  let i,sd_final =
    match Stringdata.next sd with
    | Some (Some ('x',sd')) ->
        begin
          match next_hex sd' with
          | None ->
              Message.error_at sd (Error.XMLL_not_well_formed "CharRef")
          | Some sd_n ->
              let sd_num = Stringdata.sub sd' sd_n in
              try
                (int_of_string ("0x" ^ Stringdata.to_string sd_num)),
                sd_n
              with
                Failure _ ->
                  (* catching empty sd_num *)
                  Message.error_at sd (Error.XMLL_not_well_formed "CharRef")
        end
    | Some _ ->
        begin
          match next_decimals sd with
          | None ->
              Message.error_at sd (Error.XMLL_not_well_formed "CharRef")
          | Some sd_n ->
              let sd_num = Stringdata.sub sd sd_n in
              try (int_of_string (Stringdata.to_string sd_num)),sd_n with
                Failure _ ->
                  (* catching empty sd_num *)
                  Message.error_at sd (Error.XMLL_not_well_formed "CharRef")
        end
    | None ->
        Message.error_at sd (Error.XMLL_not_well_formed "CharRef")
  in
  begin
    let enc = Utf8.enc_of_int i in
    begin
      match enc with
      | Utf8.Byte1 _ ->
          begin
            match Stringdata.next sd_final with
            | Some (Some (';',_)) ->
                (** ASCII character *)
                if error_status.Xml.ascii_ref
                then
                  Message.error_at sd
                    (Error.XMLL_SEC_ascii_char_reference_not_allowed)
                else
                  Message.warning_at sd
                    (Error.XMLL_SEC_ascii_char_reference_not_allowed)
            | Some _  | None ->
                Message.error_at sd (Error.XMLL_not_well_formed "Reference")
          end
      | Utf8.Byte2 _ | Utf8.Byte3 _ | Utf8.Byte4 _ ->
          ()
    end;
    let sd_char =
      Stringdata.of_string
        (Utf8.string_of_enc enc) in
    if
      ( i = (Char.code '&')
      || i = (Char.code '\'')
      || i = (Char.code '\"')
      || i = (Char.code '<')
      || i = (Char.code '>') )
    then
      sd_char,sd_final
    else
      match read_fun sd_char with
      | None ->
          Message.error_at sd (Error.XMLL_char_reference_not_well_formed)
      | Some _ ->
          sd_char,sd_final
  end

let next_reference error_status entities read_fun sd =
  let sd_value,sd_r =
    match Stringdata.next sd with
    | Some (Some ('&',sd1)) ->
        begin
          match Stringdata.next sd1 with
          | Some (Some ('#',sd2)) ->
              next_CharRef error_status read_fun sd2
          | Some _ ->
              next_EntityRef entities sd1
          | None ->
              Message.error_at sd (Error.XMLL_not_well_formed "Reference")
        end
    | Some _ | None ->
        Message.error_at sd (Error.XMLL_not_well_formed "Reference")
  in
  match Stringdata.next sd_r with
  | Some (Some (';',sd_next)) ->  sd_value,sd_next
  | Some _  | None ->
      Message.error_at sd (Error.XMLL_not_well_formed "Reference")



(** {2 Lexing attributes} *)

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
*)
let next_att_val error_status decl sd_start =
  match Stringdata.next sd_start with
  | Some (Some ('\'',sd_start_val)) ->
      let rec next_aux sd1 =
        match Stringdata.next sd1 with
        | Some (Some ('\'',_)) | None ->
            sd1
        | Some (Some ('&',_)) ->
            let value,sd2 =
              next_reference
                error_status
                Xml_val.predefined_entities
                (next_AttValue_singlequote_mDisc decl)
                sd1
            in
            next_aux (Stringdata.mark_reference value sd2)
        | Some _ ->
            begin
              match next_AttValue_singlequote_mDisc decl sd1 with
              | None -> sd1
              | Some sd2 ->
                  next_aux (Stringdata.mark_data sd2)
            end
      in
      let sd_begin_val = Stringdata.mark_data sd_start_val in
      let sd_end_val = next_aux sd_begin_val in
      let value = Stringdata.sub sd_begin_val sd_end_val in
      begin
        match Stringdata.next sd_end_val with
        | Some (Some ('\'',sd_end)) ->
            value,sd_end
        | _ ->
            Message.error_at sd_start
              (Error.XMLL_not_well_formed "AttValue")
      end
  | Some (Some ('\"',sd_start_val)) ->
      let rec next_aux sd1 =
        match Stringdata.next sd1 with
        | Some (Some ('\"',_)) | None ->
            sd1
        | Some (Some ('&',_)) ->
            let value,sd2 =
              next_reference
                error_status
                Xml_val.predefined_entities
                (next_AttValue_doublequote_mDisc decl)
                sd1
            in
            next_aux (Stringdata.mark_reference value sd2)
        | Some _ ->
            begin
              match next_AttValue_doublequote_mDisc decl sd1 with
              | None -> sd1
              | Some sd2 ->
                  next_aux (Stringdata.mark_data sd2)
            end
      in
      let sd_begin_val = Stringdata.mark_data sd_start_val in
      let sd_end_val = next_aux sd_begin_val in
      let value = Stringdata.sub sd_begin_val sd_end_val in
      begin
        match Stringdata.next sd_end_val with
        | Some (Some ('\"',sd_end)) ->
            value,sd_end
        | _ ->
            Message.error_at sd_start
              (Error.XMLL_not_well_formed "AttValue")
      end
  | None | Some _ ->
      Message.error_at sd_start
        (Error.XMLL_not_well_formed "AttValue")

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
*)
let next_attribute error_status decl sd =
  let name ,sd1 =
    next_qname sd in
  let sd2 =
    skip_spaces sd1 in
  let sd3 =
    next_eq sd2 in
  let sd4 =
    skip_spaces sd3 in
  let att_val,sd5 =
    next_att_val error_status decl sd4
  in
  (name,att_val),sd5

(** {2 Lexing tags} *)

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
*)
let rec next_open_tag_attributes error_status decl name attrs sd =
  match Stringdata.next_utf8 sd with
  | Some ((Utf8.Byte1 '>'),sd') ->
      (Xml.Tag (name,attrs)),sd'
  | Some ((Utf8.Byte1 '/'),sd') ->
      begin
        match Stringdata.next sd' with
        | Some (Some ('>',sd'')) ->
            (Xml.Taggat (name,attrs)),sd''
        | None | Some _ ->
            Message.error_at sd (Error.XMLL_expected "'>'")
      end
  | Some ((Utf8.Byte1 ('\032'|'\009'|'\013'|'\010')),sd1) ->
      begin
        let sd2 = skip_spaces sd1 in
        if
          (match Stringdata.next sd2 with
          | Some (Some ('>',_)) | Some (Some ('/',_)) -> true
          | _ -> false)
        then
          (* there are no more attributes, we recurse to close the tag *)
          next_open_tag_attributes error_status decl name attrs sd2
        else
          let (att_name,att_value),sd3 =
            next_attribute error_status decl sd2 in
          if Xml.M_unexp.mem att_name attrs
          then
            Message.error_at
              att_name.Xml.local_name
              Error.XMLL_XMLP_attribute_definition_not_unique
          else
            next_open_tag_attributes error_status decl
              name (Xml.M_unexp.add att_name att_value attrs) sd3
      end
  | Some ((Utf8.Byte1 _|Utf8.Byte2 _|Utf8.Byte3 _|Utf8.Byte4 _),_)
  | None ->
      Message.error_at sd (Error.XMLL_expected "'>' or '/>'")

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
*)
let next_open_tag error_status decl sd =
  let name,sd' =
    next_qname sd in
  next_open_tag_attributes error_status decl name Xml.M_unexp.empty sd'

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.XMLL_expected "'>'"]
*)
let rec next_close_tag_end name sd =
  match Stringdata.next sd with
  | Some (Some ('>',sd')) ->
      (Xml.Gat name),sd'
  | Some _ ->
      let sd' =
        next_space sd in
      next_close_tag_end name sd'
  | None ->
      Message.error_at sd (Error.XMLL_expected "'>'")

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
*)
let rec next_close_tag sd =
  let name,sd' =
    next_qname sd in
  next_close_tag_end name sd'

(** {2 Lexing data} *)


(** Get the next comment {b Comment} starting a given string data.
    @raise Error.E [Error.XMLL_not_well_formed "Comment"] if the comment is not well formed.
    @raise Error.E [Error.XMLL_expected "'-->'"] if the comment does not end.
 *)
let next_comment decl b_sd =
  match next_Char_mDisc_no_hyphen_minus decl b_sd with
  | None ->
      Message.error_at b_sd (Error.XMLL_not_well_formed "Comment")
  | Some sd1 ->
      let rec aux sd =
        match Stringdata.nexts (Stringdata.of_string "-->") sd with
        | Some sd' ->
            sd'
        | None ->
            match Stringdata.nexts (Stringdata.of_string "--") sd with
            | Some _ ->
                Message.error_at sd (Error.XMLL_not_well_formed "Comment")
            | None ->
                match Stringdata.next_utf8 sd with
                | None ->
                    Message.error_at sd (Error.XMLL_expected "'-->'")
                | Some ((Utf8.Byte1 '-'),sd') ->
                    aux sd'
                | Some ((Utf8.Byte1 _|Utf8.Byte2 _|Utf8.Byte3 _|Utf8.Byte4 _),_) ->
                    match next_Char_mDisc_no_hyphen_minus decl sd with
                    | None ->
                        Message.error_at sd (Error.XMLL_not_well_formed "Comment")
                    | Some sd' ->
                        aux sd'
      in
      aux sd1







(** {2 Lexing XML declarations} *)

(** @raise Error.E [Error.XMLL_expected "'?>'"]
*)
let next_xmldecl_closing (version, encoding, standalone) sd =
  match Stringdata.nexts (Stringdata.of_string "?>") sd with
  | Some sd' ->
      { Xml.version = version;
        Xml.encoding = encoding;
        Xml.standalone = standalone },sd'
  | None ->
      Message.error_at sd (Error.XMLL_expected "'?>'")


let one_choice sd_start v =
  (fun sd ->
    match Stringdata.nexts sd_start sd with
    | None -> None
    | Some sd' -> Some (v,sd') )

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.XMLL_expected _]
*)
let next_xmldecl_standalone_val (version, encoding) sd =
  match
    Lib.choice
      [ one_choice (Stringdata.of_string "'yes'") true;
        one_choice (Stringdata.of_string "\"yes\"") true;
        one_choice (Stringdata.of_string "'no'") false;
        one_choice (Stringdata.of_string "\"no\"") false ]
      sd
  with
  | None ->
      Message.error_at sd (Error.XMLL_expected "value for 'standalone'")
  | Some (b,sd') ->
      let sd'' = skip_spaces sd' in
      next_xmldecl_closing (version, encoding, Some b) sd''

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.XMLL_expected _]
*)
let next_xmldecl_standalone spacing (version, encoding) sd =
  match Stringdata.nexts (Stringdata.of_string "standalone") sd with
  | None ->
      next_xmldecl_closing (version, encoding, None) sd
  | Some sd1 ->
      if spacing
      then
        let sd2 =
          skip_spaces sd1 in
        let sd3 =
          next_eq sd2 in
        let sd4 =
          skip_spaces sd3 in
        next_xmldecl_standalone_val (version, encoding) sd4
      else
        Message.error_at sd (Error.XMLL_expected "S")

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.XMLL_encoding_not_supported]
    @raise Error.E [Error.XMLL_expected _]
*)
let next_xmldecl_encoding_val version sd =
  match
    Lib.choice
      [ one_choice (Stringdata.of_string "'UTF-8'") Xml.Encoding_UTF8;
        one_choice (Stringdata.of_string "\"UTF-8\"") Xml.Encoding_UTF8;
        one_choice (Stringdata.of_string "'utf-8'") Xml.Encoding_UTF8;
        one_choice (Stringdata.of_string "\"utf-8\"") Xml.Encoding_UTF8 ]
      sd
  with
  | None ->
      begin
        match Stringdata.next sd with
        | Some (Some ('\"' as c1,sd1))
        | Some (Some ('\'' as c1,sd1)) ->
            begin
              match Stringdata.next_utf8 sd1 with
              | None ->
                  Message.error_at sd (Error.XMLL_not_well_formed "XMLDecl")
              | Some (_,sd2) ->
                  if not (Xml_re.test_EncNameStartChar
                            (Stringdata.sub sd1 sd2))
                  then
                    Message.error_at sd (Error.XMLL_not_well_formed "XMLDecl")
                  else
                    let sd3 =
                      match Xml_re.next_EncNameChar sd2 with
                      | None -> sd2
                      | Some sd' -> sd'
                    in
                    match Stringdata.next sd3 with
                    | None | Some None ->
                        Message.error_at sd (Error.XMLL_not_well_formed "XMLDecl")
                    | Some (Some (c2,_)) ->
                        if c1 = c2
                        then
                          Message.error_at sd (Error.XMLL_encoding_not_supported)
                        else
                          Message.error_at sd (Error.XMLL_not_well_formed "XMLDecl")
            end
        | _ ->
            Message.error_at sd (Error.XMLL_expected "value for 'encoding'")
      end
  | Some (encoding,sd') ->
      let spacing,sd'' = skip_spaces_with_notice sd' in
      next_xmldecl_standalone spacing (version, Some encoding) sd''

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.XMLL_encoding_not_supported]
    @raise Error.E [Error.XMLL_expected _]
*)
let next_xmldecl_encoding spacing version sd =
  match Stringdata.nexts (Stringdata.of_string "encoding") sd with
  | None ->
      Message.warning_at sd Error.XMLL_encoding_missing;
      next_xmldecl_standalone spacing (version, (Some Xml.Encoding_UTF8)) sd
  | Some sd1 ->
      if spacing
      then
        let sd2 =
          skip_spaces sd1 in
        let sd3 =
          next_eq sd2 in
        let sd4 =
          skip_spaces sd3 in
        next_xmldecl_encoding_val (version) sd4
      else
        Message.error_at sd (Error.XMLL_expected "S")


(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
    @raise Error.E [Error.XMLL_version_not_supported]
    @raise Error.E [Error.XMLL_encoding_not_supported]
*)
let next_xmldecl_version_val sd =
  match
    Lib.choice
      [ one_choice (Stringdata.of_string "'1.1'") Xml.Version_1_1;
        one_choice (Stringdata.of_string "\"1.1\"") Xml.Version_1_1;
        one_choice (Stringdata.of_string "'1.0'") Xml.Version_1_0;
        one_choice (Stringdata.of_string "\"1.0\"") Xml.Version_1_0 ]
      sd
  with
  | None ->
      begin
        match
          next_att_val
            { Xml.doctype = true;
              Xml.cdata = true;
              Xml.pi = true;
              Xml.comment = true;
              Xml.ascii_ref = false;
              Xml.xsi = Xml.XSI_reject_all }
            decl_1_0 sd with
        | _,sd' ->
            Message.error_at sd' (Error.XMLL_version_not_supported)
      end
  | Some (version,sd') ->
      let spacing,sd'' = skip_spaces_with_notice sd' in
      next_xmldecl_encoding spacing version sd''


(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
    @raise Error.E [Error.XMLL_version_not_supported]
    @raise Error.E [Error.XMLL_version_missing]
    @raise Error.E [Error.XMLL_encoding_not_supported]
*)
let next_xmldecl_version sd = 
  match Stringdata.nexts (Stringdata.of_string "version") sd with
  | None ->
      Message.error_at sd (Error.XMLL_version_missing)
  | Some sd1 ->
      let sd2 =
        skip_spaces sd1 in
      let sd3 =
        next_eq sd2 in
      let sd4 =
        skip_spaces sd3 in
      next_xmldecl_version_val sd4

(** {2 Lexing DOCTYPE, CDATA, PI and comments} *)


(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
*)
let next_cdata decl b_sd =
  let rec aux sd =
    let sd_marked = Stringdata.mark_data sd in
    match Stringdata.nexts (Stringdata.of_string "]]>") sd_marked with
    | Some sd' ->
        let sd_marked' = Stringdata.mark_skip sd' in
        sd_marked'
    | None ->
        match Stringdata.next sd with
        | Some (Some (']',sd')) ->
            aux sd'
        | None ->
            Message.error_at b_sd (Error.XMLL_not_well_formed "CData")
        | Some _ ->
            match next_Char_mDisc_no_square_closing_bracket decl sd with
            | Some sd' ->
                aux sd'
            | None ->
                Message.error_at sd (Error.XMLL_expected "']]>'")
  in
  aux b_sd

(** Get the next processing instruction {b PI} starting a given string data.
    @raise Error.E [Error.XMLL_pi_target_not_allowed] if the target is a
    variant of "xml".
    @raise Error.E [Error.XMLL_not_well_formed "PI"] if the PI is not well formed.
    @raise Error.E [Error.XMLL_expected "'?>'"] if the PI does not end.
 *)
let next_processing_instructions decl b_sd =
  if
    ( (Lib.opt_bool (Stringdata.nexts (Stringdata.of_string "xml") b_sd))
    || (Lib.opt_bool (Stringdata.nexts (Stringdata.of_string "xmL") b_sd))
    || (Lib.opt_bool (Stringdata.nexts (Stringdata.of_string "xMl") b_sd))
    || (Lib.opt_bool (Stringdata.nexts (Stringdata.of_string "xML") b_sd))
    || (Lib.opt_bool (Stringdata.nexts (Stringdata.of_string "Xml") b_sd))
    || (Lib.opt_bool (Stringdata.nexts (Stringdata.of_string "XmL") b_sd))
    || (Lib.opt_bool (Stringdata.nexts (Stringdata.of_string "XMl") b_sd))
    || (Lib.opt_bool (Stringdata.nexts (Stringdata.of_string "XML") b_sd)) )
  then
    Message.error_at b_sd (Error.XMLL_pi_target_not_allowed)
  else
    match next_ncname_fun b_sd with
    | None ->
        Message.error_at b_sd (Error.XMLL_not_well_formed "PI")
    | Some (name,sd1) ->
        match Stringdata.nexts (Stringdata.of_string "?>") sd1 with
        | Some sd2 ->
            (Xml.PI (name,(Stringdata.sub sd1 sd1))),sd2
        | None ->
            let sd2 = next_space sd1 in
            let rec aux sd =
              match Stringdata.nexts (Stringdata.of_string "?>") sd with
              | Some sd' ->
                  (Xml.PI (name,(Stringdata.sub sd2 sd))),sd'
              | None ->
                  match Stringdata.next sd with
                  | Some (Some ('?',sd')) ->
                      aux sd'
                  | None ->
                      Message.error_at b_sd (Error.XMLL_not_well_formed "PI")
                  | Some _ ->
                      match next_Char_mDisc_no_question_mark decl sd with
                      | Some sd' ->
                          aux sd'
                      | None ->
                          Message.error_at sd (Error.XMLL_expected "'?>'")
            in
            aux sd2


(** Get the next {b doctype} starting a given string data. This
    function checks partially the syntax of DocTypes by looking only at the
    opening and closing angled brackets.

    @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
*)
let next_doctype decl sd =
  let rec aux awaiting sd1 =
    match Stringdata.next_utf8 sd1 with
    | Some ((Utf8.Byte1 '<'),_) ->
        begin
          match Stringdata.nexts (Stringdata.of_string "<!--") sd1 with
          | Some sd2 ->
              let sd3 = next_comment decl sd2 in
              aux awaiting sd3
          | None ->
              match Stringdata.nexts (Stringdata.of_string "<?") sd1 with
              | Some sd2 ->
                  let _,sd3 =
                    next_processing_instructions decl sd2 in
                  aux awaiting sd3
              | None ->
                  match Stringdata.nexts (Stringdata.of_string "<!") sd1 with
                  | Some sd2 ->
                      aux (awaiting + 1) sd2
                  | None ->
                      Message.error_at sd1
                        (Error.XMLL_not_well_formed "DocType1")
        end
    | Some ((Utf8.Byte1 '>'),sd2) ->
        if awaiting = 0
        then
          (Xml.Doctype (Stringdata.sub sd sd2)),sd2
        else
          aux (awaiting - 1) sd2
    | None ->
        Message.error_at sd
          (Error.XMLL_not_well_formed "DocType3")
    | Some ((Utf8.Byte1 _|Utf8.Byte2 _|Utf8.Byte3 _|Utf8.Byte4 _),sd2) ->
        aux awaiting sd2
  in
  aux 0 sd



(** {2 Lexing XML tokens} *)

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
    @raise Error.E [Error.XMLL_version_not_supported]
    @raise Error.E [Error.XMLL_version_missing]
    @raise Error.E [Error.XMLL_encoding_not_supported]
*)
let first_xml_token_xmldecl sd =
  match Stringdata.nexts (Stringdata.of_string "<?xml") sd with
  | Some sd' ->
      let sd'' = next_space sd' in
      next_xmldecl_version sd''
  | None ->
      { Xml.version = Xml.Version_1_0;
        Xml.encoding = Some Xml.Encoding_UTF8;
        Xml.standalone = None },sd




(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_token_not_finished]
*)
let rec next_data error_status decl begin_sd =
  let rec next_aux has_ref_cdata sd : Stringdata.t * bool =
    match Stringdata.nexts (Stringdata.of_string "<!--") sd with
    | Some sd' ->
        (* Data contains a comment *)
        if error_status.Xml.comment
        then Message.error_at sd (Error.XMLP_SEC_comment)
        else Message.warning_at sd (Error.XMLP_SEC_comment);
        let sd'' = next_comment decl sd' in
        next_aux has_ref_cdata (Stringdata.mark_skip sd'')
    | None ->
        begin
          match Stringdata.next sd with
          | Some (Some (']',sd')) ->
              begin
                match Stringdata.nexts (Stringdata.of_string "]]>") sd with
                | None ->
                    (* Data contains the character ']' not starting a ']]>' *)
                    next_aux has_ref_cdata sd'
                | Some _ ->
                    (* Data contains the sequence ']]>' *)
                    Message.error_at sd (Error.XMLL_not_well_formed "CharData")
              end
          | Some (Some ('<',_)) ->
              begin
                let sd_marked = Stringdata.mark_data sd in
                match
                  Stringdata.nexts
                    (Stringdata.of_string "<![CDATA[")
                    sd_marked
                with
                | Some sd' ->
                    (* Data contains CDSect *)
                    let sd_marked' = Stringdata.mark_skip sd' in
                    if error_status.Xml.cdata
                    then Message.error_at sd (Error.XMLP_SEC_cdata)
                    else Message.warning_at sd (Error.XMLP_SEC_cdata);
                    let sd'' = next_cdata decl sd_marked' in
                    next_aux true (Stringdata.mark_data sd'')
                | None ->
                    (* Data ends with '<' *)
                    sd,has_ref_cdata
              end
          | None ->
              (* Data ends with the end of input *)
              sd,has_ref_cdata
          | Some (Some ('&',_)) ->
              (* Data contains a reference *)
              let value,sd' =
                next_reference
                  error_status
                  Xml_val.predefined_entities
                  (next_CharData_mDisc decl)
                  sd
              in
              next_aux true (Stringdata.mark_reference value sd')
          | Some _ ->
              begin
                match next_CharData_mDisc_no_square_closing_bracket decl sd with
                | None ->
                    (* Data contains non valid characters *)
                    Message.error_at sd (Error.XMLL_not_well_formed "CharData")
                | Some sd' ->
                    (* Data contains a sequence of valid characters *)
                    next_aux has_ref_cdata (Stringdata.mark_data sd')
              end
        end
  in
  let sd_start = Stringdata.mark_data begin_sd in
  let sd_end,has_ref_cdata = next_aux false sd_start in
  let data = Stringdata.sub sd_start sd_end in
  if Stringdata.empty data
  then
    (* Data is empty, we skip it *)
    next_xml_token error_status decl sd_end
  else
    (* Data is completed, we return it *)
    (Xml.Data (data,has_ref_cdata)),sd_end


(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
    @raise Error.E [Error.XMLL_pi_target_not_allowed]
    @raise Error.E [Error.XMLL_token_not_finished]
    @raise Error.E [Error.XMLL_empty_input]
*)
and next_xml_token error_status (decl : Xml.declaration) sd =
  match Stringdata.nexts (Stringdata.of_string "</") sd with
  | Some sd' ->
      next_close_tag sd'
  | None ->
      match Stringdata.nexts (Stringdata.of_string "<!DOCTYPE") sd with
      | Some sd' ->
          if error_status.Xml.doctype
          then Message.error_at sd (Error.XMLP_SEC_doctype)
          else
            begin
              Message.warning_at sd (Error.XMLP_SEC_doctype);
              let sd'' = next_space sd' in
              next_doctype decl sd''
            end
      | None ->
          match Stringdata.nexts (Stringdata.of_string "<![CDATA[") sd with
          | Some _ ->
              next_data error_status decl sd
          | None ->
              match Stringdata.nexts (Stringdata.of_string "<?") sd with
              | Some sd' ->
                  if error_status.Xml.pi
                  then Message.error_at sd (Error.XMLP_SEC_pi)
                  else
                    begin
                      Message.warning_at sd (Error.XMLP_SEC_pi);
                      next_processing_instructions decl sd'
                    end
              | None ->
                  match Stringdata.nexts (Stringdata.of_string "<!--") sd with
                  | Some _ ->
                      if error_status.Xml.comment
                      then Message.error_at sd (Error.XMLP_SEC_comment)
                      else
                        begin
                          next_data error_status decl sd
                        end
                  | None ->
                      match Stringdata.next sd with
                      | Some (Some ('<',sd')) ->
                          next_open_tag error_status decl sd'
                      | Some _ ->
                          next_data error_status decl sd
                      | None ->
                          Message.error_at sd (Error.XMLL_empty_input)



(** {2 Lexing XML content} *)

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
    @raise Error.E [Error.XMLL_pi_target_not_allowed]
    @raise Error.E [Error.XMLL_token_not_finished]
    @raise Error.E [Error.XMLL_empty_input]
*)
let stream_of_sd error_status set initial_sd =
  let rec next sd =
    Xml.Stream
      (fun () ->
        if  Stringdata.empty sd
        then
          None
        else
          let lexeme,sd' =
            next_xml_token error_status set sd in
          let sd'' = Stringdata.clear_marks sd' in
          Some (lexeme,next sd'')
      )
  in
  next initial_sd

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.SD_subtraction_sources]
    @raise Error.E [Error.SD_subtraction_limits]
    @raise Error.E [Error.SD_subtraction_positions]
    @raise Error.E [Error.XMLL_expected _]
    @raise Error.E [Error.XMLL_version_not_supported]
    @raise Error.E [Error.XMLL_version_missing]
    @raise Error.E [Error.XMLL_encoding_not_supported]
    @raise Error.E [Error.XMLL_pi_target_not_allowed]
    @raise Error.E [Error.XMLL_token_not_finished]
    @raise Error.E [Error.XMLL_empty_input]
*)
let lex_xml error_status xml_sd : Xml.declaration * Xml.stream =
  if Stringdata.empty xml_sd
  then
    Message.error_at xml_sd Error.XMLL_empty_input
  else
    let decl,body_sd =
      first_xml_token_xmldecl xml_sd  in
    decl, (stream_of_sd error_status decl body_sd)

(** {2 Other lexing functions} *)

(** Test if a given string data is composed only of space characters.
    @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
*)
let is_spaces sd_init : bool =
  let sd' = skip_spaces sd_init in
  Stringdata.empty sd'



(** Test if a given string data is a a {b NCName}, relies on
    {!Xml_re.test_Name_NS_mDisc}. *)
let is_ncname sd =
  Xml_re.test_Name_NS_mDisc sd

(** Return the unexpanded name corresponding to a given string
    data. Return [None] if the entire string data is not a qualified
    name. *)
let lex_qname sd =
  match next_qname_fun sd with
  | None -> None
  | Some (name,sd') ->
      if  Stringdata.empty sd'
      then
        Some name
      else
        None


(** Return the expanded name corresponding to a given string data in a
    given namespace context. Return [None] if the entire string data
    is not a qualified name or if the namespace resolution does not
    succeed. *)
let lex_qname_ns (ns,sd) : Xml.expanded_name option =
  match lex_qname sd with
  | None -> None
  | Some unexpanded_name ->
      Xml_ns.expand_elt_opt unexpanded_name ns

let lex_list sd =
  let sd' = Stringdata.whitespace_collapse sd in
  let rec aux accu begin_sd sd1 =
    match Stringdata.next_utf8 sd1 with
    | Some ((Utf8.Byte1 '\032'),sd2) ->
        let item =
          Stringdata.sub begin_sd sd1 in
        aux
          (if Stringdata.empty item
          then accu
          else item::accu)
          sd2
          sd2
    | None ->
        let item =
          Stringdata.sub begin_sd sd1 in
        List.rev
          (if Stringdata.empty item
          then accu
          else item::accu)
    | Some ((Utf8.Byte1 _|Utf8.Byte2 _ |Utf8.Byte3 _ |Utf8.Byte4 _),sd2) ->
        aux
          accu
          begin_sd
          sd2
  in
  aux [] sd' sd'

