(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_parser.ml 1654 2012-05-22 17:21:28Z maarek $ *)


(** {1 XML parser} *)


(** {2 Tree building} *)

module M = Map.Make (struct type t = int let compare = compare end)

module S :
  sig
    type 'a t
    val push : 'a -> 'a t -> 'a t
    val pull : 'a t -> ('a * 'a t) option
    val length : 'a t -> int
    val empty : 'a t
    val to_list : 'a t -> 'a list
  end = struct
    type 'a t = int * 'a M.t
    let push v (i,m) =
      let i' = i + 1 in
      i',(M.add i' v m)
    let pull (i,m) =
      try
        let v = M.find i m in
        Some (v,(i-1,M.remove i m))
      with
      | Not_found ->
          None
    let length (i,_) = i
    let empty =
      (0,M.empty)
    let to_list (_,m) =
      List.map (fun (_,v) -> v) (M.bindings m)
  end

type visited =
    (Xml.raw_tag option * Xml.raw_element Xml.content S.t) S.t

(** [add_content c visited] adds [c] (which is usually a leaf or a
    node) to the top of the content stack of the last opened tag of
    the stack [visited], which must not be empty.
    @raise Error.E [Error.A_empty_parser_stack]
*)
let add_content c visited =
  match S.pull visited with
  | None ->
      Message.error (Error.A_empty_parser_stack)
  | Some ((opened_o, content_stack),stack') ->
      let content_stack' =
        S.push c content_stack in
      S.push (opened_o, content_stack') stack'


(** [add_tag test_depth t visited] adds [t] to the top of the
    [visited] stack. Hence [t] becomes the last opened (and not yet
    closed) tag.  It performs a depth test [test_depth] on the number
    of nested elements allowed.
 *)
let add_tag test_depth t visited =
  test_depth (S.push (Some t,S.empty) visited)


(** [add_gat test_contents t visited] closes the last opened tag of
    [visited] stack with [t]. This means that the stack must not be
    empty, must have at least a tag opened. Warning: the last opened
    tag is not necessarily the one we are closing ! One will have to
    check the balance of tags afterwards. When the node has been
    built, it is added to the contents of its parent tag with
    [add_content]. It performs a contents test [test_contents] on the
    number of content elements.
    @raise Error.E [Error.A_empty_parser_stack]
    @raise Error.E [Error.XMLP_opening_tag_missing]
*)
let add_gat test_contents t visited =
  match S.pull visited with
  | None ->
      Message.error (Error.A_empty_parser_stack)
  | Some ((None, _),_) ->
      Message.error_at
        t.Xml.local_name
        (Error.XMLP_opening_tag_missing)
  | Some ((Some t_last_opened, contents),visited') ->
      let closed_content =
        Xml.Node { Xml.node =
                   Xml.Element_tag_gat (t_last_opened,t);
                   Xml.contents = S.to_list (test_contents contents) }
      in
      add_content closed_content visited'


(** [add_taggat test_depth t visited] adds t as a node to the content
    of its parent tag. It performs a depth test [test_depth] on the
    number of nested elements allowed.
    @raise Error.E [Error.A_empty_parser_stack]
*)
let add_taggat test_depth t visited =
  let closed_content =
    Xml.Node { Xml.node =  Xml.Element_taggat (t);
               Xml.contents = [] }
  in
  test_depth (add_content closed_content visited)


(** [add_data t visited] adds [t] as a leaf to the contents of its
    parent tag.
    @raise Error.E [Error.A_empty_parser_stack]
*)
let add_data t visited =
  match S.pull visited with
  | None ->
      Message.error (Error.A_empty_parser_stack)
  | Some ((opened_o, content_stack),stack') ->
      S.push (opened_o, (S.push (Xml.Leaf t) content_stack)) stack'


(** [next_lexeme error_status test_depth test_contents visited lexeme]
    is the operation that regroups the above treatments depending on
    the lexeme. It shall be called in a sequential manner to build an
    xml tree upon a stream of lexemes. The behavior of {b DocType}, {b
    CData}, {b PI} and {b comment} is dictated by [error_status]. It
    transmits a depth and a contents test.
    @raise Error.E [Error.A_empty_parser_stack]
    @raise Error.E [Error.XMLP_opening_tag_missing]
*)
let next_lexeme error_status test_depth test_contents visited = function
  | Xml.Tag t ->
      add_tag test_depth t visited
  | Xml.Gat t ->
      add_gat test_contents t visited
  | Xml.Taggat t ->
      add_taggat test_depth t visited
  | Xml.Data (t,_) ->
      add_data t visited
  | Xml.Doctype _ ->
      if error_status.Xml.doctype
      then Message.error (Error.XMLP_SEC_doctype)
      else
        (* warning raised by Xml_lexer *)
        visited
  | Xml.PI _ ->
      if error_status.Xml.pi
      then Message.error (Error.XMLP_SEC_pi)
      else
        (* warning raised by Xml_lexer *)
        visited


(** [parse_next_lexeme config stream] wraps [next_lexeme] so as to
    call it sequentially upon a stream. This function is
    tail-recursive.
    @raise Error.E [Error.A_empty_parser_stack]
    @raise Error.E [Error.XMLP_opening_tag_missing]
    It also raises whatever stream_fun can raise
*)
let parse_next_lexeme config stream =
  let test_depth =
    match config.Xml.max_depth with
    | None -> (fun x -> x)
    | Some i ->
        (fun stack ->
          if i >= S.length stack
          then stack
          else Message.error Error.SEC_depth_limit_reached)
  in
  let test_contents =
    match config.Xml.max_contents with
    | None -> (fun x -> x)
    | Some i ->
        (fun stack ->
          if i >= S.length stack
          then stack
          else Message.error Error.SEC_contents_limit_reached)
  in
  let rec aux visited ((Xml.Stream stream_fun) as stream) =
    let root_element_completed =
      match S.pull visited with
      | Some ((None, v),_) ->
          (* one completed element, we have parsed the root element *)
          S.length v = 1
      | _ -> false
    in
    if root_element_completed
    then
      visited,stream
    else
      match stream_fun () with
      | None ->
          (* nothing left in the stream *)
          visited,stream
      | Some (lexeme, stream') ->
          aux (next_lexeme
                 config.Xml.error_status
                 test_depth
                 test_contents
                 visited
                 lexeme) stream'
  in
  let empty_visited =
    S.push (None,S.empty) S.empty in
  aux empty_visited stream


(** [parse_misc_star] discards comments, PIs and spaces and returns
    the stream beginning with another lexeme that is not among them
    @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    It also raises whatever stream can raise
*)
let parse_misc_star stream : Xml.stream =
  (* We put it as an auxiliary internal function in case we need to
     accumulate the comments, PIs, spaces later on *)
  let rec aux_misc_star ((Xml.Stream stream_fun) as stream) =
    match stream_fun () with
      | Some (Xml.PI _, aux_stream') ->
	  Message.warning Error.XMLP_SEC_pi; aux_misc_star aux_stream'
      | Some (Xml.Data (d,false), aux_stream') ->
          (* Content does not contain any Reference or CDSect, we
             check if its characters are only spaces *)
	if Xml_lexer.is_spaces d
        then
           aux_misc_star aux_stream'
        else
          stream
      | Some (Xml.Data (_,true), _) ->
          stream
      | Some (Xml.Tag _, _ | Xml.Gat _, _ | Xml.Taggat _, _
              | Xml.Doctype _, _)
      | None -> stream
  in
  aux_misc_star stream


(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    It also raises whatever stream_fun can raise
*)
let parse_doctype ((Xml.Stream stream_fun) as stream) =
  match stream_fun () with
    | Some (Xml.Doctype _, stream') ->
      Message.warning Error.XMLP_SEC_doctype;
      parse_misc_star stream'
    | Some (Xml.PI _, _)
    | Some (Xml.Data _, _)
    | Some (Xml.Tag _, _)
    | Some (Xml.Gat _, _)
    | Some (Xml.Taggat _, _)
    | None -> stream


(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    It also raises whatever stream can raise
*)
let parse_prolog stream =
  let passed_misc_stream = parse_misc_star stream in
  let passed_doctype = parse_doctype passed_misc_stream in
  passed_doctype



(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.A_empty_parser_stack]
    @raise Error.E [Error.XMLP_opening_tag_missing]
    @raise Error.E [Error.XMLP_content_after_root_not_allowed]
    @raise Error.E [Error.XMLP_data_before_root_not_allowed]
    @raise Error.E [Error.XMLP_closing_tag_missing]
    @raise Error.E [Error.XMLP_tree_not_recognized]
*)
let parse_element config stream =
  let visited,stream' = parse_next_lexeme config stream in
  match S.to_list visited with
  | [ (None, contents) ] ->
      begin
        match S.to_list contents with
        | [Xml.Node element] ->
            element,stream'
        | (Xml.Leaf t) :: _ ->
            Message.error_at t (Error.XMLP_data_before_root_not_allowed)
        | []
        | _::_::_ ->
            Message.error (Error.XMLP_tree_not_recognized)
      end
  | (Some (t,_), _)::_ ->
      Message.error_at
         t.Xml.local_name
        (Error.XMLP_closing_tag_missing)
  | []
  | (None, _)::_::_ -> Message.error (Error.XMLP_tree_not_recognized)

(** While the handling of tags is pretty regular because of its
    recursive nature, parsing the top requires accurate reactions
    depending on the shape of the prolog. This is what [parse_tree]
    does.
    @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.A_empty_parser_stack]
    @raise Error.E [Error.XMLP_opening_tag_missing]
    @raise Error.E [Error.XMLP_content_after_root_not_allowed]
    @raise Error.E [Error.XMLP_data_before_root_not_allowed]
    @raise Error.E [Error.XMLP_closing_tag_missing]
    @raise Error.E [Error.XMLP_tree_not_recognized]
*)
let parse_tree config (declaration,stream0) =
  let stream1 = parse_prolog stream0 in
  let element,stream2 = parse_element config stream1 in
  let Xml.Stream stream_fun = parse_misc_star stream2 in
  match stream_fun () with
  | None ->
      { Xml.declaration = declaration;
        Xml.root = element }
  | Some _ ->
      Message.error (Error.XMLP_content_after_root_not_allowed)

(******************************************************************************)

(** {2 Tag parsing} *)

let eq_raw_names
    { Xml.prefix = prefix_1;
      Xml.local_name = localname_1 }
    { Xml.prefix = prefix_2;
      Xml.local_name = localname_2 } =
  Lib.opt_eq Stringdata.eq prefix_1 prefix_2 &&
  Stringdata.eq localname_1 localname_2

let compare_raw_names
    { Xml.prefix = prefix_1;
      Xml.local_name = localname_1 }
    { Xml.prefix = prefix_2;
      Xml.local_name = localname_2 } =
  let c = Lib.opt_compare Stringdata.comp prefix_1 prefix_2 in
  if c = 0
  then Stringdata.comp localname_1 localname_2
  else c

let eq_atts (name1,_) (name2,_) =
  eq_raw_names name1 name2

let compare_atts (name1,_) (name2,_) =
  compare_raw_names name1 name2

(** @raise Error.E [Error.XMLL_XMLP_attribute_definition_not_unique]
    @raise Error.E [Error.XMLP_opening_closing_tags_not_matching]
*)
let parse_element_tags test_attributes = function
  | Xml.Element_tag_gat ((name1, attrs),
                         name2) ->
      if eq_raw_names name1 name2
      then { Xml.element_name = name1;
             Xml.attributes =
             test_attributes attrs }
      else
        Message.error_at
          name2.Xml.local_name
          (Error.XMLP_opening_closing_tags_not_matching)
  | Xml.Element_taggat (name, attrs) ->
      { Xml.element_name = name;
        Xml.attributes = test_attributes attrs }

(** @raise Error.E [Error.XMLL_XMLP_attribute_definition_not_unique]
    @raise Error.E [Error.XMLP_opening_closing_tags_not_matching]
*)
let rec parse_content_tags test_attributes test_length = function
  | Xml.Node node ->
      Xml.Node (parse_node_tags test_attributes test_length node)
  | Xml.Leaf sd as leaf ->
      let length =
        Stringdata.byte_length sd
        (*
        List.fold_left
          (fun i sd ->
           i + (Stringdata.byte_length sd))
          0
          l *)
      in
      test_length length;
      leaf


(** @raise Error.E [Error.XMLL_XMLP_attribute_definition_not_unique]
    @raise Error.E [Error.XMLP_opening_closing_tags_not_matching]
*)
and parse_node_tags test_attributes test_length = function
  | { Xml.node = element;
      Xml.contents = contents } ->
        { Xml.node = parse_element_tags test_attributes element;
          Xml.contents =
          List.map (parse_content_tags test_attributes test_length) contents }

(** @raise Error.E [Error.XMLL_XMLP_attribute_definition_not_unique]
    @raise Error.E [Error.XMLP_opening_closing_tags_not_matching]
*)
let parse_tags config tree =
  let test_attributes =
    match config.Xml.max_attributes with
    | None -> (fun x -> x)
    | Some i ->
        (fun attrs ->
          if i >= Xml.M_unexp.cardinal attrs
          then attrs
          else Message.error Error.SEC_attributes_limit_reached)
  in
  let test_length =
    match config.Xml.max_length with
    | None -> (fun _ -> ())
    | Some i ->
        (fun length ->
          if i >= length
          then ()
          else Message.error Error.SEC_length_limit_reached)
  in
  match tree with
  | { Xml.declaration = declaration;
      Xml.root = root } ->
        { Xml.declaration = declaration;
          Xml.root = parse_node_tags test_attributes test_length root }

(** {2 Namespace expansion} *)

let expand_namespaces_attributes ns attrs =
  Xml.M_unexp.fold
    (fun unexp_name value m_exp ->
      let exp_name = Xml_ns.expand_attr unexp_name ns in
      if Xml.M_exp.mem exp_name m_exp
      then
        Message.error_at
          exp_name.Xml.name
          Error.XMLL_XMLP_attribute_definition_not_unique
      else
        (* if the attribute name is 'xml:space' that the
           value is either 'default' or 'preserve' *)
        if Xml_lib.eq_name exp_name Xml_val.e_xml_space
            && not
            (Stringdata.eq value Xml_val.v_xml_space_preserve ||
            Stringdata.eq value Xml_val.v_xml_space_default)
        then
          Message.error_at value Error.XMLP_xml_space_value_invalid;
        Xml.M_exp.add exp_name value m_exp)
    attrs
    Xml.M_exp.empty


(** @raise Error.E [Error.XMLP_namespace_uri_reserved]
    @raise Error.E [Error.XMLP_namespace_prefix_reserved]
    @raise Error.E [Error.XMLP_prefixed_declaration_value_empty]
    @raise Error.E [Error.A_stringdata_comparison_output]
    @raise Error.E [Error.XMLP_attribute_not_recognised]
    @raise Error.E [Error.XMLP_namespace_prefix_not_declared]
*)
let rec expand_namespaces_content ns = function
  | Xml.Node node -> Xml.Node (expand_namespaces_node ns node)
  | Xml.Leaf s -> Xml.Leaf s

(** @raise Error.E [Error.XMLP_namespace_uri_reserved]
    @raise Error.E [Error.XMLP_namespace_prefix_reserved]
    @raise Error.E [Error.XMLP_prefixed_declaration_value_empty]
    @raise Error.E [Error.A_stringdata_comparison_output]
    @raise Error.E [Error.XMLP_attribute_not_recognised]
    @raise Error.E [Error.XMLP_namespace_prefix_not_declared]
*)
and expand_namespaces_node ns = function
  | { Xml.node = { Xml.element_name = element_name;
                   Xml.attributes = attrs };
      Xml.contents = contents } ->
        let ns' = Xml_ns.add_unexp_attrs attrs ns in

        (* we expand the element of the node *)
        let expanded_element =
          Xml_ns.expand_elt element_name ns' in
        { Xml.node = { Xml.element_name =
                       expanded_element;
                       Xml.attributes =
                       expand_namespaces_attributes ns' attrs };
          Xml.contents =
          List.map
            (expand_namespaces_content ns')
            contents }

(** @raise Error.E [Error.XMLP_namespace_uri_reserved]
    @raise Error.E [Error.XMLP_namespace_prefix_reserved]
    @raise Error.E [Error.XMLP_prefixed_declaration_value_empty]
    @raise Error.E [Error.A_stringdata_comparison_output]
    @raise Error.E [Error.XMLP_attribute_not_recognised]
    @raise Error.E [Error.XMLP_namespace_prefix_not_declared]
*)
let expand_namespaces = function
  | { Xml.declaration = declaration;
      Xml.root = root }
    ->
      { Xml.declaration = declaration;
        Xml.root = expand_namespaces_node Xml_ns.default root }


(** {2 XML key ({b xml:id}) unicity checking} *)

let check_xml_id t =
  Message.warning Error.XMLP_LIM_xml_id_unicity_not_implemented;
  t

(** {2 XML parser} *)

(** @raise Error.E [Error.UTF8_invalid]
    @raise Sys_error [string]
    @raise End_of_file ([Stringdata])
    @raise Error.E [Error.A_empty_parser_stack]
    @raise Error.E [Error.XMLP_opening_tag_missing]
    @raise Error.E [Error.XMLP_content_after_root_not_allowed]
    @raise Error.E [Error.XMLP_data_before_root_not_allowed]
    @raise Error.E [Error.XMLP_closing_tag_missing]
    @raise Error.E [Error.XMLP_tree_not_recognized]
    @raise Error.E [Error.XMLL_XMLP_attribute_definition_not_unique]
    @raise Error.E [Error.XMLP_opening_closing_tags_not_matching]
    @raise Error.E [Error.XMLP_namespace_uri_reserved]
    @raise Error.E [Error.XMLP_namespace_prefix_reserved]
    @raise Error.E [Error.XMLP_prefixed_declaration_value_empty]
    @raise Error.E [Error.A_stringdata_comparison_output]
    @raise Error.E [Error.XMLP_attribute_not_recognised]
    @raise Error.E [Error.XMLP_namespace_prefix_not_declared]
*)
let parse config sd =
  let raw_tree =
    parse_tree config sd in
  let unexpanded_tree =
    parse_tags config raw_tree in
  let expanded_tree =
    expand_namespaces unexpanded_tree in
  check_xml_id expanded_tree
