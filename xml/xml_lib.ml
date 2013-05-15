(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_lib.ml 1648 2012-05-16 14:11:36Z maarek $ *)

(** {1 XML library} *)


(** {2 Functions} *)

let is_silent_mode xml_config =
  xml_config = Xml_val.default_xml_configuration


let at_content = function
  | Xml.Node n -> n.Xml.node.Xml.element_name.Xml.name;
  | Xml.Leaf sd -> sd

let is_predefined_attr { Xml.uri = uri; Xml.name = name } =
  if Stringdata.eq uri Xml_val.xml_namespace
  then
    if
      Stringdata.eq name Xml_val.xml_id_localname ||
      Stringdata.eq name Xml_val.xml_lang_localname  ||
      Stringdata.eq name Xml_val.xml_base_localname  ||
      Stringdata.eq name Xml_val.xml_space_localname
    then
      true
    else
      Message.error_at name Error.XMLNS_XMLP_xml_attribute_not_recognised
  else
    if Stringdata.eq uri Xml_val.xmlns_namespace
    then
      true
    else
      false


let eq_name n1 n2 =
  match n1, n2 with
  | { Xml.uri = u1;
      Xml.name = ln1 },
    { Xml.uri = u2;
      Xml.name = ln2 } ->
    Stringdata.eq ln1 ln2 &&
    Stringdata.eq u1 u2

let eq_uri n1 n2 =
  Stringdata.eq
    n1.Xml.uri
    n2.Xml.uri



let eq_content_elt_uri uri = function
  | Xml.Node
      { Xml.node =
        { Xml.element_name = elt_name;
          Xml.attributes = _ };
        Xml.contents = _ } ->
        Stringdata.eq uri elt_name.Xml.uri
  | Xml.Leaf _ ->
      false

let get_attribute_value_and_reduce_attrs name attrs =
  try
    let v = Xml.M_exp.find name attrs in
    (Some v), (Xml.M_exp.remove name attrs)
  with
  | Not_found ->
      None,attrs

let has_attr uri attrs =
  Xml.M_exp.exists
    (fun name _ ->
      Stringdata.eq uri name.Xml.uri)
    attrs

let has_elt uri elt_list =
    Lib.list_mem
    (eq_content_elt_uri uri)
    elt_list

let has_only_xml_xmlns_attrs attrs =
  Xml.M_exp.for_all
    (fun name _ ->
      is_predefined_attr name)
    attrs

(** Get the value of an optional attribute and remove it from the
    given list of attributes. *)
let get_attr_optional attr_name attrs =
  get_attribute_value_and_reduce_attrs attr_name attrs

(** Get the value of a required attribute and remove it from the
    given list of attributes. *)
let get_attr_require attr_name loc attrs = 
  match get_attribute_value_and_reduce_attrs attr_name attrs with
  | None,_ ->
      Message.error_at
        loc
        (Error.XMLP_attribute_required
           (Stringdata.to_string attr_name.Xml.name))
  | (Some t),l -> (t,l)

(** Get the value of an attribute, return the given default value if
    not present, and remove it from the given list of attributes. *)
let get_attr_default attr_name default attrs = 
  match get_attribute_value_and_reduce_attrs attr_name attrs with
  | None,l ->
      default,l
  | (Some t),l -> (t,l)

(** Check the presence of an ignored attribute. If present, send a
    warning and remove it from the given list of attributes. *)
let get_attr_ignore attr_name attrs =
  match get_attribute_value_and_reduce_attrs attr_name attrs with
  | None,l ->
      (None,l)
  | (Some loc),l ->
      Message.warning_at
        loc
        (Error.XMLP_LIM_attribute_ignored
           (Stringdata.to_string attr_name.Xml.name));
      (None,l)

(** Check the presence of an ignored attribute. If present,
    raise an error. *)
let get_attr_error attr_name attrs =
  match get_attribute_value_and_reduce_attrs attr_name attrs with
  | None,l ->
      (None,l)
  | (Some loc),_ ->
      Message.error_at
        loc
        (Error.XMLP_LIM_attribute_not_implemented
           (Stringdata.to_string attr_name.Xml.name))

let get_space attrs =
  try
    if Stringdata.eq
        Xml_val.v_xml_space_preserve
        (Xml.M_exp.find Xml_val.e_xml_space attrs)
    then Xml.Preserve
    else Xml.Default
  with
  | Not_found ->
      Xml.Default

let clear_space_leafs space l =
  let rec clear cont = function
    | [] ->
        cont []
    | ((Xml.Leaf sd) as leaf)::t ->
        if Xml_lexer.is_spaces sd
        then
          clear (fun r -> cont r) t
        else
          clear (fun r -> cont (leaf::r)) t
    | ((Xml.Node _) as node)::t ->
        clear (fun r -> cont (node::r)) t
  in
  match space with
  | Xml.Preserve ->
      l
  | Xml.Default ->
      clear (fun x -> x) l


let clear_leafs l =
  List.filter
    (function
      | Xml.Node _ -> true
      | Xml.Leaf _ -> false)
    l


let next_elt expected_name = function
  | Xml.Node
      { Xml.node = { Xml.element_name = e_name;
                     Xml.attributes = attrs };
        Xml.contents = contents } ->
          if not (eq_name e_name expected_name)
          then
            None
          else
            let loc = e_name.Xml.name in
            Some (loc,attrs,contents)
  | Xml.Leaf _ -> None

let next_elt_required expected_name content =
  match next_elt expected_name content with
  | None ->
      Message.error_at (at_content content)
        (Error.XMLP_element_expected
           (Stringdata.to_string expected_name.Xml.name))
  | Some r -> r

let next_elt_ignore expected_name content =
  match next_elt expected_name content with
  | None -> None
  | (Some _) as r ->
      Message.warning_at (at_content content)
        (Error.XMLP_LIM_element_ignored
           (Stringdata.to_string expected_name.Xml.name));
      r


let next_elt_error expected_name content =
  let opt = next_elt expected_name content in
  match opt with
  | None -> None
  | Some (name,_,_) -> Message.error_at name
        (Error.XMLP_LIM_element_not_implemented
           (Stringdata.to_string expected_name.Xml.name))


(** {2 Error messages} *)

let to_string {Xml.uri = _; Xml.name = name} =
  "'" ^ Stringdata.to_string name ^ "'"
