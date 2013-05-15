(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_ns.ml 1686 2012-05-30 09:00:22Z maarek $ *)

(** {1 XML namespaces manager} *)

module M_ns =
  Map.Make (struct
    type t = Stringdata.t option
    let compare = Lib.opt_compare Stringdata.comp
  end)

type t = Stringdata.t M_ns.t

let default =
  M_ns.add (Some Xml_val.xml_namespace_prefix) Xml_val.xml_namespace
    (M_ns.add (Some Xml_val.xmlns_namespace_prefix) Xml_val.xmlns_namespace
       M_ns.empty)

let from_list l =
  List.fold_left
    (fun m (opt_prefix,uri) -> M_ns.add opt_prefix uri m)
    M_ns.empty
    l

(** @raise Error.E [Error.XMLNS_namespace_uri_reserved]
    @raise Error.E [Error.XMLNS_namespace_prefix_reserved]
 *)
let check_for_reserved_namespace (prefix,url) =
  if
    (* XML's uri without XML's prefix *)
    (Stringdata.eq url Xml_val.xml_namespace &&
     (match prefix with
       None -> true
     | Some prefix_sd ->
         not (Stringdata.eq prefix_sd  Xml_val.xml_namespace_prefix))) ||
    (* XMLNS's URI *)
    Stringdata.eq url Xml_val.xmlns_namespace
  then Message.error_at url (Error.XMLNS_namespace_uri_reserved)
  else
    if
      (* XML's prefix without XML's uri *)
      ( Lib.opt_eq Stringdata.eq prefix (Some Xml_val.xml_namespace_prefix)
          && not (Stringdata.eq url Xml_val.xml_namespace) ) ||
      (* XMLNS's prefix *)
        Lib.opt_eq Stringdata.eq prefix (Some Xml_val.xmlns_namespace_prefix)
    then Message.error_at url (Error.XMLNS_namespace_prefix_reserved)
    else
      begin
        if Xml_re.test_uri url
        then
          (prefix,url)
        else
          Message.error_at url Error.XMLNS_XSDT_uri_not_well_formed
      end

(** Add the 'xmlns' prefix to the attribute 'xmlns' (this is actually
    not properly specified in
    {{:http://www.w3.org/TR/REC-xml-names/}XML Namespaces}). Return
    the attribute itself otherwise.
*)
let add_xmlns_prefix = function
  | { Xml.prefix = None;
      Xml.local_name = localname } as name ->
        if Stringdata.eq localname Xml_val.xmlns_namespace_prefix
        then { Xml.prefix = Some Xml_val.xmlns_namespace_prefix;
               Xml.local_name = localname }
        else
          name
  | { Xml.prefix = Some _;
      Xml.local_name = _ } as name ->
        name

(** Test if a given attribute name is reserved. Namely that 'xml:id',
    'xml:lang' 'xml:base', 'xml:space' are the only attributes with
    'xml' as prefix.
*)
let test_reserved_attribute = function
  | { Xml.prefix = None;
      Xml.local_name = _ } ->
        false
  | { Xml.prefix = Some prefix;
      Xml.local_name = localname } ->
        Stringdata.eq prefix Xml_val.xml_namespace_prefix
          && not (Stringdata.eq localname Xml_val.xml_id_localname)
          && not (Stringdata.eq localname Xml_val.xml_lang_localname)
          && not (Stringdata.eq localname Xml_val.xml_base_localname)
          && not (Stringdata.eq localname Xml_val.xml_space_localname)

(** @raise Error.E [Error.XMLNS_namespace_uri_reserved]
    @raise Error.E [Error.XMLNS_namespace_prefix_reserved]
    @raise Error.E [Error.XMLNS_prefixed_declaration_value_empty]
 *)
let namespace_of_ns_unexp_att name value =
  match name with
    { Xml.prefix = None; Xml.local_name = _ } ->
      check_for_reserved_namespace (None, value)
  | { Xml.prefix = Some _; Xml.local_name = localname } ->
      if Stringdata.empty value
      then
        Message.error_at value Error.XMLNS_prefixed_declaration_value_empty
      else
        check_for_reserved_namespace ((Some localname), value)

(** @raise Error.E [Error.XMLNS_namespace_uri_reserved]
    @raise Error.E [Error.XMLNS_namespace_prefix_reserved]
    @raise Error.E [Error.XMLNS_prefixed_declaration_value_empty]
 *)
let namespace_of_ns_exp_att { Xml.uri = uri; Xml.name = name } value =
  if Stringdata.eq uri Xml_val.xmlns_namespace
  then
    if Stringdata.eq name Xml_val.xmlns_namespace_prefix
    then (None,value)
    else (Some name, value)
  else Message.error (Error.A_function "namespace_of_ns_exp_att")

let add_unexp_attrs attrs pre_m_ns =
  let attrs_ns =
    Xml.M_unexp.filter
      (* we differentiate XMLNS related attributes from others *)
      (fun { Xml.prefix = opt_prefix; Xml.local_name = localname } _ ->
        match opt_prefix with
        | None ->
            Stringdata.eq localname Xml_val.xmlns_namespace_prefix
        | Some prefix ->
            Stringdata.eq prefix Xml_val.xmlns_namespace_prefix)
      attrs in
  let new_ns =
    Xml.M_unexp.fold
      (fun name value m_ns ->
        (* we get the namespace declarations *)
        let (opt_prefix,uri) = namespace_of_ns_unexp_att name value in
        (* we extend the list of namespaces in the scope of this node *)
        M_ns.add opt_prefix uri m_ns)
      attrs_ns
      pre_m_ns
       in
  new_ns


let add_exp_attrs attrs pre_m_ns =
  let attrs_ns =
    Xml.M_exp.filter
      (* we diferentiate XMLNS related attributes from others *)
      (fun { Xml.uri = uri; Xml.name = _ } _ ->
        Stringdata.eq uri Xml_val.xmlns_namespace)
      attrs in
  let new_ns =
    Xml.M_exp.fold
      (fun name value m_ns ->
        (* we get the namespace declarations *)
        let (opt_prefix,uri) = namespace_of_ns_exp_att name value in
        (* we extend the list of namespaces in the scope of this node *)
        M_ns.add opt_prefix uri m_ns)
      attrs_ns
      pre_m_ns in
  new_ns

(** Namespace URI value for non qualified names after expansion. *)
let empty_expanded_ns =
  Stringdata.of_string ""

(** [expand_opt is_element name ns_map] expands the name [name]
    according to the map of namespaces [ns_map]. [is_element] is true if
    this is an element name and false if this is an attribute name. *)
let expand_opt is_element name m_ns =
  (* we first add "xmlns" as prefix forme "xmlns" attributes
     turning it into "xmlns:xmlns" *)
  let name' = add_xmlns_prefix name in
  if (not is_element) && test_reserved_attribute name'
  then
    (* attribute name which has "xml" as prefix and an invalid local name *)
    None
  else
    match name' with
    | { Xml.prefix = prefix;
        Xml.local_name = localname } ->
          if (not is_element) && prefix = None
          then
            (* attribute name with no prefix,
               indicates no namespace.
               "The namespace name for an unprefixed attribute name
               always has no value."
             *)
            Some { Xml.uri = empty_expanded_ns;
                   Xml.name = localname }
          else
            try
              (* element name or attribute name with prefix,
                 we aare searching for the namespace in the map
               *)
              Some { Xml.uri = M_ns.find prefix m_ns;
	             Xml.name = localname }
            with
            | Not_found ->
                if is_element && prefix = None
                then
                  (* element name without and no default namespace,
                     the namepsace is empty *)
                  Some { Xml.uri = empty_expanded_ns;
	                 Xml.name = localname }
                else
                  (* name with a non declared prefix *)
                  None

(** @raise Error.E [Error.XMLNS_attribute_not_recognised]
    @raise Error.E [Error.XMLNS_namespace_prefix_not_declared]
 *)
let expand is_element name m_ns =
  match expand_opt is_element name m_ns with
  | None ->
      if (not is_element) && test_reserved_attribute name
      then
        Message.error_at
          name.Xml.local_name
          (Error.XMLNS_XMLP_xml_attribute_not_recognised)
      else
        Message.error_at
          name.Xml.local_name
          (Error.XMLNS_namespace_prefix_not_declared)
  | Some expanded_name ->
      expanded_name


let expand_elt_opt name m_ns =
  expand_opt true name m_ns

let expand_elt name m_ns =
  expand true name m_ns

let expand_attr_opt name m_ns =
  expand_opt false name m_ns

let expand_attr name m_ns =
  expand false name m_ns
