(***********************************************************************)
(*                                                                     *)
(*                  Xml Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd_transducer.ml 1708 2012-06-04 17:12:45Z maarek $ *)

(** {1 Transducer from XML to XSD components} *)

(** {2 Types} *)

(** Type for transducers' default information. *)
type default =
    { target : Xsd.uri option; (** Default URI. *)
      qualified_form_element : Xsd.form; (** Default form for elements *)
      qualified_form_attribute : Xsd.form; (** Default form for attribute *)
    }

type env =
    { default : default; (** Default information *)
      ns : Xml_ns.t (** List of declared namespace prefixes *)
    }


let update_env_ns attrs env =
  match env with
  | { default = default;
      ns = ns } ->
        { default = default;
          ns = Xml_ns.add_exp_attrs attrs ns }

(** Return the target of a local element according to the schema's
    default settings and the local target and form settings. *)
let local_target_element
    env local_targetNamespace local_form :
    Xsd.uri option =
  match local_targetNamespace with
  | None ->
      let form_opt = Xsd_lib.read_form local_form in
      if
        (form_opt = None &&
         env.default.qualified_form_element = Xsd.Qualified) ||
        form_opt = Some Xsd.Qualified
      then
        env.default.target
      else
        (* Returning the empty namespace which corresponds
           to being not qualified *)
        Some (Xsd.Uri (Stringdata.of_string ""))
  | Some uri ->
      Some (Xsd_lib.to_uri uri)

(** Return the target of a local attribute according to the schema's
    default settings and the local target and form settings. *)
let local_target_attribute
    env local_targetNamespace local_form :
    Xsd.uri option =
  match local_targetNamespace with
  | None ->
      let form_opt = Xsd_lib.read_form local_form in
      if
        (form_opt = None &&
         env.default.qualified_form_attribute = Xsd.Qualified) ||
        form_opt = Some Xsd.Qualified
      then
        env.default.target
      else
        (* Returning the empty namespace which corresponds
           to being not qualified *)
        Some (Xsd.Uri (Stringdata.of_string ""))
  | Some uri ->
      Some (Xsd_lib.to_uri uri)


(** Expand a given {b QName} according to the schema's list of
    namespaces.  (resolution
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#src-resolve}REF})

    We do not check case 4.2 as we do not have access to the import's
    namespaces. This verification is performed by the lineariser.

    @raise Message.E [Error.XSDT_qname_not_well_formed]
    *)
let expand_qname env qname =
  match Xml_lexer.lex_qname_ns (env.ns,qname) with
  | None ->
      Message.error_at qname Error.XSDT_qname_not_well_formed
  | Some expanded_name ->
      if Stringdata.empty expanded_name.Xml.uri
      then
        if env.default.target = None
        then expanded_name
        else
          Message.error_at qname Error.XSDT_qname_namespace_missing
      else
        (* We do not check the namespace *)
        expanded_name

(** Return an occurrence specification out of {b minOccurs} and {b
    maxOccurs} optional values. The default is {b minOccurs = 1} and
    {b maxOccurs = 1} except for the predefined {b xs:anyType} type.*)
let occurrence_spec min_sd_opt max_sd_opt : Xsd.occurrence =
  let min =
    match min_sd_opt with
    | None -> 1
    | Some min_sd -> Xsd_lib.to_posint min_sd in
  let max =
    match max_sd_opt with
    | None -> Xsd.Occur 1
    | Some max_sd -> Xsd_lib.to_posint_unbounded max_sd in
  begin
    match max with
    | Xsd.Occur max_val ->
        if min > max_val
        then
          Message.warning Error.XSDT_min_max_inconsistent
    | Xsd.Unbounded -> ()
  end;
  { Xsd.min_occurs = min;
    Xsd.max_occurs = max }

(** {2 Type wrappers} *)

let wrap cast f =
  fun a -> Lib.opt_apply cast (f a)

let wrap_none f =
  fun a ->
    match f a with
    | None -> None
    | Some _ -> Some None

let wrap_some f =
  fun a ->
    match f a with
    | None -> None
    | Some r -> Some (Some r)


let xsd_std std =
  Xsd.STD std

let xsd_td_std std =
  Xsd.TD (Xsd.STD std)

let xsd_td td =
  Xsd.TD td

let xsd_mgd mgd =
  Xsd.MGD mgd

let xsd_agd agd =
  Xsd.AGD agd

let xsd_ed ed =
  Xsd.ED ed

let xsd_ad ad =
  Xsd.AD ad

let ct_mixed mixed p =
  if mixed
  then Xsd.CT_mixed p
  else Xsd.CT_element_only p

(** {2 Shared functions} *)


let std_ref_of_qname env qname =
  let expanded_name =
    expand_qname env qname
  in
  if Xml_lib.eq_name expanded_name Xsd_val.st_anySimpleType
  then Xsd.STD_built_in Xsd.STD_anySimpleType
  else if Xml_lib.eq_name expanded_name Xsd_val.st_anyAtomicType
  then Xsd.STD_built_in Xsd.STD_anyAtomicType
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_string
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_string)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_boolean
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_boolean)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_decimal
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_decimal)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_float
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_float)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_double
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_double)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_duration
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_duration)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_dateTime
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_dateTime)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_time
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_time)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_date
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_date)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_gYearMonth
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_gYearMonth)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_gYear
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_gYear)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_gMonthDay
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_gMonthDay)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_gDay
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_gDay)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_gMonth
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_gMonth)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_hexBinary
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_hexBinary)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_base64Binary
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_base64Binary)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_anyURI
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_anyURI)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_QName
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_QName)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_pt_NOTATION
  then Xsd.STD_built_in (Xsd.STD_primitive_type Xsd.PT_NOTATION)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_normalizedString
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_normalizedString)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_token
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_token)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_language
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_language)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_IDREFS
  then
    begin
      Message.warning_at expanded_name.Xml.name
        Error.XSDT_LIM_id_idref_unicity_reference_not_implemented;
      Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_IDREFS)
    end
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_ENTITIES
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_ENTITIES)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_NMTOKEN
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_NMTOKEN)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_NMTOKENS
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_NMTOKENS)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_Name
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_Name)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_NCName
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_NCName)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_ID
  then
    begin
      Message.warning_at expanded_name.Xml.name
        Error.XSDT_LIM_id_idref_unicity_reference_not_implemented;
      Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_ID)
    end
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_IDREF
  then
    begin
      Message.warning_at expanded_name.Xml.name
        Error.XSDT_LIM_id_idref_unicity_reference_not_implemented;
      Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_IDREF)
    end
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_ENTITY
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_ENTITY)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_integer
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_integer)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_nonPositiveInteger
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_nonPositiveInteger)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_negativeInteger
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_negativeInteger)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_long
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_long)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_int
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_int)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_short
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_short)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_byte
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_byte)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_nonNegativeInteger
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_nonNegativeInteger)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_unsignedLong
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_unsignedLong)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_unsignedInt
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_unsignedInt)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_unsignedShort
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_unsignedShort)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_unsignedByte
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_unsignedByte)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_positiveInteger
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_positiveInteger)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_yearMonthDuration
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_yearMonthDuration)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_dayTimeDuration
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_dayTimeDuration)
  else if Xml_lib.eq_name expanded_name Xsd_val.st_npt_dateTimeStamp
  then Xsd.STD_built_in (Xsd.STD_non_primitive_type Xsd.NPT_dateTimeStamp)
  else
    Xsd.STD_td_ref (Xsd.Ref expanded_name)

let ctd_ref_of_qname env qname =
  let expanded_name =
    expand_qname env qname
  in
  if Xml_lib.eq_name expanded_name Xsd_val.ct_anyType
  then Xsd.CTD_anyType
  else
    match std_ref_of_qname env qname with
    | Xsd.STD_td_ref _ ->
        (* We do not know if the qname refers to a simple or complex type *)
        Xsd.CTD_td_ref (Xsd.Ref expanded_name)
    (* In the following cases we know that the qname refers to a simple type *)
    | Xsd.STD_built_in st ->
        Xsd.CTD_built_in_simpleType st


let get_ed_td type_opt val_attr_type_opt env loc =
  match type_opt, val_attr_type_opt with
  | None, None ->
      Xsd.CTD_anyType
  | None, (Some qname) ->
      ctd_ref_of_qname env qname
  | (Some td), None ->
      Xsd.CTD_td_ref (Xsd.Val td)
  | (Some _), (Some _) ->
      Message.error_at loc
        (Error.XSDT_constructions_mutually_exclusive
           ("attribute " ^ Xml_lib.to_string Xsd_val.a_ref ^
            " and sub-element " ^ Xml_lib.to_string Xsd_val.e_simpleType ^
            " of " ^ Xml_lib.to_string Xsd_val.e_complexType))

(** {2 Transducers} *)

let transduce_include content : Xsd.composition option =
  match Xml_lib.next_elt_error Xsd_val.e_include content with
  | None -> None
  | Some _ -> None

let transduce_redefine content : Xsd.composition option =
  match Xml_lib.next_elt_error Xsd_val.e_redefine content with
  | None -> None
  | Some _ -> None

let transduce_override content : Xsd.composition option =
  match Xml_lib.next_elt_error Xsd_val.e_include content with
  | None -> None
  | Some _ -> None

let transduce_import content : Xsd.composition option =
  match Xml_lib.next_elt Xsd_val.e_import content with
  | None ->
      None
  | Some (loc,attrs,contents) ->
      let val_attr_namespace_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_namespace attrs in
      let val_attr_schemaLocation_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_schemaLocation attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_import));
      match val_attr_schemaLocation_opt with
      | None ->
          Message.error_at loc
            Error.XSDT_LIM_schema_location_required
      | Some val_attr_schemaLocation ->
          (Some (Xsd.Import (val_attr_namespace_opt, val_attr_schemaLocation)))

let transduce_documentation content =
  match Xml_lib.next_elt_ignore Xsd_val.e_documentation content with
  | None -> None
  | Some _ -> Some None


let transduce_appinfo content =
  match Xml_lib.next_elt Xsd_val.e_appinfo content with
  | None -> None
  | Some (loc,_,_) ->
      Message.error_at loc Error.XSDT_SEC_appinfo_not_allowed

let transduce_annotation content =
  match Xml_lib.next_elt_ignore Xsd_val.e_annotation content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let _val_attr_id_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _details,contents =
        Lib.list_next_remove
        [ transduce_appinfo;
          transduce_documentation ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_annotation));
      (* We do not return anything but we check *)
      Some None


let transduce_alternative content =
  match Xml_lib.next_elt_error Xsd_val.e_alternative content with
  | None -> None
  | Some _ -> None

let transduce_unique content =
  match Xml_lib.next_elt_error Xsd_val.e_unique content with
  | None -> None
  | Some _ -> None

let transduce_key content =
  match Xml_lib.next_elt_error Xsd_val.e_key content with
  | None -> None
  | Some _ -> None

let transduce_keyref content =
  match Xml_lib.next_elt_error Xsd_val.e_keyref content with
  | None -> None
  | Some _ -> None

let transduce_openContent content =
  match Xml_lib.next_elt_error Xsd_val.e_defaultOpenContent content with
  | None -> None
  | Some _ -> None

let transduce_defaultOpenContent content =
  match Xml_lib.next_elt_error Xsd_val.e_defaultOpenContent content with
  | None -> None
  | Some _ -> None

let transduce_assert content =
  match Xml_lib.next_elt_error Xsd_val.e_assert content with
  | None -> None
  | Some _ -> None

let transduce_any env0 content :
    Xsd.uri option Xsd.p option =
  match Xml_lib.next_elt Xsd_val.e_any content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_id_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_maxOccurs_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_maxOccurs attrs in
      let val_attr_minOccurs_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_minOccurs attrs in
      let val_attr_namespace_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_namespace attrs in
      let __val_attr_notNamespace_opt,attrs =
        Xml_lib.get_attr_error Xsd_val.a_notNamespace attrs in
      let __val_attr_notQName_opt,attrs =
        Xml_lib.get_attr_error Xsd_val.a_notQName attrs in
      let val_attr_processContents_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_processContents attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_any));
      let occurrence =
        occurrence_spec
          val_attr_minOccurs_opt
          val_attr_maxOccurs_opt
      in
      let namespace_opt =
        (* to be extended with val_attr_notQName when treated *)
        match val_attr_namespace_opt with
        | None -> Xsd.Any
        | Some v ->
            Xsd_lib.read_nc_variety v
      in
      let nc_disallowed =
        (* to be extended with val_attr_notQName when treated *)
        []
      in
      let nc =
        match namespace_opt with
        | Xsd.Any ->
            Xsd.NC_any nc_disallowed
        | Xsd.Not ->
            let nc_namespaces =
              if env.default.target = None
              then [None]
              else [None; Some (env.default.target)]
            in
            Xsd.NC_not (nc_namespaces,nc_disallowed)
        | Xsd.Enumeration ->
            let nc_namespaces =
              match val_attr_namespace_opt with
              | None ->
                  Message.error_at loc
                    (Error.A_function "Xsd_transducer.transduce_any")
              | Some sd ->
                  List.map
                    (Xsd_lib.read_nc_list_item env.default.target)
                    (Xml_lexer.lex_list sd)
            in
            Xsd.NC_enumeration (nc_namespaces,nc_disallowed)
      in
      let process_contents =
        match val_attr_processContents_opt with
        | None -> Xsd.Strict
        | Some pc -> Xsd_lib.read_processContents pc
      in
      if process_contents = Xsd.Skip
      then Message.warning_at loc Error.XSDT_skip_process_contents_risky;
      Some
        { Xsd.p_occurrence = occurrence;
          Xsd.p_term = Xsd.T_w
            { Xsd.w_nc = nc;
              Xsd.w_pc = process_contents } }



(** {3 Facets} *)

(** Generic transducer of a {b constraining facet} with attributes {b
    fixed}, {b id} and {b value}. If the attribute {b fixed} is not
    present, its default value {b false} is returned. The first
    parameter of [transduce_generic_cfacet] is the facet's element
    name, the second is a wrapper for the output. *)
let transduce_generic_cfacet xsd_e return content : 'a option =
  match Xml_lib.next_elt xsd_e content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let val_attr_fixed,attrs =
        Xml_lib.get_attr_default Xsd_val.a_fixed Xsd_val.v_false attrs in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_value,attrs =
        Xml_lib.get_attr_require Xsd_val.a_value Xsd_val.v_false attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string xsd_e));
     Some (return
             (val_attr_value,
              (Xsd_lib.read_boolean val_attr_fixed)))


let transduce_minExclusive content : Xsd.cfacet option =
  transduce_generic_cfacet
    Xsd_val.e_minExclusive
    (fun (v,f) -> (Xsd.CF_minExclusive (v,f)))
    content

let transduce_minInclusive content : Xsd.cfacet option =
  transduce_generic_cfacet
    Xsd_val.e_minInclusive
    (fun (v,f) -> (Xsd.CF_minInclusive (v,f)))
    content

let transduce_maxExclusive content : Xsd.cfacet option =
  transduce_generic_cfacet
    Xsd_val.e_maxExclusive
    (fun (v,f) -> (Xsd.CF_maxExclusive (v,f)))
    content

let transduce_maxInclusive content : Xsd.cfacet option =
  transduce_generic_cfacet
    Xsd_val.e_maxInclusive
    (fun (v,f) -> (Xsd.CF_maxInclusive (v,f)))
    content

let transduce_totalDigits content : Xsd.cfacet option =
  match Xml_lib.next_elt_error Xsd_val.e_totalDigits content with
  | None -> None
  | Some _ -> None

let transduce_fractionDigits content : Xsd.cfacet option =
  match Xml_lib.next_elt_error Xsd_val.e_fractionDigits content with
  | None -> None
  | Some _ -> None

let transduce_length content : Xsd.cfacet option =
  transduce_generic_cfacet
    Xsd_val.e_length
    (fun (v,f) ->
      (Xsd.CF_length (((Xsd_lib.to_nonNegativeInteger v),f))))
    content

let transduce_minLength content : Xsd.cfacet option =
  transduce_generic_cfacet
    Xsd_val.e_minLength
    (fun (v,f) ->
      (Xsd.CF_minLength (((Xsd_lib.to_nonNegativeInteger v),f))))
    content

let transduce_maxLength content : Xsd.cfacet option =
  transduce_generic_cfacet
    Xsd_val.e_maxLength
    (fun (v,f) ->
      (Xsd.CF_maxLength (((Xsd_lib.to_nonNegativeInteger v),f))))
    content

let transduce_enumeration content : Xsd.cfacet option =
  match Xml_lib.next_elt Xsd_val.e_enumeration content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_value,attrs =
        Xml_lib.get_attr_require Xsd_val.a_value Xsd_val.v_false attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_enumeration));
      Some (Xsd.CF_enumeration val_attr_value)

let transduce_whiteSpace content : Xsd.cfacet option =
  transduce_generic_cfacet
    Xsd_val.e_whiteSpace
    (fun (v,f) ->
      (Xsd.CF_whiteSpace (((Xsd_lib.read_whiteSpace v),f))))
    content

let transduce_pattern content : Xsd.cfacet option =
  transduce_generic_cfacet
    Xsd_val.e_pattern
    (fun (v,f) ->
      (Xsd.CF_pattern (((Xsd_lib.read_pattern v),f))))
    content

let transduce_assertion content : Xsd.cfacet option =
  match Xml_lib.next_elt_error Xsd_val.e_assertion content with
  | None -> None
  | Some _ -> None

let transduce_explicitTimezone content : Xsd.cfacet option =
  match Xml_lib.next_elt_error Xsd_val.e_explicitTimezone content with
  | None -> None
  | Some _ -> None


let combine_names_or_context = function
  | ((Xsd.CTD_name _) as n,None) -> n
  | ((Xsd.CTD_name (Xsd.Ncname n)),Some name) ->
      (* TODO check if this case could occure *)
      Message.warning_at n Error.XSDT_multiple_name_for_type_definition;
      Message.warning_at name Error.XSDT_multiple_name_for_type_definition;
      Xsd.CTD_name (Xsd_lib.to_ncname Xml_lexer.is_ncname name)
  | (Xsd.CTD_context,None) -> Xsd.CTD_context
  | (Xsd.CTD_context,Some name) -> (Xsd.CTD_name (Xsd_lib.to_ncname Xml_lexer.is_ncname name))


(** Determine the effective mixed.
   Case 1 in {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#dcl.ctd.ctcc.common}REF}. *)
let combine_ct_mixed ct sd_mixed_1 sd_mixed_2 =
  let mixed_b =
    match sd_mixed_1,sd_mixed_2 with
    | None,None -> false
    | (Some sd),None
    | None,(Some sd) ->
        Xsd_lib.read_boolean sd
    | (Some sd_1),(Some sd_2) ->
        let b_1 = Xsd_lib.read_boolean sd_1 in
        let b_2 = Xsd_lib.read_boolean sd_2 in
        if b_1 = b_2
        then b_1
        else Message.error_at sd_2
            Error.XSDT_mixed_values_contradictory
  in
  match ct with
  | Xsd.CT_empty
  | Xsd.CT_simple _ ->
      ct
  | Xsd.CT_element_only p ->
      if mixed_b
      then
        Xsd.CT_mixed p
      else
        Xsd.CT_element_only p
  | Xsd.CT_mixed _ ->
      ct

let combine_mixed_name_in_td ctd name_opt sd_mixed_1 sd_mixed_2 =
  match ctd with
  | Xsd.STD _ ->
      Message.error (Error.A_function "Xsd_transducer.combine_mixed_name_in_td")
  | Xsd.CTD { Xsd.ctd_name_or_context = ctd_name_or_context;
              Xsd.ctd_target = target;
              Xsd.ctd_base = base;
              Xsd.ctd_dm = dm;
              Xsd.ctd_au = au_list;
              Xsd.ctd_aw = aw_opt;
              Xsd.ctd_ct = ct } ->
                { Xsd.ctd_name_or_context =
                  combine_names_or_context
                    (ctd_name_or_context,name_opt);
                  Xsd.ctd_target = target;
                  Xsd.ctd_base = base;
                  Xsd.ctd_dm = dm;
                  Xsd.ctd_au = au_list;
                  Xsd.ctd_aw = aw_opt;
                  Xsd.ctd_ct = combine_ct_mixed ct sd_mixed_1 sd_mixed_2 }



let rec transduce_attributeGroup_generic global env0 content :
    ('a Xsd.agd, 'a Xsd.aus) Lib.either option =
  match Xml_lib.next_elt Xsd_val.e_attributeGroup content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_id_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_name_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_name attrs in
      let val_attr_ref_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_ref attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let attr_uses,contents =
        Lib.list_next_remove
          [ transduce_attribute_local env;
            transduce_attributeGroup_local env ]
          contents in
      let any_attr_opt,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_anyAttribute ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_attributeGroup));
      let val_ncname_opt =
        Lib.opt_apply
          (Xsd_lib.to_ncname Xml_lexer.is_ncname)
          val_attr_name_opt in
      let val_ref_opt =
        Lib.opt_apply (expand_qname env) val_attr_ref_opt in
      (** There are two cases, mutually exclusive.

         These two cases are implicitly defined in
         {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#declare-attributeGroup-core}REF}.

          The second case correspond to {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#dcl.ctd.attuses}REF}.
         *)
      match global,val_ncname_opt,attr_uses,val_ref_opt with
      | (* global *) true,
        (* val_ncname_opt *) (Some val_ncname),
        (* attr_uses *) attr_uses,
        (* val_ref_opt *) None ->
          (** case 1, global attribute group definition *)
          Some
            (Lib.A
               { Xsd.agd_name = val_ncname;
                 Xsd.agd_target = env.default.target;
                 Xsd.agd_uses = attr_uses;
                 Xsd.agd_w = any_attr_opt })
      | (* global *) false,
        (* val_ncname_opt *) None,
        (* attr_uses *) [],
        (* val_ref_opt *) (Some val_ref) ->
          (** case 2, local attribute group definition reference *)
          Some
            (Lib.B
               (Xsd.AUS_agd_ref val_ref))
      | (* global *) false,
        (* val_ncname_opt *) (Some _),
        (* attr_uses *) _,
        (* val_ref_opt *) _ ->
          Message.error_at loc
            (Error.XSDT_constructions_not_allowed
               ("attribute " ^ Xml_lib.to_string Xsd_val.a_name ^
                " in local "  ^ Xml_lib.to_string Xsd_val.e_attributeGroup))
      | (* global *) false,
        (* val_ncname_opt *) None,
        (* attr_uses *) _::_,
        (* val_ref_opt *) _ ->
          Message.error_at loc
            (Error.XSDT_constructions_not_allowed
               ("sub-element in local " ^
                Xml_lib.to_string Xsd_val.e_attributeGroup))
      | (* global *) false,
        (* val_ncname_opt *) None,
        (* attr_uses *) _,
        (* val_ref_opt *) None ->
          Message.error_at loc
            (Error.XSDT_constructions_expected
               ("attribute " ^ Xml_lib.to_string Xsd_val.a_ref ^
                " in local " ^ Xml_lib.to_string Xsd_val.e_attributeGroup))
      | (* global *) true,
        (* val_ncname_opt *) None,
        (* attr_uses *) _,
        (* val_ref_opt *) _ ->
          Message.error_at loc
            (Error.XSDT_constructions_expected
               ("attribute " ^ Xml_lib.to_string Xsd_val.a_name ^
                " in global " ^ Xml_lib.to_string Xsd_val.e_attributeGroup))
      | (* global *) true,
        (* val_ncname_opt *) _,
        (* attr_uses *) _,
        (* val_ref_opt *) (Some _) ->
          Message.error_at loc
            (Error.XSDT_constructions_not_allowed
               ("attribute " ^ Xml_lib.to_string Xsd_val.a_ref ^
                " in global " ^ Xml_lib.to_string Xsd_val.e_attributeGroup))


and transduce_attributeGroup_global env content : 'a Xsd.agd option =
  match transduce_attributeGroup_generic true env content with
  | None -> None
  | Some (Lib.A a) -> Some a
  | Some (Lib.B _) -> Message.error Error.A_local_global_function_types

and transduce_attributeGroup_local env content :
    Xsd.uri option Xsd.aus option =
  match transduce_attributeGroup_generic false env content with
  | None -> None
  | Some (Lib.A _) -> Message.error Error.A_local_global_function_types
  | Some (Lib.B b) -> Some b



and transduce_attribute_generic global env0 content :
    ('a Xsd.ad, 'a Xsd.aus) Lib.either option =
  match Xml_lib.next_elt Xsd_val.e_attribute content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let val_attr_default_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_default attrs in
      let val_attr_fixed_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_fixed attrs in
      let val_attr_form_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_form attrs in
      let _val_attr_id_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_name_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_name attrs in
      let val_attr_ref_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_ref attrs in
      let val_attr_targetNamespace_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_targetNamespace attrs in
      let val_attr_type_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_type attrs in
      let val_attr_use_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_use attrs in
      let __val_attr_inheritable_opt,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_inheritable attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let std_opt,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_simpleType false env]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_attribute));
      let val_use_opt =
        Lib.opt_apply Xsd_lib.read_use val_attr_use_opt in
      let val_name_opt =
        Lib.opt_apply
          (Xsd_lib.to_ncname Xml_lexer.is_ncname)
          val_attr_name_opt in
      let val_ref_opt =
        Lib.opt_apply (expand_qname env) val_attr_ref_opt in
      let val_type_opt =
        Lib.opt_apply (std_ref_of_qname env) val_attr_type_opt in
      let std_reference =
        match val_type_opt,std_opt with
        | None,(Some std) -> Xsd.STD_td_ref (Xsd.Val (Xsd.STD std))
        | (Some std_ref),None -> std_ref
        | None,None ->
            Xsd.STD_built_in Xsd.STD_anySimpleType
        | (Some _),(Some _) ->
            Message.error_at loc
              (Error.XSDT_constructions_mutually_exclusive
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_type ^
                  " and sub-element " ^
                  Xml_lib.to_string Xsd_val.e_simpleType ^
                  " of " ^ Xml_lib.to_string Xsd_val.e_attribute))
      in
      let ad_value_constraint_opt =
          match
            val_attr_default_opt,
            val_attr_fixed_opt
          with
          | None,None -> None
          | (Some sd),None ->
              Message.warning_at loc Error.XSDT_default_value_not_verified;
              Some (Xsd.A_default sd)
          | None,(Some sd) ->
              Message.warning_at loc Error.XSDT_fixed_value_not_verified;
              Some (Xsd.A_fixed sd)
          | (Some _),(Some _) ->
            Message.error_at loc
                (Error.XSDT_constructions_mutually_exclusive
                   ("attributes " ^ Xml_lib.to_string Xsd_val.a_default ^
                    " and " ^ Xml_lib.to_string Xsd_val.a_fixed))
      in
      (** There are three cases, mutually exclusive. There exists a
          fourth case (use="prohibited") which is dealt with by
          {!Xsd_lineariser}. *)
      begin
        match
          global,
          val_ref_opt,
          val_name_opt,
          val_use_opt
        with
        | (* global *) true,
          (* ref *) None,
          (* name *) (Some val_ncname),
          (* use *) None ->
            (** case 1, global attribute declaration *)
            Some
              (Lib.A
                 { Xsd.ad_name = val_ncname;
                   Xsd.ad_target = env.default.target;
                   Xsd.ad_std = std_reference;
                   Xsd.ad_scope = Xsd.Global;
                   Xsd.ad_value_constraint = ad_value_constraint_opt })
        | (* global *) false,
          (* ref *) None,
          (* name *) (Some val_ncname),
          (* use *) _ ->
            (** case 2, attribute use and declaration *)
            let ad =
              { Xsd.ad_name = val_ncname;
                Xsd.ad_target =
                local_target_attribute
                  env val_attr_targetNamespace_opt val_attr_form_opt;
                Xsd.ad_std = std_reference;
                Xsd.ad_scope = Xsd.Local;
                Xsd.ad_value_constraint = None }
            in
            Some
              (Lib.B
                 (Xsd.AUS_au
                    { Xsd.au_use = Lib.opt_default Xsd.Optional val_use_opt;
                      Xsd.au_ad = Xsd.Val ad;
                      Xsd.au_value_constraint = ad_value_constraint_opt }))

        | (* global *) false,
          (* ref *) (Some val_ref),
          (* name *) None,
          (* use *) _ ->
            (** case 3, attribute use *)
            Some
              (Lib.B
                (Xsd.AUS_au
                   { Xsd.au_use = Lib.opt_default Xsd.Optional val_use_opt;
                     Xsd.au_ad = Xsd.Ref val_ref;
                     Xsd.au_value_constraint = ad_value_constraint_opt }))
        | (* global *) _,
          (* ref *) None,
          (* name *) None,
          (* use *) _ ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_ref ^
                  " or " ^ Xml_lib.to_string Xsd_val.a_name))
        | (* global *) false,
          (* ref *) (Some _),
          (* name *) (Some _),
          (* use *) _ ->
            Message.error_at loc
              (Error.XSDT_constructions_mutually_exclusive
                 ("attributes " ^ Xml_lib.to_string Xsd_val.a_ref ^
                  " and " ^ Xml_lib.to_string Xsd_val.a_name))
        | (* global *) true,
          (* ref *) (Some _),
          (* name *) _,
          (* use *) _ ->
            Message.error_at loc
              (Error.XSDT_constructions_not_allowed
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_ref ^
                  " in global " ^ Xml_lib.to_string Xsd_val.e_attribute))
        | (* global *) true,
          (* ref *) _,
          (* name *) _,
          (* use *) (Some _) ->
            Message.error_at loc
              (Error.XSDT_constructions_not_allowed
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_use ^
                  " in global " ^ Xml_lib.to_string Xsd_val.e_attribute))
      end


and transduce_attribute_global env content : 'a Xsd.ad option =
  match transduce_attribute_generic true env content with
  | None -> None
  | Some (Lib.A a) -> Some a
  | Some (Lib.B _) -> Message.error Error.A_local_global_function_types

and transduce_attribute_local env content :
    Xsd.uri option Xsd.aus option =
  match transduce_attribute_generic false env content with
  | None -> None
  | Some (Lib.A _) -> Message.error Error.A_local_global_function_types
  | Some (Lib.B b) -> Some b


and transduce_anyAttribute content : (Xsd.uri option Xsd.w * int) option =
  match Xml_lib.next_elt_error Xsd_val.e_anyAttribute content with
  | None -> None
  | Some _ -> None
        (* The integer value is used to determines the document order
           of attribute wildcards. The suggested value is
           Stringdata.loc content *)

and transduce_restriction env0 std_name content : 'a Xsd.std option =
  match Xml_lib.next_elt Xsd_val.e_restriction content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let val_attr_base_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_base attrs in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let simpleType_opt,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_simpleType false env ]
          contents in
      let cfacets,contents =
        Lib.list_next_remove
        [ transduce_minExclusive;
          transduce_minInclusive;
          transduce_maxExclusive;
          transduce_maxInclusive;
          transduce_totalDigits;
          transduce_fractionDigits;
          transduce_length;
          transduce_minLength;
          transduce_maxLength;
          transduce_enumeration;
          transduce_whiteSpace;
          transduce_pattern;
          transduce_assertion;
          transduce_explicitTimezone ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_restriction));
      (* There are two cases, mutually exclusive *)
      let base_st_ref =
        match val_attr_base_opt,simpleType_opt with
        | (Some val_attr_base), None ->
            (* case 1, base *)
            std_ref_of_qname env val_attr_base
        | (Some _), (Some _)
        | None, None ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_base ^
                  " or element " ^ Xml_lib.to_string Xsd_val.e_simpleType))
        | None, Some simpleType ->
            (* case 2, simpleType *)
            Xsd.STD_td_ref (Xsd.Val (Xsd.STD simpleType))
      in
      Some { Xsd.std_name_or_context = std_name;
             Xsd.std_target = env.default.target;
             Xsd.std_facets = cfacets;
             Xsd.std_ffacets = [(* TODO ffacets *)];
             Xsd.std_variety = Xsd.STD_atomic base_st_ref }

and transduce_list env0 std_name content : 'a Xsd.std option =
  match Xml_lib.next_elt Xsd_val.e_list content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_itemType_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_itemType attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let simpleType_opt,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_simpleType false env ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_list));
      (* There are two cases, mutually exclusive *)
      let st_ref =
        match val_attr_itemType_opt,simpleType_opt with
        | (Some val_attr_itemType), None ->
            (* case 1, itemType *)
            std_ref_of_qname env val_attr_itemType
        | (Some _), (Some _)
        | None, None ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_itemType ^
                  " or element " ^ Xml_lib.to_string Xsd_val.e_simpleType))
        | None, Some simpleType ->
            (* case 2, simpleType *)
            Xsd.STD_td_ref (Xsd.Val (Xsd.STD simpleType))
      in
      Some { Xsd.std_name_or_context = std_name;
             Xsd.std_target = env.default.target;
             Xsd.std_facets = [ Xsd.CF_whiteSpace (Xsd.W_collapse, true) ];
             Xsd.std_ffacets = [(* TODO ffacets *)];
             Xsd.std_variety = Xsd.STD_list st_ref }

and transduce_union env0 std_name content : 'a Xsd.std option =
  match Xml_lib.next_elt Xsd_val.e_union content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_memberTypes_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_memberTypes attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let simpleTypes,contents =
        Lib.list_next_remove
          [ transduce_simpleType false env ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_union));
      (* There are two cases, mutually exclusive *)
      let l =
        match val_attr_memberTypes_opt,simpleTypes with
        | (Some val_attr_memberTypes), [] ->
            (* case 1, memberTypes *)
            List.map
              (std_ref_of_qname env)
              (Xml_lexer.lex_list val_attr_memberTypes)
        | (Some _), _::_ ->
            Message.error_at loc
              (Error.XSDT_constructions_mutually_exclusive
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_memberTypes ^
                  " or element " ^ Xml_lib.to_string Xsd_val.e_simpleType))
        | None, [] ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_memberTypes ^
                  " or element " ^ Xml_lib.to_string Xsd_val.e_simpleType))
        | None, _::_ ->
            (* case 2, simpleTypes *)
            List.map
              (fun v -> Xsd.STD_td_ref (Xsd.Val (Xsd.STD v)))
              simpleTypes
      in
      Some { Xsd.std_name_or_context = std_name;
             Xsd.std_target = env.default.target;
             Xsd.std_facets = [];
             Xsd.std_ffacets = [(* TODO ffacets *)];
             Xsd.std_variety = Xsd.STD_union l }

(** {b simpleType} transducer
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#element-simpleType}REF}
    - ''final'' attribute ignored (l:attr-META-no)
 *)
and transduce_simpleType global env0 content : 'a Xsd.std option =
  match Xml_lib.next_elt Xsd_val.e_simpleType content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let __val_attr_final,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_final attrs in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_name,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_name attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let std_name =
        match global,val_attr_name with
        | _, None ->
            Xsd.STD_context
        | true, Some n ->
            Xsd.STD_name (Xsd_lib.to_ncname Xml_lexer.is_ncname n)
        | false, Some n ->
            Message.error_at n
              (Error.XSDT_constructions_not_allowed
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_name ^
                  " in local " ^ Xml_lib.to_string Xsd_val.e_simpleType))
      in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let std,contents =
        match
          Lib.list_next_one_remove
            [ transduce_restriction env std_name;
              transduce_list env std_name;
              transduce_union env std_name ]
            contents
            with
        | Lib.OK (std,contents) -> (std,contents)
        | Lib.KO () ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("element " ^ Xml_lib.to_string Xsd_val.e_restriction ^
                  ", " ^ Xml_lib.to_string Xsd_val.e_list ^
                  " or " ^ Xml_lib.to_string Xsd_val.e_union))
      in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_simpleType));
      Some std

(** {b restriction} in {b simpleContent} transducer
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#element-simpleContent..restriction}REF}

    The function returns:
    - a simpleType definition
    - the derivation method (restriction)
    - an attribute list declaration
    - an attribute wildcard
 *)
let rec transduce_sc_restriction env0 content :
    ('a Xsd.std * Xsd.derivation_method * 'a Xsd.aus list * ('a Xsd.w * int) option) option =
  match Xml_lib.next_elt Xsd_val.e_restriction content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let val_attr_base_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_base attrs in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let simpleType_opt,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_simpleType false env ]
          contents in
      let cfacets,contents =
        Lib.list_next_remove
        [ transduce_minExclusive;
          transduce_minInclusive;
          transduce_maxExclusive;
          transduce_maxInclusive;
          transduce_totalDigits;
          transduce_fractionDigits;
          transduce_length;
          transduce_minLength;
          transduce_maxLength;
          transduce_enumeration;
          transduce_whiteSpace;
          transduce_pattern;
          transduce_assertion ]
          contents in
      let attr_uses,contents =
        Lib.list_next_remove
          [ transduce_attribute_local env;
            transduce_attributeGroup_local env ]
          contents in
      let any_attr_opt,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_anyAttribute ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_restriction ^
              " in " ^
              Xml_lib.to_string Xsd_val.e_simpleContent));
      (* There are two cases, mutually exclusive *)
      let base_st_ref =
        match val_attr_base_opt,simpleType_opt with
        | (Some val_attr_base), None ->
            (* case 1, base *)
            std_ref_of_qname env val_attr_base
        | (Some _), (Some _)
        | None, None ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_base ^
                  " or element " ^ Xml_lib.to_string Xsd_val.e_simpleType))
        | None, Some simpleType ->
            (* case 2, simpleType *)
            Xsd.STD_td_ref (Xsd.Val (Xsd.STD simpleType))
      in
      Some ({ Xsd.std_name_or_context = Xsd.STD_context;
              Xsd.std_target = env.default.target;
              Xsd.std_facets = cfacets;
              Xsd.std_ffacets = [(* TODO ffacets *)];
              Xsd.std_variety = Xsd.STD_atomic base_st_ref },
            Xsd.Restriction,
            attr_uses,
            any_attr_opt)

(** {b extension} in {b simpleContent} transducer
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#element-simpleContent..extension}REF}

    The function returns:
    - a simpleType definition
    - the derivation method (extension)
    - an attribute list declaration
    - an attribute wildcard
 *)
let rec transduce_sc_extension env0 content :
    ('a Xsd.std * Xsd.derivation_method * 'a Xsd.aus list * ('a Xsd.w * int) option) option =
  match Xml_lib.next_elt Xsd_val.e_extension content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let val_attr_base,attrs =
        Xml_lib.get_attr_require Xsd_val.a_base loc  attrs in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let attr_uses,contents =
        Lib.list_next_remove
          [ transduce_attribute_local env;
            transduce_attributeGroup_local env ]
          contents in
      let any_attr_opt,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_anyAttribute ]
          contents in
      let __asserts,contents =
        Lib.list_next_remove
          [ transduce_assert ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_extension ^
              " in " ^
              Xml_lib.to_string Xsd_val.e_simpleContent));
      Some ({ Xsd.std_name_or_context = Xsd.STD_context;
              Xsd.std_target = env.default.target;
              Xsd.std_facets = [(* TODO cfacets *)];
              Xsd.std_ffacets = [(* TODO ffacets *)];
              Xsd.std_variety =
              Xsd.STD_atomic
                (std_ref_of_qname env val_attr_base) },
            Xsd.Extension,
            attr_uses,
            any_attr_opt)


(** {b simpleContent} transducer
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#element-simpleContent}REF}
 *)
let transduce_simpleContent env0 content :
    (Xsd.uri option Xsd.std * Xsd.derivation_method *
       Xsd.uri option Xsd.aus list * (Xsd.uri option Xsd.w * int) option)
    option =
  match Xml_lib.next_elt Xsd_val.e_simpleContent content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let sc,contents =
        match
          Lib.list_next_one_remove
            [ transduce_sc_restriction env;
              transduce_sc_extension env ]
            contents
        with
        | Lib.OK (sc,contents) -> (sc,contents)
        | Lib.KO () ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("element " ^ Xml_lib.to_string Xsd_val.e_restriction ^
                  " or " ^ Xml_lib.to_string Xsd_val.e_extension))
      in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_simpleContent));
      Some sc

(** {b restriction} in {b complexContent} transducer
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#element-complexContent..restriction}REF}
 *)
let rec transduce_cc_restriction env0 content : 'a Xsd.ctd option =
  match Xml_lib.next_elt Xsd_val.e_restriction content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let val_attr_base,attrs =
        Xml_lib.get_attr_require Xsd_val.a_base loc attrs in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      match transduce_implicit_complexContent env contents with
      | None -> None
      | Some (((au_list,aw_opt),icc),contents) ->
          if Xml_lib.has_elt Xsd_val.uri contents
          then
            Message.error_at loc
              (Error.XSDT_extra_xsd_element_in
                 (Xml_lib.to_string Xsd_val.e_restriction ^
                  " in " ^
                  Xml_lib.to_string Xsd_val.e_complexContent));
          Some
            { Xsd.ctd_name_or_context = Xsd.CTD_context;
              Xsd.ctd_target = env.default.target;
              Xsd.ctd_base = ctd_ref_of_qname env val_attr_base;
              Xsd.ctd_dm = Xsd.Restriction;
              Xsd.ctd_au = au_list;
              Xsd.ctd_aw = aw_opt;
              Xsd.ctd_ct = icc }



(** {b extension} in {b complexContent} transducer
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#element-complexContent..extension}REF}
 *)
and transduce_cc_extension env0 content : 'a Xsd.ctd option =
  match Xml_lib.next_elt Xsd_val.e_extension content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let val_attr_base,attrs =
        Xml_lib.get_attr_require Xsd_val.a_base loc attrs in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      match transduce_implicit_complexContent env contents with
      | None -> None
      | Some (((au_list,aw_opt),icc),contents) ->
          if Xml_lib.has_elt Xsd_val.uri contents
          then
            Message.error_at loc
              (Error.XSDT_extra_xsd_element_in
                 (Xml_lib.to_string Xsd_val.e_extension ^
                  " in " ^
                  Xml_lib.to_string Xsd_val.e_complexContent));
          Some { Xsd.ctd_name_or_context = Xsd.CTD_context;
                 Xsd.ctd_target = env.default.target;
                 Xsd.ctd_base = ctd_ref_of_qname env val_attr_base;
                 Xsd.ctd_dm = Xsd.Extension;
                 Xsd.ctd_au = au_list;
                 Xsd.ctd_aw = aw_opt;
                 Xsd.ctd_ct = icc }


and transduce_complexContent env0 content :
    (Stringdata.t option * 'a Xsd.td) option =
  match Xml_lib.next_elt Xsd_val.e_complexContent content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_mixed,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_mixed attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let ctd,contents =
        match
          Lib.list_next_one_remove
            [ transduce_cc_restriction env;
              transduce_cc_extension env ]
            contents
        with
        | Lib.OK (ctd,contents) -> (ctd,contents)
        | Lib.KO () ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("element " ^ Xml_lib.to_string Xsd_val.e_restriction ^
                  " or " ^ Xml_lib.to_string Xsd_val.e_extension))
      in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_complexContent));
      Some (val_attr_mixed,
            Xsd.CTD ctd)

and transduce_modelGroup_generic global env0 content :
    ('a Xsd.mgd, 'a Xsd.p) Lib.either option =
  match Xml_lib.next_elt Xsd_val.e_group content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_id_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_maxOccurs_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_maxOccurs attrs in
      let val_attr_minOccurs_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_minOccurs attrs in
      let val_attr_name_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_name attrs in
      let val_attr_ref_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_ref attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let p_opt,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_all env;
            transduce_choice env;
            transduce_sequence env ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_group));
      let val_name_opt =
        Lib.opt_apply
          (Xsd_lib.to_ncname Xml_lexer.is_ncname)
          val_attr_name_opt in
      let val_ref_opt =
        Lib.opt_apply (expand_qname env) val_attr_ref_opt in
      let occurrence =
        occurrence_spec val_attr_minOccurs_opt val_attr_maxOccurs_opt in
      (** There are two cases, mutually exclusive. *)
      begin
        match
          global,
          val_ref_opt,
          val_attr_minOccurs_opt,
          val_attr_maxOccurs_opt,
          val_name_opt,
          p_opt
        with
        | (* global *) true,
          (* ref *) None,
          (* minOccurs *) None,
          (* maxOccurs *) None,
          (* name *) (Some val_ncname),
          (* particle *) (Some p) ->
            (** case 1 (with particle), global group definition *)
            let mg =
              match p.Xsd.p_term with
              | Xsd.T_mg mg -> mg
              | Xsd.T_ed _
              | Xsd.T_mgd _
              | Xsd.T_w _ ->
                  Message.error_at loc (Error.A_function
                    "Xsd_transducer.transduce_modelGroup_generic")
            in
            Some
              (Lib.A
                 { Xsd.mgd_name = val_ncname;
                   Xsd.mgd_target = env.default.target;
                   Xsd.mgd_mg = mg; })
        | (* global *) true,
          (* ref *) None,
          (* minOccurs *) None,
          (* maxOccurs *) None,
          (* name *) (Some val_ncname),
          (* particle *) None ->
            (** case 1 (empty), global group definition *)
            let mg =
              Xsd.MG_sequence []
            in
            Some
              (Lib.A
                 { Xsd.mgd_name = val_ncname;
                   Xsd.mgd_target = env.default.target;
                   Xsd.mgd_mg = mg; })
        | (* global *) false,
          (* ref *) (Some val_ref),
          (* minOccurs *) _,
          (* maxOccurs *) _,
          (* name *) None,
          (* particle *) None ->
            (** case 2, particle *)
            Some
              (Lib.B
                 { Xsd.p_occurrence = occurrence;
                   Xsd.p_term = Xsd.T_mgd (Xsd.Ref val_ref) })
        | (* global *) true,
          (* ref *) (Some _),
          (* minOccurs *) _,
          (* maxOccurs *) _,
          (* name *) _,
          (* particle *) _ ->
            Message.error_at loc
              (Error.XSDT_constructions_not_allowed
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_ref ^
                  " in global " ^ Xml_lib.to_string Xsd_val.e_group))
        | (* global *) false,
          (* ref *) _,
          (* minOccurs *) _,
          (* maxOccurs *) _,
          (* name *) (Some _),
          (* particle *) _ ->
            Message.error_at loc
              (Error.XSDT_constructions_not_allowed
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_name ^
                  " in local " ^ Xml_lib.to_string Xsd_val.e_group))
        | (* global *) false,
          (* ref *) None,
          (* minOccurs *) _,
          (* maxOccurs *) _,
          (* name *) _,
          (* particle *) _ ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_ref ^
                  " in local " ^ Xml_lib.to_string Xsd_val.e_group))
        | (* global *) _,
          (* ref *) (Some _),
          (* minOccurs *) _,
          (* maxOccurs *) _,
          (* name *) _,
          (* particle *) (Some _) ->
            Message.error_at loc
              (Error.XSDT_constructions_mutually_exclusive
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_ref ^
                  " and sub-elements"))
        | (* global *) true,
          (* ref *) _,
          (* minOccurs *) _,
          (* maxOccurs *) _,
          (* name *) None,
          (* particle *) _ ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_name ^
                  " in global " ^ Xml_lib.to_string Xsd_val.e_group))
        | (* global *) true,
          (* ref *) _,
          (* minOccurs *) (Some _),
          (* maxOccurs *) _,
          (* name *) _,
          (* particle *) _ ->
            Message.error_at loc
              (Error.XSDT_constructions_not_allowed
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_minOccurs ^
                  " in global " ^ Xml_lib.to_string Xsd_val.e_group))
        | (* global *) true,
          (* ref *) _,
          (* minOccurs *) _,
          (* maxOccurs *) (Some _),
          (* name *) _,
          (* particle *) _ ->
            Message.error_at loc
              (Error.XSDT_constructions_not_allowed
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_maxOccurs ^
                  " in global " ^ Xml_lib.to_string Xsd_val.e_group))
      end


and transduce_modelGroup_global env content : 'a Xsd.mgd option =
  match transduce_modelGroup_generic true env content with
  | None -> None
  | Some (Lib.A a) -> Some a
  | Some (Lib.B _) -> Message.error Error.A_local_global_function_types

and transduce_modelGroup_local env content : 'a Xsd.p option =
  match transduce_modelGroup_generic false env content with
  | None -> None
  | Some (Lib.A _) -> Message.error Error.A_local_global_function_types
  | Some (Lib.B b) -> Some b



and transduce_all env0 content : 'a Xsd.p option =
  match Xml_lib.next_elt Xsd_val.e_all content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_maxOccurs,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_maxOccurs attrs in
      let val_attr_minOccurs,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_minOccurs attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let eag_list,contents =
        Lib.list_next_remove
          [ transduce_element_local env;
            transduce_any env;
            transduce_modelGroup_local env ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_all));
      let occ_spec_all =
        occurrence_spec
          val_attr_minOccurs
          val_attr_maxOccurs;
      in
      if
        occ_spec_all.Xsd.max_occurs = Xsd.Occur 1
          && ( occ_spec_all.Xsd.min_occurs = 0
             || occ_spec_all.Xsd.min_occurs = 1)
      then
        Some
          { Xsd.p_occurrence = occ_spec_all;
            Xsd.p_term =
            Xsd.T_mg
              (Xsd.MG_all eag_list) }
      else
        Message.error_at loc
          Error.XSDT_occurrence_value_invalid



and transduce_choice env0 content : 'a Xsd.p option =
  match Xml_lib.next_elt Xsd_val.e_choice content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_maxOccurs,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_maxOccurs attrs in
      let val_attr_minOccurs,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_minOccurs attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let egcsa_list,contents =
        Lib.list_next_remove
          [ transduce_element_local env;
            transduce_modelGroup_local env;
            transduce_choice env;
            transduce_sequence env;
            transduce_any env ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_choice));
      (Some
         { Xsd.p_occurrence =
           occurrence_spec
             val_attr_minOccurs
             val_attr_maxOccurs;
           Xsd.p_term =
           Xsd.T_mg
             (Xsd.MG_choice egcsa_list) } )

and transduce_sequence env0 content : 'a Xsd.p option =
  match Xml_lib.next_elt Xsd_val.e_sequence content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_maxOccurs,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_maxOccurs attrs in
      let val_attr_minOccurs,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_minOccurs attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let egcsa_list,contents =
        Lib.list_next_remove
          [ transduce_element_local env;
            transduce_modelGroup_local env;
            transduce_choice env;
            transduce_sequence env;
            transduce_any env ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_sequence));
      (Some
         { Xsd.p_occurrence =
           occurrence_spec
             val_attr_minOccurs
             val_attr_maxOccurs;
           Xsd.p_term =
           Xsd.T_mg
             (Xsd.MG_sequence egcsa_list) } )

and transduce_implicit_complexContent env contents :
    (((Xsd.uri option Xsd.aus list *
         (Xsd.uri option Xsd.w * int) option) *
        Xsd.uri option Xsd.ct) *
       Xml.expanded_element Xml.content list) option =
  let _,contents =
    Lib.list_next_none_or_one_remove
      [ transduce_openContent ]
      contents in
  let p_opt,contents =
    Lib.list_next_none_or_one_remove
      [ transduce_modelGroup_local env;
        transduce_all env;
        transduce_choice env;
        transduce_sequence env ]
      contents in
  let attr_uses,contents =
    Lib.list_next_remove
      [ transduce_attribute_local env;
        transduce_attributeGroup_local env ]
      contents in
  let any_attr_opt,contents =
    Lib.list_next_none_or_one_remove
      [ transduce_anyAttribute ]
      contents in
  let _,contents =
    Lib.list_next_remove
      [ transduce_assert ]
      contents in
  if Xml_lib.has_elt Xsd_val.uri contents
  then None
  else
    let ct =
      match p_opt with
      | None ->
          Xsd.CT_empty
      | Some p ->
          Xsd.CT_element_only p
    in
    Some
      (((attr_uses,any_attr_opt),ct),
       contents)

(** {b complexType} transducer
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#element-complexType}REF} *)
and transduce_complexType env0 content : 'a Xsd.td option =
  match Xml_lib.next_elt Xsd_val.e_complexType content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_abstract,attrs =
        Xml_lib.get_attr_error Xsd_val.a_abstract attrs in
      let _val_attr_block,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_block attrs in
      let _val_attr_final,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_final attrs in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_mixed_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_mixed attrs in
      let val_attr_name,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_name attrs in
      let _val_attr_defaultAttributesApply,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_defaultAttributesApply attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let ctd_name_or_context =
        match val_attr_name with
        | None ->
            Xsd.CTD_context
        | Some n ->
            Xsd.CTD_name (Xsd_lib.to_ncname Xml_lexer.is_ncname n)
      in
      let ctd,contents =
        (** There are three cases, mutually exclusive: *)
        begin
          match
            (Lib.list_next_list_wrap
               (transduce_simpleContent env)
               contents),
            (Lib.list_next_list_wrap
               (transduce_complexContent env)
               contents),
            (transduce_implicit_complexContent env contents) with
          | (* simpleContent *) None,
            (* complexContent *) None,
            (* implicit complexContent *) None ->
              Message.error_at loc
                (Error.XSDT_constructions_expected
                   ("element " ^ Xml_lib.to_string Xsd_val.e_simpleContent ^
                    ", " ^ Xml_lib.to_string Xsd_val.e_complexContent ^
                    " or implicit complex content"))
          | (* simpleContent *) (Some _),
            (* complexContent *) (Some _),
            (* implicit complexContent *) _
          | (* simpleContent *) _,
            (* complexContent *) (Some _),
            (* implicit complexContent *) (Some _)
          | (* simpleContent *) (Some _),
            (* complexContent *) _,
            (* implicit complexContent *) (Some _) ->
              Message.error_at loc
                (Error.XSDT_constructions_mutually_exclusive
                   ("elements " ^ Xml_lib.to_string Xsd_val.e_simpleContent ^
                    ", " ^ Xml_lib.to_string Xsd_val.e_complexContent ^
                    " and implicit complex content"))
          | (* simpleContent *) (Some ((sc,dm,au_list,aw_opt),contents)),
            (* complexContent *) None,
            (* implicit complexContent *) None ->
              (** - case 1, simple content *)
              { Xsd.ctd_name_or_context = ctd_name_or_context;
                Xsd.ctd_target = env.default.target;
                Xsd.ctd_base = Xsd.CTD_simpleContentType;
                Xsd.ctd_dm = dm;
                Xsd.ctd_au = au_list;
                Xsd.ctd_aw = aw_opt;
                Xsd.ctd_ct = Xsd.CT_simple sc},
              contents
          | (* simpleContent *) None,
            (* complexContent *) (Some ((sd_mixed_2,td),contents)),
            (* implicit complexContent *) None ->
              (** - case 2, complex content *)
              let sd_mixed_1 =
                val_attr_mixed_opt in
              (combine_mixed_name_in_td
                 td
                 val_attr_name
                 sd_mixed_1
                 sd_mixed_2),
              contents
          | (* simpleContent *) None,
            (* complexContent *) None,
            (* implicit complexContent *) (Some (((au_list,aw_opt),icc),contents)) ->
              (** - case 3, implicit complex content *)
              let mixed_b =
                match val_attr_mixed_opt with
                | None -> false
                | Some val_attr_mixed ->
                    Xsd_lib.read_boolean val_attr_mixed
              in
              let ct =
                if mixed_b
                then
                  (* the content type is mixed *)
                  match icc with
                  | Xsd.CT_empty ->
                      Xsd.CT_mixed
                        { Xsd.p_occurrence =
                          { Xsd.min_occurs = 0;
                            Xsd.max_occurs = Xsd.Unbounded };
                          Xsd.p_term = Xsd.T_mg (Xsd.MG_sequence []) }
                  | Xsd.CT_element_only p ->
                      Xsd.CT_mixed p
                  | Xsd.CT_simple _
                  | Xsd.CT_mixed _ ->
                      (* these constructors could not be returned by
                         transduce_implicit_complexContent *)
                      Message.error_at loc
                        (Error.A_function
                           "Xsd_transducer.transduce_complexType")
                else
                  (* the content type is the implicit one *)
                  icc
              in
              { Xsd.ctd_name_or_context = ctd_name_or_context;
                Xsd.ctd_target = env.default.target;
                Xsd.ctd_base = Xsd.CTD_anyType;
                Xsd.ctd_dm = Xsd.Restriction;
                Xsd.ctd_au = au_list;
                Xsd.ctd_aw = aw_opt;
                Xsd.ctd_ct = ct },
              contents
        end
      in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_complexType));
      (Some (Xsd.CTD ctd))

and transduce_element_generic global env0 content :
    ('a Xsd.ed, 'a Xsd.p) Lib.either option =
  match Xml_lib.next_elt Xsd_val.e_element content with
  | None -> None
  | Some (loc,attrs,contents) ->
      let env = update_env_ns attrs env0 in
      let _val_attr_abstract,attrs =
        Xml_lib.get_attr_error Xsd_val.a_abstract attrs in
      let _val_attr_block,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_block attrs in
      let val_attr_default_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_default attrs in
      let _val_attr_final,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_final attrs in
      let val_attr_fixed_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_fixed attrs in
      let val_attr_form_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_form attrs in
      let _val_attr_id_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_maxOccurs,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_maxOccurs attrs in
      let val_attr_minOccurs,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_minOccurs attrs in
      let val_attr_name_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_name attrs in
      let __val_attr_nillable,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_nillable attrs in
      let val_attr_ref_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_ref attrs in
      let _val_attr_substitutionGroup,attrs =
        Xml_lib.get_attr_error Xsd_val.a_substitutionGroup attrs in
      let val_attr_targetNamespace_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_targetNamespace attrs in
      let val_attr_type_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_type attrs in
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let _,contents =
        Lib.list_next_none_or_one_remove
          [ transduce_annotation ]
          contents in
      let type_opt,contents =
        Lib.list_next_none_or_one_remove
          [ wrap xsd_std (transduce_simpleType false env);
            transduce_complexType env ]
          contents in
      let _,contents =
        Lib.list_next_remove
         [ transduce_alternative ]
          contents in
      let __keys,contents =
        Lib.list_next_remove
        [ transduce_unique;
          transduce_key;
          transduce_keyref ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_element));
      let ed_value_constraint_opt =
          match
            val_attr_default_opt,
            val_attr_fixed_opt
          with
          | None,None -> None
          | (Some sd),None ->
              Some (Xsd.A_default sd)
          | None,(Some sd) ->
              Some (Xsd.A_fixed sd)
          | (Some _),(Some _) ->
            Message.error_at loc
                (Error.XSDT_constructions_mutually_exclusive
                   ("attributes " ^ Xml_lib.to_string Xsd_val.a_default ^
                    " and " ^ Xml_lib.to_string Xsd_val.a_fixed))
      in
      (** There are three cases, mutually exclusive *)
      begin
        match global,val_attr_ref_opt,val_attr_name_opt with
        | (* global*) true,
          (* val_attr_ref_opt *) None,
          (* val_attr_name_opt *) (Some val_attr_name) ->
            (** case 1, global element *)
            Some
              (Lib.A
                 { Xsd.ed_ncname =
                   Xsd_lib.to_ncname Xml_lexer.is_ncname val_attr_name;
                   Xsd.ed_target = env.default.target;
                   Xsd.ed_td =
                   get_ed_td
                     type_opt
                     val_attr_type_opt
                     env
                     loc;
                   Xsd.ed_scope = Xsd.Global;
                   Xsd.ed_value_constraint = ed_value_constraint_opt } )
        | (* global*) false,
          (* val_attr_ref_opt *) None,
          (* val_attr_name_opt *) (Some val_attr_name) ->
            (** case 2, local element value *)
            Some
              (Lib.B
                 { Xsd.p_occurrence =
                   occurrence_spec
                     val_attr_minOccurs
                     val_attr_maxOccurs;
                   Xsd.p_term =
                   Xsd.T_ed
                     (Xsd.Val
                        { Xsd.ed_ncname =
                          Xsd_lib.to_ncname Xml_lexer.is_ncname val_attr_name;
                          Xsd.ed_target =
                          local_target_element
                            env val_attr_targetNamespace_opt val_attr_form_opt;
                          Xsd.ed_td =
                          get_ed_td
                            type_opt
                            val_attr_type_opt
                            env
                            loc;
                          Xsd.ed_scope = Xsd.Local;
                          Xsd.ed_value_constraint =
                          ed_value_constraint_opt} ) })
        | (* global*) false,
          (* val_attr_ref_opt *) (Some val_attr_ref),
          (* val_attr_name_opt *) None ->
            (** case 3, local element reference *)
            Some
              (Lib.B
                 { Xsd.p_occurrence =
                   occurrence_spec
                     val_attr_minOccurs
                     val_attr_maxOccurs;
                   Xsd.p_term =
                   Xsd.T_ed (Xsd.Ref (expand_qname env val_attr_ref)) })
        | (* global*) true,
          (* val_attr_ref_opt *) (Some _),
          (* val_attr_name_opt *) _ ->
            Message.error_at loc
              (Error.XSDT_constructions_not_allowed
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_ref ^
                  " in global " ^ Xml_lib.to_string Xsd_val.e_element))
        | (* global*) false,
          (* val_attr_ref_opt *) (Some _),
          (* val_attr_name_opt *) (Some _) ->
            Message.error_at loc
              (Error.XSDT_constructions_mutually_exclusive
                 ("attributes " ^ Xml_lib.to_string Xsd_val.a_name ^
                  " and " ^ Xml_lib.to_string Xsd_val.a_ref))
        | (* global*) _,
          (* val_attr_ref_opt *) None,
          (* val_attr_name_opt *) None ->
            Message.error_at loc
              (Error.XSDT_constructions_expected
                 ("attribute " ^ Xml_lib.to_string Xsd_val.a_name ^
                  " or " ^ Xml_lib.to_string Xsd_val.a_ref))
      end

and transduce_element_global env content : 'a Xsd.ed option =
  match transduce_element_generic true env content with
  | None -> None
  | Some (Lib.A a) -> Some a
  | Some (Lib.B _) -> Message.error Error.A_local_global_function_types

and transduce_element_local env content : 'a Xsd.p option =
  match transduce_element_generic false env content with
  | None -> None
  | Some (Lib.A _) -> Message.error Error.A_local_global_function_types
  | Some (Lib.B b) -> Some b


let transduce_notation content (* : Xsd.nd option *) =
  match Xml_lib.next_elt_error Xsd_val.e_notation content with
  | None -> None
  | Some _ -> None


(** {b schema} transducer *)
let transduce = function
  | { Xml.declaration = _;
      Xml.root = { Xml.node =
                     { Xml.element_name = root_name;
                       Xml.attributes = attrs };
                   Xml.contents = contents }
    } ->
      let ns = Xml_ns.add_exp_attrs attrs Xml_ns.default in
      let loc = root_name.Xml.name in
      if not (Xml_lib.eq_name root_name Xsd_val.e_schema)
      then
        Message.error_at loc
          (Error.XSDT_constructions_expected
             ("element " ^ Xml_lib.to_string Xsd_val.e_schema));
      let val_attr_attributeFormDefault_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_attributeFormDefault attrs in
      let __val_attr_blockDefault,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_blockDefault attrs in
      let __val_attr_defaultAttributes,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_defaultAttributes attrs in
      let __val_attr_xpathDefaultNamespace,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_xpathDefaultNamespace attrs in
      let val_attr_elementFormDefault_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_elementFormDefault attrs in
      let __val_attr_finalDefault,attrs =
        Xml_lib.get_attr_ignore Xsd_val.a_finalDefault attrs in
      let _val_attr_id,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_id attrs in
      let val_attr_targetNamespace_opt,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_targetNamespace attrs in
      let val_attr_version,attrs =
        Xml_lib.get_attr_optional Xsd_val.a_version attrs in
      (* xml:lang *)
      Xsd_lib.check_xsd_version val_attr_version;
      if Xml_lib.has_attr Xsd_val.uri attrs
      then
        Message.error_at loc Error.XSDT_extra_xsd_attribute_not_recognised;
      let target = Lib.opt_apply Xsd_lib.to_uri val_attr_targetNamespace_opt in
      let qualified_form_element =
        match Xsd_lib.read_form val_attr_elementFormDefault_opt with
        | None -> Xsd.Unqualified
        | Some q -> q
      in
      let qualified_form_attribute =
        match Xsd_lib.read_form val_attr_attributeFormDefault_opt with
        | None -> Xsd.Unqualified
        | Some q -> q
      in
      let env =
        { default =
          { target = target;
            qualified_form_element = qualified_form_element;
            qualified_form_attribute = qualified_form_attribute;
            (* ns_list = (Lib.list_of_opt target) @ [Xsd.Uri Xsd_val.uri; Xsd.Uri Xsd_val.instance_uri] *) (* plus namespaces of import *) };
          ns = ns } in
      let space =
        Xml_lib.get_space attrs in
      let contents =
        Xml_lib.clear_space_leafs space contents in
      let composition_opts,contents =
        Lib.list_next_remove
          [ wrap_some transduce_include;
            wrap_some transduce_import;
            wrap_some transduce_redefine;
            wrap_some transduce_override;
            wrap_none transduce_annotation ]
          contents in
      let _OC,contents =
        Lib.list_next_remove
          [ transduce_defaultOpenContent;
            transduce_annotation ]
          contents in
      let definition_opts,contents =
        Lib.list_next_remove
          [ wrap_some (wrap xsd_td_std (transduce_simpleType true env));
            wrap_some (wrap xsd_td (transduce_complexType env));
            wrap_some (wrap xsd_mgd (transduce_modelGroup_global env));
            wrap_some (wrap xsd_agd (transduce_attributeGroup_global env));
            wrap_some (wrap xsd_ed (transduce_element_global env));
            wrap_some (wrap xsd_ad (transduce_attribute_global env));
            transduce_notation;
            wrap_none transduce_annotation ]
          contents in
      if Xml_lib.has_elt Xsd_val.uri contents
      then
        Message.error_at loc
          (Error.XSDT_extra_xsd_element_in
             (Xml_lib.to_string Xsd_val.e_schema));
      let compositions =
        Lib.list_flatten_opt composition_opts in
      let definitions =
        Lib.list_flatten_opt definition_opts in
      { Xsd.schema_form = Xsd.Modules compositions;
        Xsd.target = env.default.target;
        Xsd.definitions = definitions }

(** {2 Inliners} *)

let inline_uri primary = function
  | None -> primary
  | Some uri -> uri

let inline_reference inline_f primary = function
  | Xsd.Val v -> Xsd.Val (inline_f primary v)
  | Xsd.Ref _ as r -> r

let inline_nc primary = function
  | Xsd.NC_any nc_disallowed_list ->
      Xsd.NC_any nc_disallowed_list
  | Xsd.NC_enumeration (uri_opt_list,nc_disallowed_list) ->
      Xsd.NC_enumeration
        (List.map
            (Lib.opt_apply
               (inline_uri primary))
            uri_opt_list,
         nc_disallowed_list)
  | Xsd.NC_not (uri_opt_list,nc_disallowed_list) ->
      Xsd.NC_not
        (List.map
           (Lib.opt_apply
              (inline_uri primary))
           uri_opt_list,
         nc_disallowed_list)

let inline_w primary = function
  | { Xsd.w_nc = nc;
      Xsd.w_pc = pc } ->
        { Xsd.w_nc = inline_nc primary nc;
          Xsd.w_pc = pc }

let rec inline_mg primary = function
  | Xsd.MG_all p_l -> Xsd.MG_all (List.map (inline_p primary) p_l)
  | Xsd.MG_choice p_l -> Xsd.MG_choice (List.map (inline_p primary) p_l)
  | Xsd.MG_sequence p_l -> Xsd.MG_sequence (List.map (inline_p primary) p_l)

and inline_t primary = function
  | Xsd.T_ed ed_ref ->
      Xsd.T_ed (inline_reference inline_ed primary ed_ref)
  | Xsd.T_mg  mg ->
      Xsd.T_mg (inline_mg primary mg)
  | Xsd.T_mgd  mgd_ref ->
      Xsd.T_mgd (inline_reference inline_mgd primary mgd_ref)
  | Xsd.T_w w ->
      Xsd.T_w (inline_w primary w)

and inline_p primary = function
  | { Xsd.p_occurrence = occurrence;
      Xsd.p_term = t } ->
        { Xsd.p_occurrence = occurrence;
          Xsd.p_term = inline_t primary t }

and inline_std_ref primary = function
  | Xsd.STD_built_in st -> Xsd.STD_built_in st
  | Xsd.STD_td_ref td_ref ->
      Xsd.STD_td_ref (inline_reference inline_td primary td_ref)

and inline_std_variety primary = function
  | Xsd.STD_atomic std_ref ->
      Xsd.STD_atomic (inline_std_ref primary std_ref)
  | Xsd.STD_list std_ref ->
      Xsd.STD_list (inline_std_ref primary std_ref)
  | Xsd.STD_union std_ref_l ->
      Xsd.STD_union
        (List.map
           (inline_std_ref primary)
           std_ref_l)

and inline_std primary = function
  | { Xsd.std_name_or_context = std_name_or_context;
      Xsd.std_target = uri;
      Xsd.std_facets = cfacet_l;
      Xsd.std_ffacets = ffacet_l;
      Xsd.std_variety = std_variety } ->
        { Xsd.std_name_or_context = std_name_or_context;
          Xsd.std_target =  inline_uri primary uri;
          Xsd.std_facets = cfacet_l;
          Xsd.std_ffacets = ffacet_l;
          Xsd.std_variety = inline_std_variety primary std_variety }

and inline_ct primary = function
  | Xsd.CT_empty as ct -> ct
  | Xsd.CT_simple std -> Xsd.CT_simple (inline_std primary std)
  | Xsd.CT_element_only p -> Xsd.CT_element_only (inline_p primary p)
  | Xsd.CT_mixed p -> Xsd.CT_mixed (inline_p primary p)

and inline_ctd_ref primary = function
  | Xsd.CTD_anyType -> Xsd.CTD_anyType
  | Xsd.CTD_td_ref td_ref ->
      Xsd.CTD_td_ref (inline_reference inline_td primary td_ref)
  | Xsd.CTD_simpleContentType ->
      Xsd.CTD_simpleContentType
  | Xsd.CTD_built_in_simpleType st ->
      Xsd.CTD_built_in_simpleType st

and inline_ctd primary = function
  | { Xsd.ctd_name_or_context = ctd_name_or_context;
      Xsd.ctd_target = uri;
      Xsd.ctd_base = ctd_ref;
      Xsd.ctd_dm = derivation_method;
      Xsd.ctd_au = au_list;
      Xsd.ctd_aw = aw_opt;
      Xsd.ctd_ct = ct } ->
        { Xsd.ctd_name_or_context = ctd_name_or_context;
          Xsd.ctd_target = inline_uri primary uri;
          Xsd.ctd_base = inline_ctd_ref primary ctd_ref;
          Xsd.ctd_dm = derivation_method;
          Xsd.ctd_au = List.map (inline_aus primary) au_list;
          Xsd.ctd_aw =
          Lib.opt_apply
            (fun (w,i) -> (inline_w primary w),i)
            aw_opt;
          Xsd.ctd_ct = inline_ct primary ct }

and inline_td primary = function
  | Xsd.STD std -> Xsd.STD (inline_std primary std)
  | Xsd.CTD ctd -> Xsd.CTD (inline_ctd primary ctd)

and inline_ed primary = function
  | { Xsd.ed_ncname = ncname;
      Xsd.ed_target = uri;
      Xsd.ed_td = ctd_ref;
      Xsd.ed_scope = scope;
      Xsd.ed_value_constraint = ed_value_constraint_opt } ->
        { Xsd.ed_ncname = ncname;
          Xsd.ed_target = inline_uri primary uri;
          Xsd.ed_td = inline_ctd_ref primary ctd_ref;
          Xsd.ed_scope = scope;
          Xsd.ed_value_constraint = ed_value_constraint_opt }

and inline_ad primary = function
  | { Xsd.ad_name = ad_name;
      Xsd.ad_target = ad_target;
      Xsd.ad_std = ad_std;
      Xsd.ad_scope = ad_scope;
      Xsd.ad_value_constraint = ad_value_constraint } ->
        { Xsd.ad_name = ad_name;
          Xsd.ad_target = inline_uri primary ad_target;
          Xsd.ad_std = inline_std_ref primary ad_std;
          Xsd.ad_scope = ad_scope;
          Xsd.ad_value_constraint = ad_value_constraint }

and inline_au primary = function
  | { Xsd.au_use = use;
      Xsd.au_ad = ad_ref;
      Xsd.au_value_constraint = ad_value_constraint_opt } ->
        { Xsd.au_use = use;
          Xsd.au_ad = inline_reference inline_ad primary ad_ref;
          Xsd.au_value_constraint = ad_value_constraint_opt }

and inline_aus primary = function
  | Xsd.AUS_au au ->
      Xsd.AUS_au (inline_au primary au)
  | Xsd.AUS_agd_ref expanded_name ->
      Xsd.AUS_agd_ref expanded_name

and inline_mgd primary = function
  | { Xsd.mgd_name = ncname;
      Xsd.mgd_target = uri;
      Xsd.mgd_mg = mg } ->
        { Xsd.mgd_name = ncname;
          Xsd.mgd_target = inline_uri primary uri;
          Xsd.mgd_mg = inline_mg primary mg }

and inline_agd primary = function
  | { Xsd.agd_name = agd_name;
      Xsd.agd_target = agd_target;
      Xsd.agd_uses = agd_uses;
      Xsd.agd_w = agd_w } ->
        { Xsd.agd_name = agd_name;
          Xsd.agd_target = inline_uri primary agd_target;
          Xsd.agd_uses = List.map (inline_aus primary) agd_uses;
          Xsd.agd_w =
          Lib.opt_apply
            (fun (w,i) -> (inline_w primary w),i)
            agd_w }

let inline_definition primary = function
  | Xsd.TD td -> Xsd.TD (inline_td primary td)
  | Xsd.AD ad -> Xsd.AD (inline_ad primary ad)
  | Xsd.ED ed -> Xsd.ED (inline_ed primary ed)
  | Xsd.AGD agd -> Xsd.AGD (inline_agd primary agd)
  | Xsd.MGD mgd -> Xsd.MGD (inline_mgd primary mgd)


let inline_module_definitions secondaries =
  Xsd_lib.M_composite.fold
    (fun _ (target,schema) l ->
      match schema with
      | { Xsd.schema_form = _;
          Xsd.target = _;
          Xsd.definitions = definitions } ->
            l @
            (List.map (inline_definition target) definitions) )
    secondaries
    []

(** Inline schema composites *)
let inline primary secondaries =
  match primary with
  | { Xsd.schema_form = _;
      Xsd.target = Some target;
      Xsd.definitions = definitions } ->
        let modules_definitions =
          inline_module_definitions secondaries in
        let schema_definitions =
          List.map (inline_definition target) definitions in
        let inlined_definitions =
          schema_definitions @ modules_definitions in
        { Xsd.schema_form = Xsd.Inlined;
          Xsd.target = target;
          Xsd.definitions = inlined_definitions }
  | { Xsd.schema_form = _;
      Xsd.target = None;
      Xsd.definitions = _ } ->
        Message.error Error.GEN_XSDT_primary_target_uri_missing
