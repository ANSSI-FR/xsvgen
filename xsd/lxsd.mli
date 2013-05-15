(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lxsd.mli 1686 2012-05-30 09:00:22Z maarek $ *)


(** {1 Lambda-XSD types} *)

type uri_id = Id_uri of int

type td_id = Id_td of int

type ad_id = Id_ad of int

type ed_id = Id_ed of int

type agd_id = Id_agd of int

type mgd_id = Id_mgd of int

type max_occurrence_val =
  | Occur of int
  | Unbounded

type occurrence =
    { min_occurs : int;
      max_occurs : max_occurrence_val;
    }

type process_contents =
  | Skip
  | Strict
  | Lax

type nc_disallowed =
  | NC_qname of Xml.expanded_name (* qname *)
  | NC_defined
  | NC_sibling


type scope =
  | Global
  | Local

(** Schema Component: {b \{namespace constraint\}} *)
type nc =
  | NC_any of nc_disallowed list
  | NC_enumeration of uri_id list * nc_disallowed list
  | NC_not of uri_id list * nc_disallowed list

type ctd_name_or_context =
  | CTD_name of Stringdata.t
  | CTD_context_ed of ed_id
  | CTD_context_ctd of td_id

type w =
    { (* IGNORED {annotations} *)
      w_nc : nc;
      w_pc : process_contents
    }

type t =
  | T_ed of ed_id
  | T_mgd of mgd_id
  | T_w of w


and p =
    { p_occurrence : occurrence;
      p_term : t
     (* IGNORED {annotations} *)
    }


type regexp =
  | RegExp of Stringdata.t


(** Primitive types *)
type primitive_type =
  | PT_string (** {b \{string\}} *)
  | PT_boolean (** {b \{boolean\}} *)
  | PT_decimal (** {b \{decimal\}} *)
  | PT_float (** {b \{float\}} *)
  | PT_double (** {b \{double\}} *)
  | PT_duration (** {b \{duration\}} *)
  | PT_dateTime (** {b \{dateTime\}} *)
  | PT_time (** {b \{time\}} *)
  | PT_date (** {b \{date\}} *)
  | PT_gYearMonth (** {b \{gYearMonth\}} *)
  | PT_gYear (** {b \{gYear\}} *)
  | PT_gMonthDay (** {b \{gMonthDay\}} *)
  | PT_gDay (** {b \{gDay\}} *)
  | PT_gMonth (** {b \{gMonth\}} *)
  | PT_hexBinary (** {b \{hexBinary\}} *)
  | PT_base64Binary (** {b \{base64Binary\}} *)
  | PT_anyURI (** {b \{anyURI\}} *)
  | PT_QName (** {b \{QName\}} *)
  | PT_NOTATION (** {b \{NOTATION\}} *)


type non_primitive_type =
  | NPT_normalizedString (** {b \{normalizedString\}} *)
  | NPT_token (** {b \{token\}} *)
  | NPT_language (** {b \{language\}} *)
  | NPT_IDREFS (** {b \{IDREFS\}} *)
  | NPT_ENTITIES (** {b \{ENTITIES\}} *)
  | NPT_NMTOKEN (** {b \{NMTOKEN\}} *)
  | NPT_NMTOKENS (** {b \{NMTOKENS\}} *)
  | NPT_Name (** {b \{Name\}} *)
  | NPT_NCName (** {b \{NCName\}} *)
  | NPT_ID (** {b \{ID\}} *)
  | NPT_IDREF (** {b \{IDREF\}} *)
  | NPT_ENTITY (** {b \{ENTITY\}} *)
  | NPT_integer (** {b \{integer\}} *)
  | NPT_nonPositiveInteger (** {b \{nonPositiveInteger\}} *)
  | NPT_negativeInteger (** {b \{negativeInteger\}} *)
  | NPT_long (** {b \{long\}} *)
  | NPT_int (** {b \{int\}} *)
  | NPT_short (** {b \{short\}} *)
  | NPT_byte (** {b \{byte\}} *)
  | NPT_nonNegativeInteger (** {b \{nonNegativeInteger\}} *)
  | NPT_unsignedLong (** {b \{unsignedLong\}} *)
  | NPT_unsignedInt (** {b \{unsignedInt\}} *)
  | NPT_unsignedShort (** {b \{unsignedShort\}} *)
  | NPT_unsignedByte (** {b \{unsignedByte\}} *)
  | NPT_positiveInteger (** {b \{positiveInteger\}} *)
  | NPT_yearMonthDuration (** {b \{yearMonthDuration\}} *)
  | NPT_dayTimeDuration (** {b \{dayTimeDuration\}} *)
  | NPT_dateTimeStamp (** {b \{dateTimeStamp\}} *)

type built_in_st =
  | STD_anySimpleType
  | STD_anyAtomicType
  | STD_primitive_type of primitive_type
  | STD_non_primitive_type of non_primitive_type

(** {b \{whiteSpace\}} value.

    The type constructors' order is significant: from less restrictive to
    most restrictive.
 *)
type whitespace =
  | W_replace
  | W_collapse
  | W_preserve

(** Atomic type length counters. *)
type length_counter =
  | Counter_hex
  | Counter_base64
  | Counter_char

type order =
  | Order_float
  | Order_int

type 'a bounds =
  | Bounds of 'a * 'a

type length_bounds =
    int option bounds

type value_bound =
  | Inclusive of Stringdata.t
  | Exclusive of Stringdata.t

type value_bounds =
    value_bound list bounds

type ext_regexp =
  | RE_regexp of regexp
  | RE_primitive_type of primitive_type
  | RE_non_primitive_type of non_primitive_type

type 'a facets =
    { f_length : 'a option;
      f_pattern : ext_regexp list;
      f_enumeration : Stringdata.t list option;
      f_value : (order * value_bounds) option }

type item_facets = (length_counter * length_bounds) facets
type list_facets = (length_bounds) facets


(** {b \{constraining facet\}}
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/datatypes.html#dt-constraining-facet}REF}
 *)
type cfacet =
  | CF_length of int * bool
  | CF_minLength of int * bool
  | CF_maxLength of int * bool
  | CF_pattern of regexp * bool
  | CF_enumeration of Stringdata.t list
  | CF_maxInclusive of Stringdata.t * bool
  | CF_maxExclusive of Stringdata.t * bool
  | CF_minExclusive of Stringdata.t * bool
  | CF_minInclusive of Stringdata.t * bool
  | CF_whiteSpace of whitespace * bool


type std_ref =
  | STD_built_in of built_in_st
  | STD_td_ref of td_id

(** the variable *)
type std_variety =
  | STD_atomic of std_ref
  | STD_list of std_ref
  | STD_union of std_ref list

type lstd_member =
  | LSTD_atomic of item_facets
  | LSTD_list of list_facets * item_facets

type st_unresolved =
    { std_facets : cfacet list;
      std_variety : std_variety }

type st_resolved =
    { lstd_whitespace : whitespace;
      lstd_members : lstd_member list }

(** Simple type definition. Variable ['st_inheritance] should be
    either {!st_resolved} or {!st_unresolved} depending if the
    inheritance propagation has been performed. *)
type 'st_inheritance std =
    { (* IGNORED {annotations} *)
      (* NOT NEEDED std_name_or_context : std_name_or_context; *)
      (* NOT NEEDED std_target : uri_id; *)
      (* IGNORED {final} *)
      std_details : 'st_inheritance }

type value_constraint =
  | A_default of Stringdata.t
  | A_fixed of Stringdata.t

type au =
    { (* IGNORED {annotations} *)
      au_required : bool; (** {b \{required\}} *)
      au_ad : ad_id ; (** {b \{attribute declaration\}} *)
      au_value_constraint : value_constraint option (** {b \{value constraint\}} *)
      (* IGNORED {inheritable} *)
    }

and ctd_ref_un =
  | CTD_anyType_un
  | CTD_td_ref_un of td_id
  | CTD_simpleContentType_un

type derivation_method_un =
  | Extension_un
  | Restriction_un

type ctd_variety_un =
  | CT_empty_un
  | CT_simple_un of td_id
  | CT_element_only_un of p
  | CT_mixed_un of p


(** {b \{content type\}} unresolved *)
type ct_unresolved =
    { ctd_base_un : ctd_ref_un;
      ctd_dm_un : derivation_method_un;
      ctd_variety_un : ctd_variety_un }

type ctd_ref =
  | CTD_anyType
  | CTD_td_ref of td_id


(** {b \{content type\}} resolved *)
type ct_resolved =
  | CT_anyType
  | CT_empty
  | CT_simple of td_id
  | CT_element_only of p
    (* IGNORED {open content} *)
  | CT_mixed of p
    (* IGNORED {open content} *)

(** Complex type definition. Variable ['ct_inheritance] should be
    either {!ct_resolved} or {!ct_unresolved} depending if the
    inheritance propagation has been performed. *)
type 'ct_inheritance ctd =
    { (* IGNORED a {annotations} *)
      (* NOT NEEDED ctd_name_or_context : ctd_name_or_context; *)
      (* NOT NEEDED ctd_targer : uri_id; *)
      (* IGNORED {final} *)
      (* IN ctd_ct ctd_base : ctd_ref; *)
      (* IN ctd_ct ctd_dm : derivation_method; *)
      (* IGNORED {abstract} *)
      ctd_au : agd_id list; (** {b \{attribute uses\}} *)
      ctd_aw : w option; (** {b \{attribute wildcard\}} *)
      ctd_ct : 'ct_inheritance; (** {b \{content type\}} *)
      (* IGNORED {prohibited substitutions} *)
      (* IGNORED {assertions} *)
    }

type ('st_inheritance,'ct_inheritance) td =
  | STD of 'st_inheritance std
  | CTD of 'ct_inheritance ctd

type ed =
    { (* a {annotations} *)
      ed_ncname : Stringdata.t; (** {b \{name\}} *)
      ed_target : uri_id; (** {b \{target namespace\}} *)
      ed_td : ctd_ref; (** {b \{type definition\}} *)
      (* tt {type table} *)
      (* NOT NEEDED {b \{scope\}} *)
      ed_value_constraint : value_constraint option; (** {b \{value constraint\}} *)
      (* {nillable} *)
      (* icd {identity-constraint definitions} *)
      (* IGNORED ed {substitution group affiliations} *)
      (* IGNORED {substitution group exclusions} *)
      (* {disallowed substitutions} *)
      (* IGNORED {abstract} *)
    }

(** Schema Component: {b \{attribute declarations\}} *)
and ad =
    { (* IGNORED {annotations} *)
      ad_name : Stringdata.t; (** {b \{name\}} *)
      ad_target : uri_id; (** {b \{target namespace\}} *)
      ad_std : td_id; (** {b \{type definition\}} *)
      ad_value_constraint : value_constraint option (** {b \{value constraint\}} *)
      (* IGNORED {inheritable} *)
    }

type au_unresolved =
  | AU_unresolved of au list * agd_id list

type au_resolved =
  | AU_resolved of au list

(** Schema Component: {b \{attribute group definitions\}}. Variable
    ['au_inheritance] should be either {!au_unresolved} or
    {!au_resolved} depending if the attribute use inheritance
    propagation has been performed.  *)
type 'au_inheritance agd =
    { agd_uses : 'au_inheritance;  (** {b \{attribute uses\}} *)
      agd_w : w option; (** {b \{attribute wildcard\}} *)
    }

type mgd =
    (* IGNORED {annotations} *)
    (* NOT NEEDED {name} *)
    (* NOT NEEDED {target namespace} *)
    (* mgd merged with mg *)
  | MG_all of p list
  | MG_choice of p list
  | MG_sequence of p list

(** {b \{schema\}} *)
type ('st_inheritance,'ct_inheritance,'au_inheritance) schema =
    { target : uri_id;
      uris : (uri_id * Stringdata.t) list;
      (* a {annotations} *)
      tds : (td_id * ('st_inheritance,'ct_inheritance) td) list; (** {b \{type definitions\}} *)
      ads : (ad_id * ad) list; (** {b \{attribute declarations\}} *)
      eds : (ed_id * ed) list; (** {b \{element declarations\}} *)
      agds : (agd_id * 'au_inheritance agd) list; (** {b \{attribute group definitions\}} *)
      mgds : (mgd_id * mgd) list; (** {b \{model group definitions\}} *)
      (* nd {notation declarations} *)
      (* icd {identity-constraint definitions} *)
      roots : ed_id list; (** root elements (as {b \{scope\}} {b \{global\}}) *)
    }


type resolved_schema =
    (st_resolved,ct_resolved,au_resolved) schema


(** XSD's component constraints {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#outcome-cos}REF} *)
type cos =
  | COS_applicable_facets
  | COS_st_restricts_2_1

(** XSD's Schema Representation Constraint {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#outcome-src}REF} *)
type src =
  | SRC_ct_5
  | SRC_import_1_1
  | SRC_import_3_1

type scc =
  | SCC_whiteSpace_valid_restriction (** {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/#whiteSpace-valid-restriction}REF} *)
  | SCC_length_valid_restriction (** {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/#length-valid-restriction}REF} *)
  | SCC_maxminExInclusive_valid_restriction (** {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/#maxInclusive-valid-restriction}REF} and others *)
  | SCC_ct_extends (** {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/#cos-ct-extends}REF} Warning, in the W3C recommendation, the label is a COS but the title is a SCC *)
  | SCC_derivation_ok_restriction
 (** {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/#derivation-ok-restriction}REF} *)


(** XSD's {b \{Validation rules\}} *)
type cvc =
  | CVC_accept
  | CVC_accept_1
  | CVC_accept_2
  | CVC_accept_3
  | CVC_assess_attr
  | CVC_assess_elt
  | CVC_assess_elt_1
  | CVC_assess_elt_2
  | CVC_assess_elt_3
  | CVC_attribute
  | CVC_au
  | CVC_complex_content
  | CVC_complex_content_1
  | CVC_complex_type
  | CVC_complex_type_1_1
  | CVC_complex_type_1_2
  | CVC_complex_type_1_3
  | CVC_complex_type_1_4
  | CVC_complex_type_2
  | CVC_complex_type_3
  | CVC_complex_type_4
  | CVC_datatype_valid
  | CVC_datatype_valid_1
  | CVC_datatype_valid_2_1
  | CVC_datatype_valid_2_2
  | CVC_datatype_valid_2_3
  | CVC_elt
  | CVC_elt_5_2_1
  | CVC_elt_5_2_2
  | CVC_enumeration_valid
  | CVC_facet_valid
  | CVC_length_valid
  | CVC_maxLength_valid
  | CVC_minLength_valid
  | CVC_minInclusive_valid
  | CVC_maxInclusive_valid
  | CVC_minExclusive_valid
  | CVC_maxExclusive_valid
  | CVC_model_group
  | CVC_model_group_all
  | CVC_model_group_choice
  | CVC_model_group_sequence
  | CVC_particle
  | CVC_pattern_valid
  | CVC_simple_type_p of primitive_type option
  | CVC_simple_type_np of non_primitive_type option
  | CVC_type_3_1_1
  | CVC_type_3_1_2
  | CVC_type_3_1
  | CVC_type_3_2
  | CVC_type
  | CVC_wildcard
  | CVC_wildcard_1
  | CVC_wildcard_name
  | CVC_wildcard_name_1
  | CVC_wildcard_namespace
  | CVC_wildcard_namespace_1
  | CVC_wildcard_namespace_2
  | CVC_wildcard_namespace_3
  | Skip_wildcard_valid
  | Strict_wildcard_valid
  | Lax_wildcard_valid
  | AnyType_valid
  | Schema_root_valid

