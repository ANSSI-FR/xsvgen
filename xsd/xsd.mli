(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd.mli 1686 2012-05-30 09:00:22Z maarek $ *)

(** {1 XSD components types} *)

(** The OCaml types defined in this module follow the "Schema
    Component" as defined in
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#components}REF}. Each
    Schema component or field is refered to in {b \{bold\}} in this
    module's documentation.

  Most of the acronyms used are the one used for hyperlinking
 definitions in
 {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/}REF}.
*)

(** {2 Types to differentiate {!Stringdata.t} values} *)

(** URI *)
type uri = Uri of Stringdata.t

(** ID *)
type id = Id of Stringdata.t

(** XML "non-colonized" name *)
type ncname = Ncname of Stringdata.t

(** {2 Types shared across components} *)

(** Value or reference *)
type 'a reference =
  | Val of 'a (** Value *)
  | Ref of Xml.expanded_name (** Reference *)

(** Occurrence *)
type occurrence =
    { min_occurs : int;
      max_occurs : max_occurrence_val;
    }

(** Upper bound *)
and max_occurrence_val =
  | Occur of int
  | Unbounded

(** {b \{process contents\}} *)
type process_contents =
  | Skip
  | Strict
  | Lax

(** {b \{namespace constraint\}}.{b \{disallowed names\}} *)
type nc_disallowed =
  | NC_qname of Xml.expanded_name (* qname *)
  | NC_defined
  | NC_sibling

(** {b \{complex type definition\}}.{b \{derivation method\}} *)
type derivation_method =
  | Extension
  | Restriction

(** {b \{whiteSpace\}} value

    The type constructors' order is significant: from less restrictive to
    most restrictive.
*)
type whitespace =
  | W_replace
  | W_collapse
  | W_preserve

(** Values for attribute declarations' "use" attribute *)
type use =
  | Optional
  | Prohibited
  | Required

type form =
  | Qualified
  | Unqualified

type scope =
  | Global
  | Local

(** Values for wildcard declaration's "namespace" attribute *)
type nc_variety =
  | Any
  | Not
  | Enumeration

(** {2 Primitive and built-in types} *)

(** Primitive type *)
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
  | PT_base64Binary (** {b base64Binary} *)
  | PT_anyURI (** {b \{anyURI\}} *)
  | PT_QName (** {b \{QName\}} *)
  | PT_NOTATION (** {b \{NOTATION\}} *)

(** Non primitive built-in type *)
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
  | STD_anySimpleType (** {b \{anySimpleType\}} *)
  | STD_anyAtomicType (** {b \{anyAtomicType\}} *)
  | STD_primitive_type of primitive_type
  | STD_non_primitive_type of non_primitive_type


(** {2 XSD's regular expressions} *)

(** AST of XSD's regular expressions {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/#regexs}REF} *)
type regexp =
  | RE_regexp of re_branch list
and re_branch =
  | RE_branch of re_piece list
and re_piece =
  | RE_piece of re_atom * re_quantifier option
and re_atom =
  | RE_atom of Utf8.range
  | RE_atom_re of regexp
and re_quantifier =
  | RE_none_or_one
  | RE_one_or_more
  | RE_none_or_more
  | RE_quantity of re_quantity
and re_quantity =
  | RE_range of int * int
  | RE_min of int
  | RE_exact of int


(** {2 Facets} *)

(** {b \{constraining facet\}}
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/datatypes.html#dt-constraining-facet}REF}
 *)
type cfacet =
  | CF_length of int (** {b \{value\}} *) * bool (** {b \{fixed\}} *)
  | CF_minLength of int (** {b \{value\}} *) * bool (** {b \{fixed\}} *)
  | CF_maxLength of int (** {b \{value\}} *) * bool (** {b \{fixed\}} *)
  | CF_pattern of regexp (** {b \{value\}} *) * bool (** {b \{fixed\}} *)
  | CF_enumeration of Stringdata.t (** {b \{value\}} *)
  | CF_whiteSpace of whitespace (** {b \{value\}} *) * bool (** {b \{fixed\}} *)
  | CF_maxInclusive of Stringdata.t (** {b \{value\}} *) * bool (** {b \{fixed\}} *)
  | CF_maxExclusive of Stringdata.t (** {b \{value\}} *) * bool (** {b \{fixed\}} *)
  | CF_minExclusive of Stringdata.t (** {b \{value\}} *) * bool (** {b \{fixed\}} *)
  | CF_minInclusive of Stringdata.t (** {b \{value\}} *) * bool (** {b \{fixed\}} *)
  | CF_totalDigits (** Not supported *)
  | CF_fractionDigits (** Not supported *)
  | CF_assertions (** Not supported *)
  | CF_explicitTimezone (** Not supported *)

(** {b \{fundamental facets\}}
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/datatypes.html#dt-fundamental-facet}REF} *)
type ffacet =
  | FF_ordered
  | FF_bounded
  | FF_cardinality
  | FF_numeric

(** {2 Schema components} *)

(** Schema Component: {b \{schema\}}

   Schema representation dependent on its form ({!composite} of
   {!inlined}) and on its genericity ({!uri} [option] for generic and
   {!uri} for specialised). *)
type ('f,'uri) schema =
    { schema_form : 'f;
      target : 'uri;
      (* a {annotations} *)
      definitions : 'uri definition list
      (* nd {notation declarations} *)
      (* icd {identity-constraint definitions} *)
    }
(** Definition (type, attribute, element, attribute group, model
    group) *)
and 'uri definition =
  | TD of 'uri td
  | AD of 'uri ad
  | ED of 'uri ed
  | AGD of 'uri agd
  | MGD of 'uri mgd

(** Composition *)
and composition =
(* | Include *)
  | Import of Stringdata.t option * Stringdata.t (** namespace schemaLocation *)
(* | Redefine *)
(* | Override *)

(** {b \{type definition\}} *)
and 'uri td =
  | STD of 'uri std
  | CTD of 'uri ctd

(** Schema Component: {b \{simple type definition\}} *)
and 'uri std =
    { (* IGNORED {annotations} *)
      std_name_or_context : std_name_or_context; (** {b \{name\}} or {b \{context\}} *)
      std_target : 'uri; (** {b \{target namespace\}} *)
      (* IGNORED {final} *)
      std_facets : cfacet list; (** {b \{facets\}} *)
      std_ffacets : ffacet list; (** {b \{fundamental facets\}} *)
      std_variety : 'uri std_variety (** {b \{variety\}} with {b \{primitive type definition\}} if {b \{atomic\}}, with {b \{base type definition\}} if {b \{atomic\}}, with {b \{base type definition\}}={b \{anySimpleType\}} if {b \{list\}} or {b \{union\}} *)
    }

(** A reference to a simple type definition. *)
and 'uri std_variety =
  | STD_atomic of 'uri std_ref (** {b \{primitive type definition\}} *)
  | STD_list of 'uri std_ref (** {b \{item type definition\}} (TODO check definition {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#std-item_type_definition}REF} which looks incoherent *)
  | STD_union of ('uri std_ref) list (** {b \{member type definitions\}} *)

(** Mutually exclusive fields {b \{name\}} and {b \{context\}} of a {b simple
    type definition}. The context information is not informed. {b
    name} can only be present for global {b \{simpleType\}}
    definitions. *)
and std_name_or_context =
  | STD_name of ncname
  | STD_context (* TODO check if the differenciation _ad _ed _ctd _std is needed *)

(** A reference to a simple type. *)
and 'uri std_ref =
  | STD_built_in of built_in_st
  | STD_td_ref of 'uri td reference

(** Schema Component: {b \{complex type definition\}} *)
and 'uri ctd =
    { (* IGNORED a {annotations} *)
      ctd_name_or_context : ctd_name_or_context; (** {b \{name\}} or {b \{context\}} *)
      ctd_target : 'uri;
      ctd_base : 'uri ctd_ref;
      (* IGNORED {final} *)
      ctd_dm : derivation_method;
      (* IGNORED {abstract} *)
      ctd_au : 'uri aus list; (** {b \{attribute uses\}} *)
      ctd_aw : ('uri w * int) option; (** {b \{attribute wildcard\}} plus an integer for determining the document order as prescribed by {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#declare-attributeGroup-wildcard}REF} *)
      ctd_ct : 'uri ct; (** {b \{content type\}} *)
      (* IGNORED {prohibited substitutions} *)
      (* IGNORED {assertions} *)
    }

(** A reference to a complex type *)
(* TODO: Is it possible to turn CTD_td_ref into: CTD_td_ref of 'uri ctd reference ? *)
and 'uri ctd_ref =
  | CTD_anyType (** {b \{anyType\}} *)
  | CTD_td_ref of 'uri td reference
  | CTD_simpleContentType
  | CTD_built_in_simpleType of built_in_st

(** Mutually exclusive fields {b \{name\}} and {b \{context\}} of a {b complex
    type definition}. The context information is not informed. *)
and ctd_name_or_context =
  | CTD_name of ncname
  | CTD_context (* TODO check if the differenciation _ed _ctd is needed *)

and value_constraint =
  | A_default of Stringdata.t
  | A_fixed of Stringdata.t

(** Schema Component: {b \{attribute declarations\}} *)
and 'uri ad =
    { (* IGNORED {annotations} *)
      ad_name : ncname; (** {b \{name\}} *)
      ad_target : 'uri; (** {b \{target namespace\}} *)
      ad_std : 'uri std_ref; (** {b \{type definition\}} *)
      ad_scope : scope; (** {b \{scope\}} *)
      ad_value_constraint : value_constraint option (** {b \{value constraint\}} *)
      (* IGNORED {inheritable} *)
    }


(** Schema Component: {b \{element declarations\}} *)
and 'uri ed =
    { (* IGNORED a {annotations} *)
      ed_ncname : ncname; (** {b \{name\}} *)
      ed_target : 'uri; (** {b \{target namespace\}} *)
      ed_td : 'uri ctd_ref; (** {b \{type definition\}} *)
      (* IGNORED tt {type table} *)
      ed_scope : scope; (** {b \{scope\}} *)
      ed_value_constraint : value_constraint option; (** {b \{value constraint\}} *)
      (* IGNORED {nillable} *)
      (* IGNORED icd {identity-constraint definitions} *)
      (* IGNORED ed {substitution group affiliations} *)
      (* IGNORED {substitution group exclusions} *)
      (* IGNORED {disallowed substitutions} *)
      (* IGNORED {abstract} *)
    }

(** Schema Component: {b \{attribute group definitions\}} *)
and 'uri agd =
    { (* IGNORED {annotations} *)
      agd_name : ncname; (** {b \{name\}} *)
      agd_target : 'uri; (** {b \{target namespace\}} *)
      agd_uses : 'uri aus list;  (** {b \{attribute uses\}} *)
      agd_w : ('uri w * int) option; (** {b \{attribute wildcard\}} plus an integer for determining the document order as prescribed by {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#declare-attributeGroup-wildcard}REF} *)
    }

(** Schema Component: {b \{model group definitions\}} *)
and 'uri mgd =
    { (* IGNORED {annotations} *)
      mgd_name : ncname;
      mgd_target : 'uri;
      mgd_mg : 'uri mg;
    }

(** {b \{content type\}} *)
and 'uri ct =
  | CT_empty
  | CT_simple of 'uri std
  | CT_element_only of 'uri p (* IGNORED {open content} *)
  | CT_mixed of 'uri p  (* IGNORED {open content} *)

(** {b \{model group\}} *)
 and 'uri mg =
    (* IGNORED {annotations} *)
  | MG_all of 'uri p list
  | MG_choice of 'uri p list
  | MG_sequence of 'uri p list

(** Schema Component: {b \{particle\}} *)
and 'uri p =
    { p_occurrence : occurrence;
      p_term : 'uri t
     (* IGNORED {annotations} *)
    }

(** {b \{term\}} *)
and 'uri t =
  | T_ed of 'uri ed reference
  | T_mg of 'uri mg
  | T_mgd of 'uri mgd reference
  | T_w of 'uri w

(** {b \{wildcard\}} *)
and 'uri w =
    { (* IGNORED {annotations} *)
      w_nc : 'uri nc;
      w_pc : process_contents
    }

(** Schema Component: {b \{attribute use\}} *)
and 'uri au =
    { (* IGNORED {annotations} *)
      au_use : use; (** {b \{required\}} plus "prohibited" *)
      au_ad : 'uri ad reference; (** {b \{attribute declaration\}} *)
      au_value_constraint : value_constraint option (** {b \{value constraint\}} *)
      (* IGNORED {inheritable} *)
    }

(** Attribute use or attribute group reference *)
and 'uri aus =
  | AUS_au of 'uri au
  | AUS_agd_ref of Xml.expanded_name

(** Schema Component: {b \{namespace constraint\}} *)
and 'uri nc =
  | NC_any of nc_disallowed list
  | NC_enumeration of 'uri option list * nc_disallowed list
  | NC_not of 'uri option list * nc_disallowed list

(** Form for schema with multiple compositions. *)
type composite =
    Modules of composition list

(** Form for flatten schema (compositions are inlined if they exist). *)
type inlined =
  | Inlined

