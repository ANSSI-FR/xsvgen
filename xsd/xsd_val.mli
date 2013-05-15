(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd_val.mli 1459 2012-04-25 15:12:38Z maarek $ *)

val uri : Stringdata.t
val instance_uri : Stringdata.t
val version_1_1 : Stringdata.t
val version_1_0 : Stringdata.t
val ct_anyType : Xml.expanded_name
val st_anySimpleType : Xml.expanded_name
val st_anyAtomicType : Xml.expanded_name
val st_pt_string : Xml.expanded_name
val st_pt_boolean : Xml.expanded_name
val st_pt_decimal : Xml.expanded_name
val st_pt_float : Xml.expanded_name
val st_pt_double : Xml.expanded_name
val st_pt_duration : Xml.expanded_name
val st_pt_dateTime : Xml.expanded_name
val st_pt_time : Xml.expanded_name
val st_pt_date : Xml.expanded_name
val st_pt_gYearMonth : Xml.expanded_name
val st_pt_gYear : Xml.expanded_name
val st_pt_gMonthDay : Xml.expanded_name
val st_pt_gDay : Xml.expanded_name
val st_pt_gMonth : Xml.expanded_name
val st_pt_hexBinary : Xml.expanded_name
val st_pt_base64Binary : Xml.expanded_name
val st_pt_anyURI : Xml.expanded_name
val st_pt_QName : Xml.expanded_name
val st_pt_NOTATION : Xml.expanded_name
val st_npt_normalizedString : Xml.expanded_name
val st_npt_token : Xml.expanded_name
val st_npt_language : Xml.expanded_name
val st_npt_IDREFS : Xml.expanded_name
val st_npt_ENTITIES : Xml.expanded_name
val st_npt_NMTOKEN : Xml.expanded_name
val st_npt_NMTOKENS : Xml.expanded_name
val st_npt_Name : Xml.expanded_name
val st_npt_NCName : Xml.expanded_name
val st_npt_ID : Xml.expanded_name
val st_npt_IDREF : Xml.expanded_name
val st_npt_ENTITY : Xml.expanded_name
val st_npt_integer : Xml.expanded_name
val st_npt_nonPositiveInteger : Xml.expanded_name
val st_npt_negativeInteger : Xml.expanded_name
val st_npt_long : Xml.expanded_name
val st_npt_int : Xml.expanded_name
val st_npt_short : Xml.expanded_name
val st_npt_byte : Xml.expanded_name
val st_npt_nonNegativeInteger : Xml.expanded_name
val st_npt_unsignedLong : Xml.expanded_name
val st_npt_unsignedInt : Xml.expanded_name
val st_npt_unsignedShort : Xml.expanded_name
val st_npt_unsignedByte : Xml.expanded_name
val st_npt_positiveInteger : Xml.expanded_name
val st_npt_yearMonthDuration : Xml.expanded_name
val st_npt_dayTimeDuration : Xml.expanded_name
val st_npt_dateTimeStamp : Xml.expanded_name


val e_schema : Xml.expanded_name
val e_include : Xml.expanded_name
val e_import : Xml.expanded_name
val e_redefine : Xml.expanded_name
val e_override : Xml.expanded_name
val e_annotation : Xml.expanded_name
val e_appinfo : Xml.expanded_name
val e_documentation : Xml.expanded_name
val e_defaultOpenContent : Xml.expanded_name
val e_simpleType : Xml.expanded_name
val e_simpleContent : Xml.expanded_name
val e_complexType : Xml.expanded_name
val e_complexContent : Xml.expanded_name
val e_group : Xml.expanded_name
val e_attributeGroup : Xml.expanded_name
val e_element : Xml.expanded_name
val e_attribute : Xml.expanded_name
val e_notation : Xml.expanded_name
val e_alternative : Xml.expanded_name
val e_unique : Xml.expanded_name
val e_key : Xml.expanded_name
val e_keyref : Xml.expanded_name
val e_all : Xml.expanded_name
val e_any : Xml.expanded_name
val e_choice : Xml.expanded_name
val e_sequence : Xml.expanded_name
val e_anyAttribute : Xml.expanded_name
val e_assert : Xml.expanded_name
val e_restriction : Xml.expanded_name
val e_extension : Xml.expanded_name
val e_list : Xml.expanded_name
val e_union : Xml.expanded_name
val e_minExclusive : Xml.expanded_name
val e_minInclusive : Xml.expanded_name
val e_maxExclusive : Xml.expanded_name
val e_maxInclusive : Xml.expanded_name
val e_totalDigits : Xml.expanded_name
val e_fractionDigits : Xml.expanded_name
val e_length : Xml.expanded_name
val e_minLength : Xml.expanded_name
val e_maxLength : Xml.expanded_name
val e_enumeration : Xml.expanded_name
val e_whiteSpace : Xml.expanded_name
val e_pattern : Xml.expanded_name
val e_assertion : Xml.expanded_name
val e_explicitTimezone : Xml.expanded_name

val a_attributeFormDefault : Xml.expanded_name
val a_blockDefault : Xml.expanded_name
val a_defaultAttributes : Xml.expanded_name
val a_xpathDefaultNamespace : Xml.expanded_name
val a_elementFormDefault : Xml.expanded_name
val a_finalDefault : Xml.expanded_name
val a_id : Xml.expanded_name
val a_version : Xml.expanded_name
val a_targetNamespace : Xml.expanded_name
val a_namespace : Xml.expanded_name
val a_schemaLocation : Xml.expanded_name
val a_abstract : Xml.expanded_name
val a_block : Xml.expanded_name
val a_default : Xml.expanded_name
val a_final : Xml.expanded_name
val a_fixed : Xml.expanded_name
val a_form : Xml.expanded_name
val a_maxOccurs : Xml.expanded_name
val a_minOccurs : Xml.expanded_name
val a_name : Xml.expanded_name
val a_nillable : Xml.expanded_name
val a_ref : Xml.expanded_name
val a_substitutionGroup : Xml.expanded_name
val a_type : Xml.expanded_name
val a_mixed : Xml.expanded_name
val a_defaultAttributesApply : Xml.expanded_name
val a_base : Xml.expanded_name
val a_itemType : Xml.expanded_name
val a_memberTypes : Xml.expanded_name
val a_value : Xml.expanded_name
val a_use : Xml.expanded_name
val a_inheritable : Xml.expanded_name
val a_notNamespace : Xml.expanded_name
val a_notQName : Xml.expanded_name
val a_processContents : Xml.expanded_name


val v_true : Stringdata.t
val v_true_1 : Stringdata.t
val v_false : Stringdata.t
val v_false_0 : Stringdata.t

val v_w_preserve : Stringdata.t
val v_w_replace : Stringdata.t
val v_w_collapse : Stringdata.t

val v_qualified : Stringdata.t
val v_unqualified : Stringdata.t

val v_optional : Stringdata.t
val v_required : Stringdata.t
val v_prohibited : Stringdata.t

val v_hhany : Stringdata.t
val v_hhother : Stringdata.t

val v_hhtargetNamespace : Stringdata.t
val v_hhlocal : Stringdata.t

val v_strict : Stringdata.t
val v_skip : Stringdata.t
val v_lax : Stringdata.t

val v_unbounded : Stringdata.t
