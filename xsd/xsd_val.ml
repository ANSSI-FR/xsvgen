(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd_val.ml 1686 2012-05-30 09:00:22Z maarek $ *)

(** {1 XSD values} *)

(** {2 XSD URI and versions} *)
let uri = Stringdata.of_string "http://www.w3.org/2001/XMLSchema"
let instance_uri = Stringdata.of_string "http://www.w3.org/2001/XMLSchema-instance"

let version_1_1 = Stringdata.of_string "1.1"
let version_1_0 = Stringdata.of_string "1.0"

let name ln = { Xml.uri = uri; Xml.name = ln }

let attr_name ln = { Xml.uri = Xml_ns.empty_expanded_ns; Xml.name = ln }

(** {2 XSD predefined types} *)

let ct_anyType = name (Stringdata.of_string "anyType")
let st_anySimpleType = name (Stringdata.of_string "anySimpleType")
let st_anyAtomicType = name (Stringdata.of_string "anyAtomicType")
let st_pt_string = name (Stringdata.of_string "string")
let st_pt_boolean = name (Stringdata.of_string "boolean")
let st_pt_decimal = name (Stringdata.of_string "decimal")
let st_pt_float = name (Stringdata.of_string "float")
let st_pt_double = name (Stringdata.of_string "double")
let st_pt_duration = name (Stringdata.of_string "duration")
let st_pt_dateTime = name (Stringdata.of_string "dateTime")
let st_pt_time = name (Stringdata.of_string "time")
let st_pt_date = name (Stringdata.of_string "date")
let st_pt_gYearMonth = name (Stringdata.of_string "gYearMonth")
let st_pt_gYear = name (Stringdata.of_string "gYear")
let st_pt_gMonthDay = name (Stringdata.of_string "gMonthDay")
let st_pt_gDay = name (Stringdata.of_string "gDay")
let st_pt_gMonth = name (Stringdata.of_string "gMonth")
let st_pt_hexBinary = name (Stringdata.of_string "hexBinary")
let st_pt_base64Binary = name (Stringdata.of_string "base64Binary")
let st_pt_anyURI = name (Stringdata.of_string "anyURI")
let st_pt_QName = name (Stringdata.of_string "QName")
let st_pt_NOTATION = name (Stringdata.of_string "NOTATION")
let st_npt_normalizedString = name (Stringdata.of_string "normalizedString")
let st_npt_token = name (Stringdata.of_string "token")
let st_npt_language = name (Stringdata.of_string "language")
let st_npt_IDREFS = name (Stringdata.of_string "IDREFS")
let st_npt_ENTITIES = name (Stringdata.of_string "ENTITIES")
let st_npt_NMTOKEN = name (Stringdata.of_string "NMTOKEN")
let st_npt_NMTOKENS = name (Stringdata.of_string "NMTOKENS")
let st_npt_Name = name (Stringdata.of_string "Name")
let st_npt_NCName = name (Stringdata.of_string "NCName")
let st_npt_ID = name (Stringdata.of_string "ID")
let st_npt_IDREF = name (Stringdata.of_string "IDREF")
let st_npt_ENTITY = name (Stringdata.of_string "ENTITY")
let st_npt_integer = name (Stringdata.of_string "integer")
let st_npt_nonPositiveInteger = name (Stringdata.of_string "nonPositiveInteger")
let st_npt_negativeInteger = name (Stringdata.of_string "negativeInteger")
let st_npt_long = name (Stringdata.of_string "long")
let st_npt_int = name (Stringdata.of_string "int")
let st_npt_short = name (Stringdata.of_string "short")
let st_npt_byte = name (Stringdata.of_string "byte")
let st_npt_nonNegativeInteger = name (Stringdata.of_string "nonNegativeInteger")
let st_npt_unsignedLong = name (Stringdata.of_string "unsignedLong")
let st_npt_unsignedInt = name (Stringdata.of_string "unsignedInt")
let st_npt_unsignedShort = name (Stringdata.of_string "unsignedShort")
let st_npt_unsignedByte = name (Stringdata.of_string "unsignedByte")
let st_npt_positiveInteger = name (Stringdata.of_string "positiveInteger")
let st_npt_yearMonthDuration = name (Stringdata.of_string "yearMonthDuration")
let st_npt_dayTimeDuration = name (Stringdata.of_string "dayTimeDuration")
let st_npt_dateTimeStamp = name (Stringdata.of_string "dateTimeStamp")

(** {2 XSD elements} *)

let e_schema = name (Stringdata.of_string "schema")
let e_include = name (Stringdata.of_string "include")
let e_import = name (Stringdata.of_string "import")
let e_redefine = name (Stringdata.of_string "redefine")
let e_override = name (Stringdata.of_string "override")
let e_annotation = name (Stringdata.of_string "annotation")
let e_appinfo = name (Stringdata.of_string "appinfo")
let e_documentation = name (Stringdata.of_string "documentation")
let e_defaultOpenContent = name (Stringdata.of_string "defaultOpenContent")
let e_simpleType = name (Stringdata.of_string "simpleType")
let e_simpleContent = name (Stringdata.of_string "simpleContent")
let e_complexType = name (Stringdata.of_string "complexType")
let e_complexContent = name (Stringdata.of_string "complexContent")
let e_group = name (Stringdata.of_string "group")
let e_attributeGroup = name (Stringdata.of_string "attributeGroup")
let e_element = name (Stringdata.of_string "element")
let e_attribute = name (Stringdata.of_string "attribute")
let e_notation = name (Stringdata.of_string "notation")
let e_alternative = name (Stringdata.of_string "alternative")
let e_unique = name (Stringdata.of_string "unique")
let e_key = name (Stringdata.of_string "key")
let e_keyref = name (Stringdata.of_string "keyref")
let e_all = name (Stringdata.of_string "all")
let e_any = name (Stringdata.of_string "any")
let e_choice = name (Stringdata.of_string "choice")
let e_sequence = name (Stringdata.of_string "sequence")
let e_anyAttribute = name (Stringdata.of_string "anyAttribute")
let e_assert = name (Stringdata.of_string "assert")
let e_restriction = name (Stringdata.of_string "restriction")
let e_extension = name (Stringdata.of_string "extension")
let e_list = name (Stringdata.of_string "list")
let e_union = name (Stringdata.of_string "union")
let e_minExclusive = name (Stringdata.of_string "minExclusive")
let e_minInclusive = name (Stringdata.of_string "minInclusive")
let e_maxExclusive = name (Stringdata.of_string "maxExclusive")
let e_maxInclusive = name (Stringdata.of_string "maxInclusive")
let e_totalDigits = name (Stringdata.of_string "totalDigits")
let e_fractionDigits = name (Stringdata.of_string "fractionDigits")
let e_length = name (Stringdata.of_string "length")
let e_minLength = name (Stringdata.of_string "minLength")
let e_maxLength = name (Stringdata.of_string "maxLength")
let e_enumeration = name (Stringdata.of_string "enumeration")
let e_whiteSpace = name (Stringdata.of_string "whiteSpace")
let e_pattern = name (Stringdata.of_string "pattern")
let e_assertion = name (Stringdata.of_string "assertion")
let e_explicitTimezone = name (Stringdata.of_string "explicitTimezone")

(** {2 XSD attributes} *)

let a_attributeFormDefault = attr_name (Stringdata.of_string "attributeFormDefault")
let a_blockDefault = attr_name (Stringdata.of_string "blockDefault")
let a_defaultAttributes = attr_name (Stringdata.of_string "defaultAttributes")
let a_xpathDefaultNamespace = attr_name (Stringdata.of_string "xpathDefaultNamespace")
let a_elementFormDefault = attr_name (Stringdata.of_string "elementFormDefault")
let a_finalDefault = attr_name (Stringdata.of_string "finalDefault")
let a_id = attr_name (Stringdata.of_string "id")
let a_version = attr_name (Stringdata.of_string "version")
let a_targetNamespace = attr_name (Stringdata.of_string "targetNamespace")
let a_namespace = attr_name (Stringdata.of_string "namespace")
let a_schemaLocation = attr_name (Stringdata.of_string "schemaLocation")
let a_abstract = attr_name (Stringdata.of_string "abstract")
let a_block = attr_name (Stringdata.of_string "block")
let a_default = attr_name (Stringdata.of_string "default")
let a_final = attr_name (Stringdata.of_string "final")
let a_fixed = attr_name (Stringdata.of_string "fixed")
let a_form = attr_name (Stringdata.of_string "form")
let a_maxOccurs = attr_name (Stringdata.of_string "maxOccurs")
let a_minOccurs = attr_name (Stringdata.of_string "minOccurs")
let a_name = attr_name (Stringdata.of_string "name")
let a_nillable = attr_name (Stringdata.of_string "nillable")
let a_ref = attr_name (Stringdata.of_string "ref")
let a_substitutionGroup = attr_name (Stringdata.of_string "substitutionGroup")
let a_type = attr_name (Stringdata.of_string "type")
let a_mixed = attr_name (Stringdata.of_string "mixed")
let a_defaultAttributesApply = attr_name (Stringdata.of_string "defaultAttributesApply")
let a_base = attr_name (Stringdata.of_string "base")
let a_itemType = attr_name (Stringdata.of_string "itemType")
let a_memberTypes = attr_name (Stringdata.of_string "memberTypes")
let a_value = attr_name (Stringdata.of_string "value")
let a_use = attr_name (Stringdata.of_string "use")
let a_inheritable = attr_name (Stringdata.of_string "inheritable")
let a_notNamespace = attr_name (Stringdata.of_string "notNamespace")
let a_notQName = attr_name (Stringdata.of_string "notQName")
let a_processContents = attr_name (Stringdata.of_string "processContents")


(** {2 XSD values} *)

let v_true = Stringdata.of_string "true"
let v_true_1 = Stringdata.of_string "1"
let v_false = Stringdata.of_string "false"
let v_false_0 = Stringdata.of_string "0"

let v_w_preserve = Stringdata.of_string "preserve"
let v_w_replace = Stringdata.of_string "replace"
let v_w_collapse = Stringdata.of_string "collapse"

let v_qualified = Stringdata.of_string "qualified"
let v_unqualified = Stringdata.of_string "unqualified"

let v_optional = Stringdata.of_string "optional"
let v_required = Stringdata.of_string "required"
let v_prohibited = Stringdata.of_string "prohibited"

let v_hhany = Stringdata.of_string "##any"
let v_hhother = Stringdata.of_string "##other"

let v_hhtargetNamespace = Stringdata.of_string "##targetNamespace"
let v_hhlocal = Stringdata.of_string "##local"


let v_strict = Stringdata.of_string "strict"
let v_skip = Stringdata.of_string "skip"
let v_lax = Stringdata.of_string "lax"

let v_unbounded = Stringdata.of_string "unbounded"
