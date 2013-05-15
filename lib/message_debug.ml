(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: message_debug.ml 1704 2012-06-04 16:49:33Z maarek $ *)

(** {1 Message handling library, debug version} *)


(** Information *)
type info =
  | INFO_file_XML_valid of File_in.t
  | INFO_file_XML_invalid
  | INFO_file_XML_notwf
  | INFO_validating_element of Stringdata.t
  | INFO_testing_element of Stringdata.t
  | INFO_valid_element of Stringdata.t
  | INFO_invalid_element of Stringdata.t
  | INFO_fetching_td of int
  | INFO_fetching_ed of int
  | INFO_fetching_ad of int
  | INFO_fetching_agd of int
  | INFO_fetching_mgd of int
  | INFO_document_valid
  | INFO_pattern of Stringdata.t
  | INFO_XSD_primary_file of File_in.t
  | INFO_XSD_secondary_file of File_in.t
  | INFO_validator_configuration of Xml.configuration

let valid_exit_code = 10

let default_error_exit_code = 13

let error_exit_code = function
    (* 11: invalid but well-formed *)
  | Error.RE_block_unknown _
  | Error.RE_syntax _
  | Error.RE_UTF8_invalid
  | Error.RE_LIM_charprop_not_implemented
  | Error.XSDT_skip_process_contents_risky
  | Error.XSDT_version_not_supported
  | Error.XSDT_version_1_0_not_supported
  | Error.XSDT_version_missing
  | Error.XSDT_qname_namespace_missing
  | Error.XSDT_qname_not_well_formed
  | Error.XSDT_mixed_values_contradictory
  | Error.XSDT_min_max_inconsistent
  | Error.XSDT_constructions_mutually_exclusive _
  | Error.XSDT_extra_xsd_attribute_not_recognised
  | Error.XSDT_extra_xsd_element_in _
  | Error.XSDT_LIM_schema_location_required
  | Error.XSDT_multiple_name_for_type_definition
  | Error.XSDT_constructions_not_allowed _
  | Error.XSDT_constructions_expected _
  | Error.XSDT_occurrence_value_invalid
  | Error.XSDT_LIM_id_idref_unicity_reference_not_implemented
  | Error.XSDT_default_value_not_verified
  | Error.XSDT_fixed_value_not_verified
  | Error.XSV_input_file_not_provided
  | Error.XSV_multiple_input_files_not_allowed
  | Error.XSV_command_line_incorrect
  | Error.XSV_fail
  | Error.XSV_uri_not_matching
      (* 11: invalid but well-formed *)
    -> 11
        (* 12: not well-formed *)
  | Error.UTF8_invalid
  | Error.UTF8_overlong
  | Error.UTF8_first_byte_invalid
  | Error.UTF8_range_split_invalid
  | Error.Unicode_code_point_out_of_range
  | Error.XMLL_expected _
  | Error.XMLL_token_not_finished
  | Error.XMLL_standalone_not_recognised
  | Error.XMLL_version_missing
  | Error.XMLL_version_not_supported
  | Error.XMLL_empty_input
  | Error.XMLL_not_well_formed _
  | Error.XMLL_char_reference_not_well_formed
  | Error.XMLP_opening_tag_missing
  | Error.XMLP_content_after_root_not_allowed
  | Error.XMLP_data_before_root_not_allowed
  | Error.XMLP_closing_tag_missing
  | Error.XMLP_tree_not_recognized
  | Error.XMLP_opening_closing_tags_not_matching
  | Error.XMLL_XMLP_attribute_definition_not_unique
  | Error.XMLP_attribute_required _
  | Error.XMLP_LIM_attribute_ignored _
  | Error.XMLP_LIM_attribute_not_implemented _
  | Error.XMLP_element_expected _
  | Error.XMLP_LIM_element_ignored _
  | Error.XMLP_LIM_element_not_implemented _
  | Error.XMLP_LIM_xml_id_unicity_not_implemented
  | Error.XMLP_xml_space_value_invalid
  | Error.XMLNS_namespace_uri_reserved
  | Error.XMLNS_namespace_prefix_reserved
  | Error.XMLNS_XMLP_xml_attribute_not_recognised
  | Error.XMLNS_namespace_prefix_not_declared
  | Error.XMLNS_prefixed_declaration_value_empty
  | Error.XMLNS_XSDT_uri_not_well_formed
  | Error.XMLL_pi_target_not_allowed
  | Error.XMLL_entity_reference_unknown
  | Error.XSDL_multiple_definition _
  | Error.XSDL_missing_definition _
  | Error.XSDL_missing_definition_s
  | Error.XSDL_recursive_definitions
  | Error.XSDL_non_deterministic_grammar
  | Error.XSDL_facet_not_applicable
  | Error.XSDL_counter_not_applicable
  | Error.XSDL_order_not_applicable
  | Error.XSDL_XSV_LIM_notation_not_implemented
  | Error.XSDL_XSV_LIM_attribute_group_wildcard_not_implemented
  | Error.XSDL_LIM_order_not_implemented
  | Error.XSDL_simple_type_derivation_invalid
  | Error.XSDL_LIM_facet_not_implemented
  | Error.XSDL_whitespace_values_not_combinable
  | Error.XSDL_extension_of_anytype_risky
  | Error.XSDL_complex_type_extension_invalid
  | Error.XSDL_complex_type_restriction_invalid
  | Error.XSDL_all_model_not_well_formed
  | Error.XSDL_XSV_LIM_more_than_one_wildcard_in_all_model
  | Error.XSDL_LIM_determinism_check_not_implemented
  | Error.GEN_file_not_referenced
  | Error.GEN_referenced_file_missing
  | Error.GEN_import_effective_uris_mismatch
  | Error.GEN_imports_mismatch
  | Error.GEN_primary_uri_import_not_allowed
  | Error.GEN_XSDT_primary_target_uri_missing
  | Error.GEN_debug_mode
      (* 12: not well-formed *)
    -> 12
        (* restrictions -> 13 *)
  | Error.XMLP_SEC_comment
  | Error.XMLP_SEC_pi
  | Error.XMLP_SEC_cdata
  | Error.XMLP_SEC_doctype
  | Error.XMLL_encoding_missing
  | Error.XMLL_encoding_not_supported
  | Error.XMLL_SEC_ascii_char_reference_not_allowed
  | Error.XSDT_SEC_appinfo_not_allowed
  | Error.XSDL_SEC_no_root_element_declared
  | Error.XSV_SEC_xsi_attribute_not_allowed
  | Error.XSV_SEC_xsi_schemaLocation_ignored
  | Error.XSV_SEC_xsi_attribute_ignored
      (* restrictions -> 13 *)
    -> default_error_exit_code
      (* limitations -> 13 *)
  | Error.LIM_nc_disallowed
      (* limitations -> 13 *)
    -> default_error_exit_code
      (* 13: error *)
  | Error.EXE _
  | Error.A_function _
  | Error.A_stringdata_comparison_output
  | Error.A_empty_parser_stack
  | Error.A_double_log_setting
  | Error.A_empty_members_list
  | Error.A_local_global_function_types
  | Error.ML_max_string_reached
  | Error.ML_max_int_reached
  | Error.SD_subtraction_sources
  | Error.SD_subtraction_limits
  | Error.SD_subtraction_positions
  | Error.SD_structure
  | Error.SD_building_regexp_failed
  | Error.SEC_length_limit_reached
  | Error.SEC_depth_limit_reached
  | Error.SEC_contents_limit_reached
  | Error.SEC_attributes_limit_reached
  | Error.F_wrong_input_file
  | Error.F_wrong_input_extension
  | Error.F_wrong_printing_file_extension
  | Error.F_wrong_output_file_extension
  | Error.F_wrong_output_basename
  | Error.F_GEN_primary_file_not_provided
  | Error.IO_open_out_file_fail _
  | Error.IO_open_in_file_fail _
  | Error.IO_input
  | Error.IO_lib_dir_missing
  | Error.IO_output_dir
  | Error.XSV_uri_table_corrupted _
  | Error.XSV_type_table_corrupted _
  | Error.XSV_element_table_corrupted _
  | Error.XSV_attribute_table_corrupted _
  | Error.XSV_attribute_group_table_corrupted _
  | Error.XSV_model_group_table_corrupted _
  | Error.XSV_debug_mode
      (* 13: error *)
    -> default_error_exit_code

exception E of Error.t

type mtype =
  | Error
  | Warning
  | Info
  | Debug
  | Todo

let log_formatter = ref Format.err_formatter

let log_formatter_setup = ref false


let set_log formatter =
  if !log_formatter_setup
  then
    raise (Error.E Error.A_double_log_setting)
  else
    begin
      log_formatter := formatter;
      log_formatter_setup := true;
    end


(** {2 Printing library for debug} *)
module Pr_debug = struct


  let string_of_version = function
    | Xml.Version_1_0 -> "1.0"
    | Xml.Version_1_1 -> "1.1"

  let string_of_encoding = function
    | None -> "-"
    | Some Xml.Encoding_UTF8 -> "UTF-8"

  let string_of_standalone = function
    | None -> "-"
    | Some true -> "yes"
    | Some false -> "no"

  let string_of_sd sd =
    Format.sprintf "%s \"%s\""
      (Stringdata.string_debug sd)
      (Stringdata.to_string sd)

  let string_of_prefix_option f = function
    | None -> "_"
    | Some s -> f s

  let string_of_attrs m_attrs =
    Xml.M_unexp.fold
      (fun {Xml.prefix =p; Xml.local_name = l} value s ->
        (string_of_prefix_option string_of_sd p) ^
        ":" ^
        (string_of_sd l) ^
        " " ^
        (string_of_sd value) ^
        s)
      m_attrs
      ""


  let string_of_lexeme = function
    | Xml.Tag ({Xml.prefix =_; Xml.local_name = l},_) ->
        Format.sprintf "TAG: %s"
          (string_of_sd l)
    | Xml.Gat {Xml.prefix =_; Xml.local_name = l} ->
        Format.sprintf "GAT: %s"
          (string_of_sd l)
    | Xml.Taggat ({Xml.prefix =_; Xml.local_name = l},_) ->
        Format.sprintf "TAGGAT: %s"
          (string_of_sd l)
    | Xml.Data (t,b) ->
        Format.sprintf "DATA %s: %s"
          (if b then "t" else "f")
          (string_of_sd t)
    | Xml.Doctype t ->
        Format.sprintf
          "DOCTYPE: %s"
          (string_of_sd t)
    | Xml.PI (t1,t2) ->
        Format.sprintf "PI: %s - %s"
          (string_of_sd t1)
          (string_of_sd t2)


  let string_of_declaration = function
    | { Xml.version = v;
        Xml.encoding = e;
        Xml.standalone = s} ->
          Format.sprintf "Xmldecl %s %s %s"
            (string_of_version v)
            (string_of_encoding e)
            (string_of_standalone s)


  let td_id = function
      Lxsd.Id_td i -> "td_id " ^ (string_of_int i)


  let ed_id = function
      Lxsd.Id_ed i -> "ed_id " ^ (string_of_int i)


  let mgd_id = function
      Lxsd.Id_mgd i -> "mgd_id " ^ (string_of_int i)


  let rec fp_list f (func, l, sep) =
    match l with
    | [] -> ()
    | [h] ->
        Format.fprintf f "%a"
          func h
    | h :: t ->
        Format.fprintf f "%a%s@,%a"
          func h
          sep
          fp_list (func, t, sep)

  let fp_range f = function
    | (bfun,Utf8.R(c1,c2,b)) ->
        Format.fprintf f "[%02X-%02X]%a"
          (Char.code c1)
          (Char.code c2)
          bfun b

  let fp_b1 f = function
    | Utf8.B1 ->
        Format.fprintf f ""

  let fp_b2 f = function
    | Utf8.B2 l ->
        Format.fprintf f "@[<v>%a@]"
          fp_list ((fun f r -> fp_range f (fp_b1,r)),l,"")

  let fp_b3 f = function
    | Utf8.B3 l ->
        Format.fprintf f "@[<v>%a@]"
          fp_list ((fun f r -> fp_range f (fp_b2,r)),l,"")

  let fp_b4 f = function
    | Utf8.B4 l ->
        Format.fprintf f "@[<v>%a@]"
          fp_list ((fun f r -> fp_range f (fp_b3,r)),l,"")


  let fp_utf8_range f = function
    | { Utf8.byte1 = l1;
        Utf8.byte2 = l2;
        Utf8.byte3 = l3;
        Utf8.byte4 = l4 } ->
          Format.fprintf f "@[<v>%s @[<v>%a@]@,%s @[<v>%a@]@,%s @[<v>%a@]@,%s @[<v>%a@]@]@."
            "1"
            fp_list ((fun f r -> fp_range f (fp_b1,r)),l1,"")
            "2"
            fp_list ((fun f r -> fp_range f (fp_b2,r)),l2,"")
            "3"
            fp_list ((fun f r -> fp_range f (fp_b3,r)),l3,"")
            "4"
            fp_list ((fun f r -> fp_range f (fp_b4,r)),l4,"")


  let string_of_range = function
    | Utf8.R(c1,c2,_) ->
        Format.sprintf "[\\%i-\\%i]"
          (Char.code c1)
          (Char.code c2)

  let string_of_ranges l =
    List.fold_left
      (fun s r -> s ^ (string_of_range r))
      ""
      l

  let string_of_utf8_range : Utf8.range -> string = function
    | { Utf8.byte1 = l1;
        Utf8.byte2 = l2;
        Utf8.byte3 = l3;
        Utf8.byte4 = l4 } ->
          "1 " ^ string_of_ranges l1 ^ "\n" ^
          "2 " ^ string_of_ranges l2 ^ "\n" ^
          "3 " ^ string_of_ranges l3 ^ "\n" ^
          "4 " ^ string_of_ranges l4 ^ "\n"


  let string_code_of_string s =
    let len = String.length s in
    let rlen = len * 4 in
    if rlen > Sys.max_string_length
    then raise (Error.E Error.ML_max_string_reached);
    let rec pr_char rs i =
      if i >= len
      then rs
      else
        let n =
          "\\" ^ (Format.sprintf "%03i" (Char.code s.[i]))
        in
        pr_char (rs ^ n) (i + 1)
    in
    pr_char "" 0


  let string_hex_of_string s =
    let len = String.length s in
    let rec calc_len i ri =
      if i >= len
      then ri
      else if Char.code s.[i] <= 0x7F
      then calc_len (i + 1) (ri + 1)
      else calc_len (i + 1) (ri + 2)
    in
    let rlen = calc_len 0 0 in
    if rlen > Sys.max_string_length
    then raise (Error.E Error.ML_max_string_reached);
    let rec pr_char rs i =
      if i >= len
      then rs
      else
        let n =
          if Char.code s.[i] <= 0x7F
          then
            String.make 0 s.[i]
          else
            Format.sprintf "%02X" (Char.code s.[i])
        in
        pr_char (rs ^ n) (i + 1)
    in
    pr_char "" 0

  let hex_string_of_string s =
    let len = String.length s in
    let rec calc_len i ri =
      if i >= len
      then ri
      else if s.[i] >= '\032' && s.[i] <= '\126'
      then calc_len (i + 1) (ri + 1)
      else calc_len (i + 1) (ri + 2)
    in
    let rlen = calc_len 0 0 in
    if rlen > Sys.max_string_length
    then raise (Error.E Error.ML_max_string_reached);
    let rec pr_char rs i =
      if i >= len
      then rs
      else
        let n =
          if s.[i] >= '\032' && s.[i] <= '\126'
          then
            String.make 0 s.[i]
          else
            Format.sprintf "%02X" (Char.code s.[i])
        in
        pr_char (rs ^ n) (i + 1)
    in
    pr_char "" 0

end



module Pr_error = struct

(** {2 Printing library for errors} *)


  let string_of_wfc = function
    | Error.WFC_uniqattspec ->
        "Unique Att Spec"
    | Error.WFC_GIMatch ->
        "Element Type Match"
    | Error.WFC_Legalchar ->
        "Legal Character"

  let string_of_nsc = function
    | Error.NSC_xmlReserved ->
        "Reserved Prefixes and Namespace Names"
    | Error.NSC_NSDeclared ->
        "Prefix Declared"
    | Error.NSC_NoPrefixUndecl ->
        "No Prefix Undeclaring"
    | Error.NSC_AttrsUnique ->
        "Attributes Unique"

  let primitive = function
    | Lxsd.PT_string -> "string"
    | Lxsd.PT_boolean -> "boolean"
    | Lxsd.PT_decimal -> "decimal"
    | Lxsd.PT_float -> "float"
    | Lxsd.PT_double -> "double"
    | Lxsd.PT_duration -> "duration"
    | Lxsd.PT_dateTime -> "dateTime"
    | Lxsd.PT_time -> "time"
    | Lxsd.PT_date -> "date"
    | Lxsd.PT_gYearMonth -> "gYearMonth"
    | Lxsd.PT_gYear -> "gYear"
    | Lxsd.PT_gMonthDay -> "gMonthDay"
    | Lxsd.PT_gDay -> "gDay"
    | Lxsd.PT_gMonth -> "gMonth"
    | Lxsd.PT_hexBinary -> "hexBinary"
    | Lxsd.PT_base64Binary -> "base64Binary"
    | Lxsd.PT_anyURI -> "anyURI"
    | Lxsd.PT_QName -> "QName"
    | Lxsd.PT_NOTATION -> "NOTATION"

  let non_primitive = function
    | Lxsd.NPT_normalizedString -> "normalizedString"
    | Lxsd.NPT_token -> "token"
    | Lxsd.NPT_language -> "language"
    | Lxsd.NPT_IDREFS -> "IDREFS"
    | Lxsd.NPT_ENTITIES -> "ENTITIES"
    | Lxsd.NPT_NMTOKEN -> "NMTOKEN"
    | Lxsd.NPT_NMTOKENS -> "NMTOKENS"
    | Lxsd.NPT_Name -> "Name"
    | Lxsd.NPT_NCName -> "NCName"
    | Lxsd.NPT_ID -> "ID"
    | Lxsd.NPT_IDREF -> "IDREF"
    | Lxsd.NPT_ENTITY -> "ENTITY"
    | Lxsd.NPT_integer -> "integer"
    | Lxsd.NPT_nonPositiveInteger -> "nonPositiveInteger"
    | Lxsd.NPT_negativeInteger -> "negativeInteger"
    | Lxsd.NPT_long -> "long"
    | Lxsd.NPT_int -> "int"
    | Lxsd.NPT_short -> "short"
    | Lxsd.NPT_byte -> "byte"
    | Lxsd.NPT_nonNegativeInteger -> "nonNegativeInteger"
    | Lxsd.NPT_unsignedLong -> "unsignedLong"
    | Lxsd.NPT_unsignedInt -> "unsignedInt"
    | Lxsd.NPT_unsignedShort -> "unsignedShort"
    | Lxsd.NPT_unsignedByte -> "unsignedByte"
    | Lxsd.NPT_positiveInteger -> "positiveInteger"
    | Lxsd.NPT_yearMonthDuration -> "yearMonthDuration"
    | Lxsd.NPT_dayTimeDuration -> "dayTimeDuration"
    | Lxsd.NPT_dateTimeStamp -> "dateTimeStamp"



  let primitive_opt = function
    | None -> ""
    | Some p -> " '" ^ primitive p ^ "'"

  let non_primitive_opt = function
    | None -> ""
    | Some p -> " '" ^ non_primitive p ^ "'"

  let string_of_cvc = function
    | Lxsd.CVC_accept ->
        "Element Sequence Accepted (Particle)"
    | Lxsd.CVC_accept_1 ->
        "Element Sequence Accepted (Particle) /1"
    | Lxsd.CVC_accept_2 ->
        "Element Sequence Accepted (Particle) /2"
    | Lxsd.CVC_accept_3 ->
        "Element Sequence Accepted (Particle) /3"
    | Lxsd.CVC_assess_attr ->
        "Schema-Validity Assessment (Attribute)"
    | Lxsd.CVC_assess_elt ->
        "Schema-Validity Assessment (Element)"
    | Lxsd.CVC_assess_elt_1 ->
        "Schema-Validity Assessment (Element) /1"
    | Lxsd.CVC_assess_elt_2 ->
        "Schema-Validity Assessment (Element) /2"
    | Lxsd.CVC_assess_elt_3 ->
        "Schema-Validity Assessment (Element) /3"
    | Lxsd.CVC_attribute ->
        "Attribute Locally Valid"
    | Lxsd.CVC_au ->
        "Attribute Locally Valid (Use)"
    | Lxsd.CVC_complex_content ->
        "complex content"
    | Lxsd.CVC_complex_content_1 ->
        "complex content /1"
    | Lxsd.CVC_complex_type ->
        "complex type"
    | Lxsd.CVC_complex_type_1_1 ->
        "complex type /1.1"
    | Lxsd.CVC_complex_type_1_2 ->
        "complex type /1.2"
    | Lxsd.CVC_complex_type_1_3 ->
        "complex type /1.3"
    | Lxsd.CVC_complex_type_1_4 ->
        "complex type /1.4"
    | Lxsd.CVC_complex_type_2 ->
        "complex type /2"
    | Lxsd.CVC_complex_type_3 ->
        "complex type /3"
    | Lxsd.CVC_complex_type_4 ->
        "complex type /4"
    | Lxsd.CVC_datatype_valid ->
        "datatype valid"
    | Lxsd.CVC_datatype_valid_1 ->
        "datatype valid /1"
    | Lxsd.CVC_datatype_valid_2_1 ->
        "datatype valid /2.1"
    | Lxsd.CVC_datatype_valid_2_2 ->
        "datatype valid /2.2"
    | Lxsd.CVC_datatype_valid_2_3 ->
        "datatype valid /2.3"
    | Lxsd.CVC_elt ->
        "Element Locally Valid (Element)"
    | Lxsd.CVC_elt_5_2_1 ->
        "Element Locally Valid (Element) /5.2.1"
    | Lxsd.CVC_elt_5_2_2 ->
        "Element Locally Valid (Element) /5.2.2"
    | Lxsd.CVC_enumeration_valid ->
        "enumeration valid"
    | Lxsd.CVC_facet_valid ->
        "Facet Valid"
    | Lxsd.CVC_length_valid ->
        "length valid"
    | Lxsd.CVC_maxLength_valid ->
        "maxLength valid"
    | Lxsd.CVC_minLength_valid ->
        "minLength valid"
    | Lxsd.CVC_minInclusive_valid ->
        "minInclusive valid"
    | Lxsd.CVC_maxInclusive_valid ->
        "maxInclusive valid"
    | Lxsd.CVC_minExclusive_valid ->
        "minExclusive valid"
    | Lxsd.CVC_maxExclusive_valid ->
        "maxExclusive valid"
    | Lxsd.CVC_model_group ->
        "model group"
    | Lxsd.CVC_model_group_all ->
        "model group /all"
    | Lxsd.CVC_model_group_choice ->
        "model group /choice"
    | Lxsd.CVC_model_group_sequence ->
        "model group /sequence"
    | Lxsd.CVC_particle ->
        "Element Sequence Valid"
    | Lxsd.CVC_pattern_valid ->
        "pattern valid"
    | Lxsd.CVC_simple_type_p opt_p ->
        "string valid" ^ (primitive_opt opt_p)
    | Lxsd.CVC_simple_type_np opt_p ->
        "string valid" ^ (non_primitive_opt opt_p)
    | Lxsd.CVC_type ->
        "Element Locally Valid (Type)"
    | Lxsd.CVC_type_3_1 ->
        "Element Locally Valid (Type) /3.1"
    | Lxsd.CVC_type_3_1_1 ->
        "Element Locally Valid (Type) /3.1.1"
    | Lxsd.CVC_type_3_1_2 ->
        "Element Locally Valid (Type) /3.1.2"
    | Lxsd.CVC_type_3_2 ->
        "Element Locally Valid (Type) /3.2"
    | Lxsd.CVC_wildcard ->
        "Item Valid (Wildcard)"
    | Lxsd.CVC_wildcard_1 ->
        "Item Valid (Wildcard) /1"
    | Lxsd.CVC_wildcard_name ->
        "Wildcard allows Expanded Name"
    | Lxsd.CVC_wildcard_name_1 ->
        "Wildcard allows Expanded Name /1"
    | Lxsd.CVC_wildcard_namespace ->
        "Wildcard allows Namespace Name"
    | Lxsd.CVC_wildcard_namespace_1 ->
        "Wildcard allows Namespace Name /1"
    | Lxsd.CVC_wildcard_namespace_2 ->
        "Wildcard allows Namespace Name /2"
    | Lxsd.CVC_wildcard_namespace_3 ->
        "Wildcard allows Namespace Name /3"
    | Lxsd.Skip_wildcard_valid ->
        "skip wildcard valid"
    | Lxsd.Strict_wildcard_valid ->
        "strict wildcard valid"
    | Lxsd.Lax_wildcard_valid ->
        "lax wildcard valid"
    | Lxsd.AnyType_valid ->
        "anyType valid"
    | Lxsd.Schema_root_valid ->
        "schema root valid"

  let string_of_cos = function
    | Lxsd.COS_applicable_facets ->
        "Applicable Facets"
    | Lxsd.COS_st_restricts_2_1 ->
        "Derivation Valid (Restriction, Simple) /2.1"

  let string_of_src = function
    | Lxsd.SRC_ct_5 ->
        "Complex Type Definition Representation OK /5"
    | Lxsd.SRC_import_1_1 ->
        "Import Constraints and Semantics /1.1"
    | Lxsd.SRC_import_3_1 ->
        "Import Constraints and Semantics /3.1"

  let string_of_scc = function
    | Lxsd.SCC_whiteSpace_valid_restriction ->
        "whiteSpace valid restriction"
    | Lxsd.SCC_length_valid_restriction ->
        "length valid restriction"
    | Lxsd.SCC_maxminExInclusive_valid_restriction ->
        "max/minEx/Inclusive valid restriction"
    | Lxsd.SCC_ct_extends ->
        "Derivation Valid (Extension)"
    | Lxsd.SCC_derivation_ok_restriction ->
        "Derivation Valid (Restriction, Complex)"

  (** Return the string corresponding to an XML's Well-formedness constraint *)
  let xml_wfc wfc =
    " [WFC " ^ (string_of_wfc wfc) ^ "]"


  (** Return the string corresponding to a namespace constraint *)
  let ns_c nsc =
    " [NSC " ^ (string_of_nsc nsc) ^ "]"

  (** Return the string corresponding to a validation rule *)
  let cvc r =
    " [CVC " ^ (string_of_cvc r) ^ "]"

  (** Return the string corresponding to a Schema component constraint *)
  let cos r =
    " [COS " ^ (string_of_cos r) ^ "]"

  (** Return the string corresponding to a Schema representation constraint *)
  let src r =
    " [SRC " ^ (string_of_src r) ^ "]"

  (** Return the string corresponding to a Schema Component Constraint *)
  let scc r =
    " [SCC " ^ (string_of_scc r) ^ "]"

  let sec () =
    String.copy " (security constraint)"

  let lim () =
    String.copy " (limitation)"

  let string_of_xsdl_def = function
    | Error.XSDLDEF_type ->
        "type"
    | Error.XSDLDEF_attribute ->
        "attribute"
    | Error.XSDLDEF_element ->
        "element"
    | Error.XSDLDEF_attribute_group ->
        "attribute group"
    | Error.XSDLDEF_model_group ->
        "model group"

  let string_of_error = function
    | Error.LIM_nc_disallowed ->
        "XSD construction not implemented" ^ lim ()
    | Error.SEC_attributes_limit_reached ->
        "maximum attributes limit reached" ^ sec ()
    | Error.SEC_contents_limit_reached ->
        "maximum contents limit reached" ^ sec ()
    | Error.SEC_depth_limit_reached ->
        "maximum depth limit reached" ^ sec ()
    | Error.SEC_length_limit_reached ->
        "maximum length limit reached" ^ sec ()
    | Error.F_wrong_output_file_extension ->
        "wrong output file extension"
    | Error.F_wrong_output_basename ->
        "wrong output name"
    | Error.F_wrong_printing_file_extension ->
        "wrong printing file extension"
    | Error.F_wrong_input_extension ->
        "wrong input extension"
    | Error.F_wrong_input_file ->
        "wrong input file"
    | Error.F_GEN_primary_file_not_provided ->
        "XSD primary file not provided"
    | Error.IO_input ->
        "fail to read input"
    | Error.IO_open_in_file_fail s ->
        "fail to read file " ^ s
    | Error.IO_open_out_file_fail s ->
        "fail to write file " ^ s
    | Error.IO_lib_dir_missing ->
        "validator library directory missing"
    | Error.IO_output_dir ->
        "fail to prepare the output directory"
    | Error.SD_subtraction_positions ->
        "subtraction positions error"
    | Error.SD_subtraction_limits ->
        "subtraction limits error"
    | Error.SD_subtraction_sources ->
        "subtraction sources error"
    | Error.SD_structure ->
        "string data error"
    | Error.SD_building_regexp_failed ->
        "building regular expression failed"
    | Error.UTF8_invalid ->
        "invalid UTF-8 encoding"
    | Error.UTF8_overlong ->
        "overlong UTF-8 encoding"
    | Error.UTF8_first_byte_invalid ->
        "first byte for 1-, 2-, 3-, 4--bytes UTF-8 encoding invalid"
    | Error.UTF8_range_split_invalid ->
        "UTF-8 range split invalid"
    | Error.Unicode_code_point_out_of_range ->
        "Unicode code point out of range"
    | Error.XMLL_encoding_missing ->
        "XML encoding is missing, using UTF-8"
    | Error.XMLL_encoding_not_supported ->
        "encoding not supported"
    | Error.XMLL_token_not_finished ->
        "XML token not finished"
    | Error.XMLL_standalone_not_recognised ->
        "'standalone' value not recognised"
    | Error.XMLL_version_missing ->
        "version missing in XML declaration"
    | Error.XMLL_version_not_supported ->
        "XML version not supported"
    | Error.XMLL_pi_target_not_allowed ->
        "processing instructions target not allowed"
    | Error.XMLL_empty_input ->
        "empty input"
    | Error.XMLL_expected s ->
        "XML syntax error, " ^ s ^ " expected"
    | Error.XMLL_not_well_formed s ->
        "XML syntax error, " ^ s ^ " not well formed"
    | Error.XMLL_entity_reference_unknown ->
        "entity reference unknown"
    | Error.XMLL_char_reference_not_well_formed ->
        "char reference not well formed" ^ (xml_wfc Error.WFC_Legalchar)
    | Error.XMLL_SEC_ascii_char_reference_not_allowed ->
        "ASCII char reference not allowed" ^ sec ()
    | Error.XMLP_opening_tag_missing ->
        "opening tag missing"
    | Error.XMLP_SEC_doctype ->
        "DocType in file" ^ sec ()
    | Error.XMLP_SEC_cdata ->
        "CDATA in file" ^ sec ()
    | Error.XMLP_SEC_pi ->
        "PI in file" ^ sec ()
    | Error.XMLP_SEC_comment ->
        "comment in file" ^ sec ()
    | Error.XMLP_content_after_root_not_allowed ->
        "Invalid data/elements after root element"
    | Error.XMLP_data_before_root_not_allowed ->
        "Unauthorised data before the root node"
    | Error.XMLP_closing_tag_missing ->
        "Closing tag missing"
    | Error.XMLP_tree_not_recognized ->
        "Unrecognized tree in XML parser"
    | Error.XMLL_XMLP_attribute_definition_not_unique ->
        "Attribute not unique" ^ (xml_wfc Error.WFC_uniqattspec) ^ (ns_c Error.NSC_AttrsUnique)
    | Error.XMLP_opening_closing_tags_not_matching ->
        "opening closing tags not matching" ^ (xml_wfc Error.WFC_GIMatch)
    | Error.XMLP_attribute_required s ->
        "attribute '" ^ s ^ "' required"
    | Error.XMLP_LIM_attribute_ignored s ->
        "attribute '" ^ s ^ "' ignored" ^ lim ()
    | Error.XMLP_LIM_attribute_not_implemented s ->
        "attribute '" ^ s ^ "' not implemented" ^ lim ()
    | Error.XMLP_element_expected s ->
        "element '" ^ s ^ "' expected"
    | Error.XMLP_LIM_element_ignored s ->
        "element '" ^ s ^ "' ignored" ^ lim ()
    | Error.XMLP_LIM_element_not_implemented s ->
        "element '" ^ s ^ "' not implemented" ^ lim ()
    | Error.XMLP_LIM_xml_id_unicity_not_implemented ->
        "xml:id unicity not implemented" ^ lim ()
    | Error.XMLP_xml_space_value_invalid ->
        "xml:space value invalid"
    | Error.XMLNS_namespace_uri_reserved ->
        "namespace URI reserved" ^ (ns_c Error.NSC_xmlReserved)
    | Error.XMLNS_namespace_prefix_reserved ->
        "namespace prefix reserved" ^ (ns_c Error.NSC_xmlReserved)
    | Error.XMLNS_prefixed_declaration_value_empty ->
        "empty value for a prefixed namespace declaration not allowed" ^ (ns_c Error.NSC_NoPrefixUndecl)
    | Error.XMLNS_XMLP_xml_attribute_not_recognised ->
        "XML attribute not recognised"
    | Error.XMLNS_namespace_prefix_not_declared ->
        "namespace prefix not declared" ^ (ns_c Error.NSC_NSDeclared)
    | Error.XMLNS_XSDT_uri_not_well_formed ->
        "URI not well formed"
    | Error.XSDT_SEC_appinfo_not_allowed ->
        "element 'appinfo' not allowed" ^ sec ()
    | Error.XSDT_skip_process_contents_risky ->
        "XSD skip process contents is risky"
    | Error.XSDT_version_missing ->
        "XSD version is missing, using XSD version 1.1"
    | Error.XSDT_version_1_0_not_supported ->
        "XSD version 1.0 not supported, using XSD version 1.1 instead"
    | Error.XSDT_version_not_supported ->
        "XSD version not supported, using XSD version 1.1 instead"
    | Error.XSDT_qname_not_well_formed ->
        "QName not well formed"
    | Error.XSDT_qname_namespace_missing ->
        "QName's namespace missing"
    | Error.XSDT_mixed_values_contradictory ->
        "mixed values contradictory" ^ (src Lxsd.SRC_ct_5)
    | Error.XSDT_min_max_inconsistent ->
        "min max values inconsistent"
    | Error.XSDT_constructions_mutually_exclusive s ->
        s ^ " mutually exclusive"
    | Error.XSDT_extra_xsd_attribute_not_recognised ->
        "extra XSD attribute not recognised"
    | Error.XSDT_extra_xsd_element_in s ->
        "extra XSD element in " ^ s ^ " not recognised"
    | Error.XSDT_LIM_schema_location_required ->
        "schemaLocation required" ^ lim ()
    | Error.XSDT_multiple_name_for_type_definition ->
        "multiple name for a type definition"
    | Error.XSDT_constructions_not_allowed s ->
        s ^ " not allowed"
    | Error.XSDT_constructions_expected s ->
        s ^ " expected"
    | Error.XSDT_occurrence_value_invalid ->
        "occurrence value invalid"
    | Error.XSDT_LIM_id_idref_unicity_reference_not_implemented ->
        "ID unicity and IDREF, IDREFS references existence not implemented" ^ lim ()
    | Error.XSDT_default_value_not_verified ->
        "default value not verified"
    | Error.XSDT_fixed_value_not_verified ->
        "fixed value not verified"
    | Error.RE_syntax s ->
        "regular expression syntax error '" ^ s ^ "'"
    | Error.RE_block_unknown s ->
        "unknown Unicode block '" ^ s ^ "'"
    | Error.RE_UTF8_invalid ->
        "invalid UTF-8 encoding in regular expression"
    | Error.RE_LIM_charprop_not_implemented ->
        "charProp not implemented" ^ lim ()
    | Error.XSDL_multiple_definition xsdl_def ->
        "multiple " ^ string_of_xsdl_def xsdl_def ^ " definition"
    | Error.XSDL_missing_definition xsdl_def ->
        "missing " ^ string_of_xsdl_def xsdl_def ^ " definition"
    | Error.XSDL_missing_definition_s ->
        "missing definition(s) in XSD grammar"
    | Error.XSDL_recursive_definitions ->
        "recursive definitions in XSD grammar"
    | Error.XSDL_non_deterministic_grammar ->
        "non deterministic XSD grammar"
    | Error.XSDL_facet_not_applicable ->
        "facet not applicable" ^ (cos Lxsd.COS_applicable_facets)
    | Error.XSDL_counter_not_applicable ->
        "length counter not applicable" ^ (scc Lxsd.SCC_length_valid_restriction)
    | Error.XSDL_order_not_applicable ->
        "order not applicable" ^ (scc Lxsd.SCC_maxminExInclusive_valid_restriction)
    | Error.XSDL_XSV_LIM_notation_not_implemented ->
        "notation not implemented" ^ lim ()
    | Error.XSDL_XSV_LIM_attribute_group_wildcard_not_implemented ->
        "attribute group wildcard not implemented" ^ lim ()
    | Error.XSDL_LIM_order_not_implemented ->
        "order not implemented" ^ lim ()
    | Error.XSDL_simple_type_derivation_invalid ->
        "simple type derivation invalid" ^ (cos Lxsd.COS_st_restricts_2_1)
    | Error.XSDL_LIM_facet_not_implemented ->
        "facet not implemented" ^ lim ()
    | Error.XSDL_whitespace_values_not_combinable ->
        "whitespace values not combinable" ^ (scc Lxsd.SCC_whiteSpace_valid_restriction)
    | Error.XSDL_extension_of_anytype_risky ->
        "extending anyType is risky"
    | Error.XSDL_complex_type_extension_invalid ->
        "complex type extension" ^ (scc Lxsd.SCC_ct_extends)
    | Error.XSDL_complex_type_restriction_invalid ->
        "complex type restriction" ^ (scc Lxsd.SCC_derivation_ok_restriction)
    | Error.XSDL_SEC_no_root_element_declared ->
        "no root element declared" ^ sec ()
    | Error.XSDL_all_model_not_well_formed ->
        "all-model group not well formed"
    | Error.XSDL_XSV_LIM_more_than_one_wildcard_in_all_model ->
        "more than one wildcard in all-model" ^ lim ()
    | Error.XSDL_LIM_determinism_check_not_implemented ->
        "determinism check not implemented" ^ lim ()
    | Error.GEN_referenced_file_missing ->
        "referenced XSD's file missing"
    | Error.GEN_file_not_referenced ->
        "XSD file not referenced"
    | Error.GEN_import_effective_uris_mismatch ->
        "imported and effective target URI do not match" ^ (src Lxsd.SRC_import_3_1)
    | Error.GEN_imports_mismatch ->
        "importations do not match"
    | Error.GEN_primary_uri_import_not_allowed ->
        "importation of primary URI not allowed" ^ (src Lxsd.SRC_import_1_1)
    | Error.GEN_XSDT_primary_target_uri_missing ->
        "primary target URI missing"
    | Error.GEN_debug_mode ->
        "generating debug mode validator"
    | Error.XSV_input_file_not_provided ->
        "input file not provided"
    | Error.XSV_multiple_input_files_not_allowed ->
        "multiple input files not allowed"
    | Error.XSV_command_line_incorrect ->
        "command line incorrect"
    | Error.XSV_fail ->
        "invalid document"
    | Error.XSV_uri_not_matching ->
        "invalid document (URI not matching)"
    | Error.XSV_uri_table_corrupted i ->
        "validation table corrupted (URI " ^
        string_of_int i ^ " missing)"
    | Error.XSV_type_table_corrupted i ->
        "validation table corrupted (type " ^
        string_of_int i ^ " missing)"
    | Error.XSV_element_table_corrupted i ->
        "validation table corrupted (element " ^
        string_of_int i ^ " missing)"
    | Error.XSV_attribute_table_corrupted i ->
        "validation table corrupted (attribute " ^
        string_of_int i ^ " missing)"
    | Error.XSV_attribute_group_table_corrupted i ->
        "validation table corrupted (attribute group " ^
        string_of_int i ^ " missing)"
    | Error.XSV_model_group_table_corrupted i ->
        "validation table corrupted (model group " ^
        string_of_int i ^ " missing)"
    | Error.XSV_SEC_xsi_attribute_not_allowed ->
        "XML Schema Instance attribute not allowed" ^ sec ()
    | Error.XSV_SEC_xsi_schemaLocation_ignored ->
        "XML Schema Instance's schemaLocation attribute ignored" ^ sec ()
    | Error.XSV_SEC_xsi_attribute_ignored ->
        "XML Schema Instance attribute ignored" ^ sec ()
    | Error.XSV_debug_mode ->
        "validator in debug mode"
    | Error.A_empty_parser_stack
    | Error.A_stringdata_comparison_output
    | Error.A_double_log_setting
    | Error.A_empty_members_list
    | Error.A_local_global_function_types
      ->
        "unexpected error"
    | Error.A_function qfun ->
        "unexprected error (" ^ qfun ^ ")"
    | Error.ML_max_string_reached
    | Error.ML_max_int_reached ->
        "system limit reached"
    | Error.EXE (s,(Invalid_argument s')) ->
        "execution error, " ^ s ^ ", invalid argument " ^ s'
    | Error.EXE (s,_e) ->
        "execution error, " ^ s ^ ", unexpected exception"

  let fprintf_error f e =
    Format.fprintf f "%s"
      (string_of_error e)

  let string_of_max= function
    | None -> "unbounded"
    | Some i -> string_of_int i

  let string_of_status = function
    | true -> "rejected"
    | false -> "ignored"

  let string_of_xsi_status = function
  | Xml.XSI_reject_all ->
      "rejected"
  | Xml.XSI_ignore_schemaLocation_only ->
      "rejected (except schemaLocation)"
  | Xml.XSI_ignore_all ->
      "ignored"

  let string_of_info = function
    | INFO_file_XML_valid in_name ->
        "file " ^ File_in.to_string in_name ^ " XML valid"
    | INFO_file_XML_invalid ->
        "file XML invalid"
    | INFO_file_XML_notwf ->
        "file XML not well-formed"
    | INFO_validating_element sd ->
        "validating element '" ^ Stringdata.to_string sd ^ "'"
    | INFO_testing_element sd ->
        "testing element '" ^ Stringdata.to_string sd ^ "'"
    | INFO_valid_element sd ->
        "valid element '" ^ Stringdata.to_string sd ^ "'"
    | INFO_invalid_element sd ->
        "invalid element '" ^ Stringdata.to_string sd ^ "'"
    | INFO_fetching_td i ->
        "fetching type definition " ^ string_of_int i
    | INFO_fetching_ed i ->
        "fetching element definition " ^ string_of_int i
    | INFO_fetching_ad i ->
        "fetching attribute definition " ^ string_of_int i
    | INFO_fetching_agd i ->
        "fetching attribute group definition " ^ string_of_int i
    | INFO_fetching_mgd i ->
        "fetching model group definition " ^ string_of_int i
    | INFO_document_valid ->
        "document valid"
    | INFO_pattern re ->
        "regular expression in pattern " ^
        (Stringdata.to_code_escaped_string re)
    | INFO_XSD_primary_file in_name ->
        "primary XSD file " ^ File_in.to_string in_name
    | INFO_XSD_secondary_file in_name ->
        "secondary XSD file " ^ File_in.to_string in_name
    | INFO_validator_configuration
        { Xml.max_attributes = m_a;
          Xml.max_contents = m_c;
          Xml.max_depth = m_d;
          Xml.max_length = m_l;
          Xml.error_status =
          { Xml.doctype = dt;
            Xml.cdata = cd;
            Xml.pi = pi;
            Xml.comment = ct;
            Xml.ascii_ref = ar;
            Xml.xsi = xsi_status } } ->
              "configuration (" ^
              "max-attributes " ^ string_of_max m_a ^ ", " ^
              "max-contents " ^ string_of_max m_c ^ ", " ^
              "max-depth " ^ string_of_max m_d ^ ", " ^
              "max-length " ^ string_of_max m_l ^ ", " ^
              "DocType" ^ string_of_status dt ^ ", " ^
              "CData " ^ string_of_status cd ^ ", " ^
              "PI " ^ string_of_status pi ^ ", " ^
              "comment " ^ string_of_status ct ^ ", " ^
              "ASCII references " ^ string_of_status ar ^ ", " ^
              "XSI " ^ string_of_xsi_status xsi_status ^ ")"


  let fprintf_info f i =
    Format.fprintf f "%s"
      (string_of_info i)

end




let pr_fun mtype loc descr_fun descr =
  Format.fprintf !log_formatter "[%s] %a%s@."
    (match mtype with
    | Error -> "error"
    | Warning -> "warning"
    | Info -> "info"
    | Debug -> "DEBUG"
    | Todo -> "TODO")
    descr_fun descr
    (if loc="" then loc else " : " ^ loc)


let error_fun loc err =
  pr_fun Error loc Pr_error.fprintf_error err;
  raise (E err)

let error err =
  error_fun "" err

let error_at s err =
  error_fun
    (Stringdata.string_loc s)
    err

let warning_fun loc err =
  pr_fun Warning loc Pr_error.fprintf_error err

let warning err =
  warning_fun "" err

let warning_at s err =
  warning_fun
    (Stringdata.string_loc s)
    err

let info_fun loc message =
  pr_fun Info loc Pr_error.fprintf_info message

let info message =
  info_fun "" message

let info_at s message =
  info_fun (Stringdata.string_loc s) message

let warning_cvc cvc =
  pr_fun Warning ""
    (fun f -> Format.fprintf f "%s")
    ("failed validation rule" ^ Pr_error.cvc cvc)

let warning_cvc_at s cvc =
  pr_fun Warning (Stringdata.string_loc s)
    (fun f -> Format.fprintf f "%s")
    ("failed validation rule" ^ Pr_error.cvc cvc)

let info_cvc cvc =
  pr_fun Info ""
    (fun f -> Format.fprintf f "%s")
    ("validation rule" ^ Pr_error.cvc cvc)

let info_cvc_at s cvc =
  pr_fun Info (Stringdata.string_loc s)
    (fun f -> Format.fprintf f "%s")
    ("validation rule" ^ Pr_error.cvc cvc)


