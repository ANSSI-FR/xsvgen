(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Samuel Colin (SafeRiver)                                  *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_validator.ml 1704 2012-06-04 16:49:33Z maarek $ *)

(** {1 XML validator} *)

let filename = ref ""

let add_filename str=
  filename := str

let usage_message = "Usage: see -help\n"

let rec args =
  [
    ("--help",
     Arg.Unit(fun () ->
       Arg.usage args usage_message; raise Exit),
     "\t\t\t display this help")
    ;
    ("-help",
     Arg.Unit(fun () ->
       Arg.usage args usage_message; raise Exit),
     "\t\t\t See --help")
    ;
  ]




let xml_validator_configuration =
  { Xml.max_attributes = None;
    Xml.max_contents = None;
    Xml.max_depth = None;
    Xml.max_length = None;
    Xml.error_status =
    { Xml.doctype = false;
      Xml.cdata = false;
      Xml.pi = false;
      Xml.comment = false;
      Xml.ascii_ref = false;
      Xml.xsi = Xml.XSI_ignore_all } }


let xml_validate filename =
  let in_name =
    File_in.of_string filename [File_in.Xml; File_in.Xsd] in
  let ic =
    File_in.to_in_channel in_name [File_in.Xml; File_in.Xsd] in
  let xml_sd =
    Stringdata.of_in_channel ic  in_name in
  let stream =
    Xml_lexer.lex_xml
      xml_validator_configuration.Xml.error_status
      xml_sd in
  let _tree = Xml_parser.parse xml_validator_configuration stream in
  File_in.close_opened_files ()



let _ =
  try
    begin
      try
        begin
          Arg.parse args add_filename usage_message;
          if !filename = ""
          then Message.error Error.XSV_command_line_incorrect
          else xml_validate !filename
        end
      with
      | Error.E e ->
          Message.error e
    end
  with
  | Sys_error(msg) -> (prerr_endline msg; exit 3)
  | End_of_file -> (prerr_endline "End_of_file"; exit 3)
  | Message.E e ->
      begin
        match e with
          (* 2: not well-formed *)
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
        | Error.XMLNS_namespace_uri_reserved
        | Error.XMLNS_namespace_prefix_reserved
        | Error.XMLNS_XMLP_xml_attribute_not_recognised
        | Error.XMLNS_namespace_prefix_not_declared
        | Error.XMLNS_prefixed_declaration_value_empty
        | Error.XMLNS_XSDT_uri_not_well_formed
        | Error.XMLL_pi_target_not_allowed
        | Error.XMLL_entity_reference_unknown
          (* 2: not well-formed *)
          ->
            Message.info Message.INFO_file_XML_notwf;
            exit 2
          (* 1: invalid but well formed *)
        | Error.XMLP_xml_space_value_invalid
          (* 1: invalid but well formed *)
          ->
            Message.info Message.INFO_file_XML_invalid;
            exit 1
          (* restrictions -> 3 *)
        | Error.XMLP_SEC_comment
        | Error.XMLP_SEC_pi
        | Error.XMLP_SEC_cdata
        | Error.XMLP_SEC_doctype
        | Error.XMLL_encoding_missing
        | Error.XMLL_encoding_not_supported
        | Error.XMLL_SEC_ascii_char_reference_not_allowed
          (* restrictions -> 3 *)
            -> exit 3
          (* limitations -> 3 *)
        | Error.LIM_nc_disallowed
          (* limitations -> 3 *)
          -> exit 3
          (* 1: invalid but well-formed *)
          (* out of scope *)
        | Error.RE_block_unknown _
        | Error.RE_syntax _
        | Error.RE_UTF8_invalid
        | Error.RE_LIM_charprop_not_implemented
        | Error.XSDT_version_not_supported
        | Error.XSDT_version_1_0_not_supported
        | Error.XSDT_version_missing
        | Error.XSDT_qname_namespace_missing
        | Error.XSDT_qname_not_well_formed
        | Error.XSDT_skip_process_contents_risky
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
        | Error.XSDL_multiple_definition _
        | Error.XSDL_missing_definition _
        | Error.XSDL_missing_definition_s
        | Error.XSDL_recursive_definitions
        | Error.XSDL_non_deterministic_grammar
        | Error.XSDL_facet_not_applicable
        | Error.XSDL_counter_not_applicable
        | Error.XSDL_order_not_applicable
        | Error.XSDL_LIM_order_not_implemented
        | Error.XSDL_XSV_LIM_notation_not_implemented
        | Error.XSDL_XSV_LIM_attribute_group_wildcard_not_implemented
        | Error.XSDL_simple_type_derivation_invalid
        | Error.XSDL_LIM_facet_not_implemented
        | Error.XSDL_whitespace_values_not_combinable
        | Error.XSDL_extension_of_anytype_risky
        | Error.XSDL_complex_type_extension_invalid
        | Error.XSDL_complex_type_restriction_invalid
        | Error.XSDL_all_model_not_well_formed
        | Error.XSDL_XSV_LIM_more_than_one_wildcard_in_all_model
        | Error.XSDL_LIM_determinism_check_not_implemented
        | Error.GEN_referenced_file_missing
        | Error.GEN_file_not_referenced
        | Error.GEN_imports_mismatch
        | Error.GEN_import_effective_uris_mismatch
        | Error.GEN_XSDT_primary_target_uri_missing
        | Error.GEN_primary_uri_import_not_allowed
        | Error.GEN_debug_mode
        | Error.XSV_input_file_not_provided
        | Error.XSV_multiple_input_files_not_allowed
        | Error.XSV_command_line_incorrect
        | Error.XSV_fail
        | Error.XSV_uri_not_matching
        | Error.XSV_uri_table_corrupted _
        | Error.XSV_type_table_corrupted _
        | Error.XSV_element_table_corrupted _
        | Error.XSV_attribute_table_corrupted _
        | Error.XSV_attribute_group_table_corrupted _
        | Error.XSV_model_group_table_corrupted _
        | Error.XSV_debug_mode
          (* out of scope *)
          (* 1: invalid but well-formed *)
          -> exit 3
          (* 3: error *)
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
        | Error.XSV_SEC_xsi_attribute_not_allowed
        | Error.XSV_SEC_xsi_schemaLocation_ignored
        | Error.XSV_SEC_xsi_attribute_ignored
        | Error.XSDT_SEC_appinfo_not_allowed
        | Error.XSDL_SEC_no_root_element_declared
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
          (* 3: error *)
            -> exit 3
      end
  | _ ->
      (* 3: error *)
      exit 3
