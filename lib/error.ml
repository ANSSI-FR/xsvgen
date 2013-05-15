(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: error.ml 1704 2012-06-04 16:49:33Z maarek $ *)

(** {1 Error types} *)

(** Error

    Error prefixes and conventions:
    - [*SEC_*]: Security constraints
    - [*LIM_*]: Limitations
    - [*UTF8_*]: UTF-8 errors
    - [*Unicode_*]: Unicode errors
    - [*SD_*]: string data errors
    - [*F_*]: file names handelling errors
    - [*IO_*]: input-output errors
    - [*XMLL_*]: XML lexer errors
    - [*XMLNS_*]: XML namespace errors
    - [*XMLP_*]: XML parser errors
    - [*XSDT_*]: XSD transducer errors
    - [*XSDL_*]: XSD lineariser errors
    - [*GEN_*]: Generator errors
    - [*RE_*]: XSD's regular expressions manager errors
    - [*XSV_*]: XML Schema validation (validation phase only)
    - [*NOTWFXSD_*]: validation error resulting of an XSD not well-formed
    - [A_*]: assertions
    - [ML_*]: OCaml limits
 *)
type t =
  | LIM_nc_disallowed
  | SEC_attributes_limit_reached
  | SEC_contents_limit_reached
  | SEC_depth_limit_reached
  | SEC_length_limit_reached
  | F_wrong_output_file_extension
  | F_wrong_printing_file_extension
  | F_wrong_input_extension
  | F_wrong_input_file
  | F_wrong_output_basename
  | F_GEN_primary_file_not_provided
  | IO_open_in_file_fail of string
  | IO_open_out_file_fail of string
  | IO_input
  | IO_output_dir
  | IO_lib_dir_missing
  | SD_subtraction_positions
  | SD_subtraction_limits
  | SD_subtraction_sources
  | SD_structure
  | SD_building_regexp_failed
  | UTF8_invalid
  | UTF8_overlong
  | UTF8_first_byte_invalid
  | UTF8_range_split_invalid
  | Unicode_code_point_out_of_range
  | XMLL_encoding_missing
  | XMLL_encoding_not_supported
  | XMLL_token_not_finished
  | XMLL_standalone_not_recognised
  | XMLL_version_missing
  | XMLL_version_not_supported
  | XMLL_pi_target_not_allowed
  | XMLL_empty_input
  | XMLL_expected of string
  | XMLL_not_well_formed of string
  | XMLL_entity_reference_unknown
  | XMLL_char_reference_not_well_formed
  | XMLL_SEC_ascii_char_reference_not_allowed
  | XMLP_opening_tag_missing
  | XMLP_SEC_doctype
  | XMLP_SEC_cdata
  | XMLP_SEC_pi
  | XMLP_SEC_comment
  | XMLP_content_after_root_not_allowed
  | XMLP_data_before_root_not_allowed
  | XMLP_closing_tag_missing
  | XMLP_tree_not_recognized
  | XMLL_XMLP_attribute_definition_not_unique
  | XMLP_opening_closing_tags_not_matching
  | XMLP_attribute_required of string
  | XMLP_LIM_attribute_ignored of string
  | XMLP_LIM_attribute_not_implemented of string
  | XMLP_element_expected of string
  | XMLP_LIM_element_ignored of string
  | XMLP_LIM_element_not_implemented of string
  | XMLP_LIM_xml_id_unicity_not_implemented
  | XMLP_xml_space_value_invalid
  | XMLNS_namespace_uri_reserved
  | XMLNS_namespace_prefix_reserved
  | XMLNS_XMLP_xml_attribute_not_recognised
  | XMLNS_namespace_prefix_not_declared
  | XMLNS_prefixed_declaration_value_empty
  | XMLNS_XSDT_uri_not_well_formed
  | XSDT_SEC_appinfo_not_allowed
  | XSDT_skip_process_contents_risky
  | XSDT_version_missing
  | XSDT_version_1_0_not_supported
  | XSDT_version_not_supported
  | XSDT_qname_not_well_formed
  | XSDT_qname_namespace_missing
  | XSDT_mixed_values_contradictory
  | XSDT_min_max_inconsistent
  | XSDT_constructions_mutually_exclusive of string
  | XSDT_extra_xsd_attribute_not_recognised
  | XSDT_extra_xsd_element_in of string
  | XSDT_LIM_schema_location_required
  | XSDT_multiple_name_for_type_definition
  | XSDT_constructions_not_allowed of string
  | XSDT_constructions_expected of string
  | XSDT_occurrence_value_invalid
  | XSDT_LIM_id_idref_unicity_reference_not_implemented
  | XSDT_default_value_not_verified
  | XSDT_fixed_value_not_verified
  | XSDL_multiple_definition of xsdl_definition
  | XSDL_missing_definition of xsdl_definition
  | XSDL_missing_definition_s
  | XSDL_recursive_definitions
  | XSDL_non_deterministic_grammar
  | XSDL_facet_not_applicable (** {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/#defn-coss}REF} *)
  | XSDL_counter_not_applicable
  | XSDL_order_not_applicable
  | XSDL_XSV_LIM_notation_not_implemented
  | XSDL_XSV_LIM_attribute_group_wildcard_not_implemented
  | XSDL_LIM_order_not_implemented
  | XSDL_simple_type_derivation_invalid
  | XSDL_LIM_facet_not_implemented
  | XSDL_whitespace_values_not_combinable
  | XSDL_extension_of_anytype_risky
  | XSDL_complex_type_extension_invalid
  | XSDL_complex_type_restriction_invalid
  | XSDL_SEC_no_root_element_declared
  | XSDL_all_model_not_well_formed
  | XSDL_XSV_LIM_more_than_one_wildcard_in_all_model
  | XSDL_LIM_determinism_check_not_implemented
  | GEN_referenced_file_missing
  | GEN_file_not_referenced
  | GEN_import_effective_uris_mismatch
  | GEN_imports_mismatch
  | GEN_primary_uri_import_not_allowed
  | GEN_XSDT_primary_target_uri_missing
  | GEN_debug_mode
  | RE_syntax of string
  | RE_block_unknown of string
  | RE_UTF8_invalid
  | RE_LIM_charprop_not_implemented
  | XSV_input_file_not_provided
  | XSV_multiple_input_files_not_allowed
  | XSV_command_line_incorrect
  | XSV_fail
  | XSV_uri_not_matching
  | XSV_uri_table_corrupted of int
  | XSV_type_table_corrupted of int
  | XSV_element_table_corrupted of int
  | XSV_attribute_table_corrupted of int
  | XSV_attribute_group_table_corrupted of int
  | XSV_model_group_table_corrupted of int
  | XSV_SEC_xsi_attribute_not_allowed
  | XSV_SEC_xsi_schemaLocation_ignored
  | XSV_SEC_xsi_attribute_ignored
  | XSV_debug_mode
  | A_empty_parser_stack
  | A_stringdata_comparison_output
  | A_double_log_setting
  | A_empty_members_list
  | A_local_global_function_types
  | A_function of string
  | ML_max_string_reached
  | ML_max_int_reached
  | EXE of string * exn

and xsdl_definition =
  | XSDLDEF_type
  | XSDLDEF_attribute
  | XSDLDEF_element
  | XSDLDEF_attribute_group
  | XSDLDEF_model_group


(** Exception for modules on which {!Message} depends. *)
exception E of t


(** XML's {b Well-formedness constraints} *)
type xml_wfc =
  | WFC_uniqattspec (** {b Unique Att Spec}
                        {{:http://www.w3.org/TR/xml11/#uniqattspec}REF} *)
  | WFC_GIMatch (** {b Element Type Match}
                    {{:http://www.w3.org/TR/xml11/#GIMatch}REF} *)
  | WFC_Legalchar (** {bLegal Character}
                      {{:http://www.w3.org/TR/xml11/#wf-Legalchar}REF} *)

(** XMLNS's {b Namespace constraints} *)
type ns_c =
  | NSC_xmlReserved
      (** {b Reserved Prefixes and Namespace Names}
          {{:http://www.w3.org/TR/REC-xml-names/#xmlReserved}REF} *)
  | NSC_NSDeclared
      (** {b Prefix Declared}
          {{:http://www.w3.org/TR/REC-xml-names/#nsc-NSDeclared}REF} *)
  | NSC_NoPrefixUndecl
      (** {b No Prefix Undeclaring}
          {{:http://www.w3.org/TR/REC-xml-names/#nsc-NoPrefixUndecl}REF} *)
  | NSC_AttrsUnique
      (** {b Attributes Unique}
          {{:http://www.w3.org/TR/REC-xml-names/#nsc-AttrsUnique}REF} *)

