(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: generator.ml 1711 2012-06-04 18:07:56Z maarek $ *)

(** {1 XSD validator generator} *)

let version = String.copy "0.2"


let read_commandline () =
  let generator_basename = Sys.argv.(0) in
  let validator_basename = ref (Stringdata.of_string "xsval") in
  let validator_debug_mode = ref false in
  let xsd_file_names = ref [] in
  let validator_xml_configuration =
    ref Xml_val.default_xml_configuration in
  let value v option =
    if v = "unbounded"
    then None
    else
      try let i = int_of_string v in
          if i >=0
          then Some i
          else failwith "negative value"
      with
      | Failure _ -> raise (Arg.Bad ("wrong argument for option -" ^ option))
  in
  let string_of_value = function
    | None -> "unbounded"
    | Some i -> string_of_int i in
  let set_validator_debug_mode () =
    validator_debug_mode := true;
    let { Xml.max_attributes = m_a;
          Xml.max_contents = m_c;
          Xml.max_depth = m_d;
          Xml.max_length = m_l;
          Xml.error_status = _ } =
      !validator_xml_configuration in
    validator_xml_configuration :=
      { Xml.max_attributes = m_a;
        Xml.max_contents = m_c;
        Xml.max_depth = m_d;
        Xml.max_length = m_l;
        Xml.error_status = Xml_val.debug_error_status }
  in
  let set_validator_basename n =
    let n_sd = Stringdata.of_string n in
    let printable_ascii =
      Stringdata.of_string "[ -~]*" in
    let no_slash =
      Stringdata.of_string "[^/]*" in
    let only_escaped_spaces =
      Stringdata.of_string "\\([\\][ ]\\|[^ ]\\)*" in
    let no_quotes =
      Stringdata.of_string "[^\"']*" in
    if
      n = "" || n = "." || n = ".." ||
      generator_basename = n ^ ".bin" ||
      not (Stringdata.match_regexp printable_ascii n_sd) ||
      not (Stringdata.match_regexp no_slash n_sd) ||
      not (Stringdata.match_regexp only_escaped_spaces n_sd) ||
      not (Stringdata.match_regexp no_quotes n_sd)
    then
      raise (Arg.Bad "wrong argument for option -name")
    else
      validator_basename := n_sd
  in
  let set_max_attributes v =
    let { Xml.max_attributes = _;
          Xml.max_contents = m_c;
          Xml.max_depth = m_d;
          Xml.max_length = m_l;
          Xml.error_status = e_s } =
      !validator_xml_configuration in
    validator_xml_configuration :=
      { Xml.max_attributes = value v "max-attributes";
        Xml.max_contents = m_c;
        Xml.max_depth = m_d;
        Xml.max_length = m_l;
        Xml.error_status = e_s } in
  let set_max_contents v =
    let { Xml.max_attributes = m_a;
          Xml.max_contents = _;
          Xml.max_depth = m_d;
          Xml.max_length = m_l;
          Xml.error_status = e_s } =
      !validator_xml_configuration in
    validator_xml_configuration :=
      { Xml.max_attributes = m_a;
        Xml.max_contents = value v "max-contents";
        Xml.max_depth = m_d;
        Xml.max_length = m_l;
        Xml.error_status = e_s } in
  let set_max_depth v =
    let { Xml.max_attributes = m_a;
          Xml.max_contents = m_c;
          Xml.max_depth = _;
          Xml.max_length = m_l;
          Xml.error_status = e_s } =
      !validator_xml_configuration in
    validator_xml_configuration :=
      { Xml.max_attributes = m_a;
        Xml.max_contents = m_c;
        Xml.max_depth = value v "max-depth";
        Xml.max_length = m_l;
        Xml.error_status = e_s } in
  let set_max_length v =
    let { Xml.max_attributes = m_a;
          Xml.max_contents = m_c;
          Xml.max_depth = m_d;
          Xml.max_length = _;
          Xml.error_status = e_s } =
      !validator_xml_configuration in
    validator_xml_configuration :=
      { Xml.max_attributes = m_a;
        Xml.max_contents = m_c;
        Xml.max_depth = m_d;
        Xml.max_length = value v "max-length";
        Xml.error_status = e_s } in
  let usage_information =
    Format.sprintf "%s%s@.%s%s%s@.%s"
      "XML Schema validator generator, version "
      version
      "Usage: "
      generator_basename
      " <options> <XSD-primary-file> [<XSD-secondary-files>]"
      "Options are (with <value> being either an integer or 'unbounded'):"
  in
  Arg.parse
    [ "-debug",
      Arg.Unit set_validator_debug_mode,
      "Generate a validator which produces debugging information.";
      "-name",
      Arg.String set_validator_basename,
      (Format.sprintf "%s%s%s"
         "<name> Set the name of the validator (default '"
         (Stringdata.to_string !validator_basename)
         "').");
      "-max-attributes",
      Arg.String set_max_attributes,
      (Format.sprintf "%s%s%s"
         "<value> Set the maximum number of attributes per element\n     accepted by the validator (default '"
         (string_of_value
            Xml_val.default_xml_configuration.Xml.max_attributes)
         "').");
      "-max-contents",
      Arg.String set_max_contents,
      (Format.sprintf "%s%s%s"
         "<value> Set the maximum number of content children per element\n     accepted by the validator (default '"
         (string_of_value
            Xml_val.default_xml_configuration.Xml.max_contents)
         "').");
      "-max-depth",
      Arg.String set_max_depth,
      (Format.sprintf "%s%s%s"
         "<value> Set the maximum element tree depth accepted by the\n     validator (default '"
         (string_of_value
            Xml_val.default_xml_configuration.Xml.max_depth)
         "').");
      "-max-length",
      Arg.String set_max_length,
      (Format.sprintf "%s%s%s"
         "<value> Set the maximum length in bytes of attribute value and\n     string content accepted by the validator (default '"
         (string_of_value
            Xml_val.default_xml_configuration.Xml.max_length)
         "').") ]
    (fun file_name ->
      xsd_file_names :=
        (File_in.of_string file_name [File_in.Xsd]) :: !xsd_file_names)
    usage_information;
  (File_out.of_string
     !validator_debug_mode
     (Stringdata.to_string !validator_basename)),
  (List.rev !xsd_file_names),
  !validator_xml_configuration

(* TODO open_out the out_files early*)


let read_xsd is_primary expected_uri_opt file_in_name =
  let ic =
    File_in.to_in_channel file_in_name [File_in.Xsd] in
  let xml_sd =
    Stringdata.of_in_channel ic file_in_name in
  let (xml_decl,xml_stream) =
    Xml_lexer.lex_xml
      Xml_val.generator_xml_configuration.Xml.error_status
      xml_sd in
  let tree =
    Xml_parser.parse
      Xml_val.generator_xml_configuration
      (xml_decl,xml_stream) in
  Message.info (Message.INFO_file_XML_valid file_in_name);
  let composite_xsd =
    Xsd_transducer.transduce tree in
  let target_uri_opt =
    Lib.opt_apply
      (fun (Xsd.Uri uri_sd) -> uri_sd)
      composite_xsd.Xsd.target
  in
  match expected_uri_opt,target_uri_opt with
  | None,None ->
      composite_xsd
  | None,(Some target_uri) ->
      if is_primary
      then composite_xsd
      else
        Message.error_at target_uri
          Error.GEN_import_effective_uris_mismatch
  | (Some expected_uri),None ->
      Message.error_at expected_uri
        Error.GEN_import_effective_uris_mismatch
  | (Some expected_uri),(Some target_uri) ->
      if Stringdata.eq expected_uri target_uri
      then
        composite_xsd
      else
        Message.error_at expected_uri
          Error.GEN_import_effective_uris_mismatch

let check_not_primary_target primary_uri = function
  | None -> None
  | (Some uri) as sloc ->
      if Stringdata.eq uri primary_uri
      then
        Message.error_at uri
          Error.GEN_primary_uri_import_not_allowed
      else
        sloc

let read_referenced primary (Xsd.Uri primary_uri) secondaries =
  let rec aux requester parent_uri (referenced_xsd,awaiting_xsd_files) =
    let current_uri =
      match requester.Xsd.target with
      | None ->
          parent_uri
      | Some uri ->
          uri
    in
    match requester.Xsd.schema_form with
    | Xsd.Modules [] ->
        (* No modules requested *)
        (referenced_xsd,awaiting_xsd_files)
    | Xsd.Modules l ->
        List.fold_left
          (fun (refs,awaits) comp ->
            if Xsd_lib.M_composite.mem comp refs
            then (refs,awaits)
            else
              match comp with
              | Xsd.Import (expected_ns_opt,schemalocation) ->
                  let expected_ns_opt_checked =
                    check_not_primary_target primary_uri expected_ns_opt in
                  let xsd_file_name_opt,awaits' =
                    Lib.list_find_remove
                      (fun in_name ->
                        let in_name_sd =
                          Stringdata.of_string
                            (File_in.to_string in_name) in
                        Stringdata.url_match
                          in_name_sd
                          schemalocation)
                      (fun x -> x)
                      awaits
                  in
                  begin
                    match xsd_file_name_opt with
                    | None ->
                        Message.error_at schemalocation
                          Error.GEN_referenced_file_missing
                    | Some xsd_file_name ->
                        let xsd =
                          read_xsd
                            false
                            expected_ns_opt_checked
                            xsd_file_name in
                        let refs' =
                          Xsd_lib.M_composite.add
                            comp
                            (current_uri,xsd)
                            refs in
                        aux xsd current_uri(refs',awaits')
                  end)
          (referenced_xsd,awaiting_xsd_files)
          l
  in
  match aux
      primary
      (Xsd.Uri primary_uri)
      (Xsd_lib.M_composite.empty,secondaries) with
  | (secondary_xsd_s,[]) ->
      secondary_xsd_s
  | (_,file::_) ->
      let ic =
        File_in.to_in_channel file [File_in.Xsd] in
      let xml_sd =
        Stringdata.of_in_channel ic file in
      Message.error_at xml_sd Error.GEN_file_not_referenced

let generate
    (primary_xsd_file_name : File_in.t)
    (secondary_xsd_file_names : File_in.t list)
    (file_out_basename : File_out.t)
    (validator_xml_configuration : Xml.configuration) :
    unit =
  let prepared_file_out_basename = File_out.prepare file_out_basename in
  let formatter =
    File_out.to_formatter prepared_file_out_basename File_out.Log in
  Message.set_log formatter;
  if not (Xml_lib.is_silent_mode validator_xml_configuration)
  then
    Message.warning Error.GEN_debug_mode;
  Message.info (Message.INFO_XSD_primary_file primary_xsd_file_name);
  List.iter
    (fun file -> Message.info (Message.INFO_XSD_secondary_file file))
    secondary_xsd_file_names;
  Message.info
    (Message.INFO_validator_configuration validator_xml_configuration);
  let primary_xsd =
    read_xsd true None primary_xsd_file_name in
  let primary_uri =
    match primary_xsd.Xsd.target with
    | None -> Message.error Error.GEN_XSDT_primary_target_uri_missing
    | Some uri -> uri in
  let secondary_xsd_s =
    read_referenced primary_xsd primary_uri secondary_xsd_file_names in
  let flat_xsd =
    Xsd_transducer.inline primary_xsd secondary_xsd_s in
  let lxsd =
    Xsd_lineariser.linearise flat_xsd
  in
  let ml_out =
    File_out.to_formatter prepared_file_out_basename File_out.ML in
  let sh_out =
    File_out.to_formatter prepared_file_out_basename File_out.Sh in
  Code_generator.generate_ml ml_out validator_xml_configuration lxsd;
  Code_generator.generate_sh sh_out prepared_file_out_basename

let ( _ : unit ) =
  try
    begin
      try
        match read_commandline () with
        | validator_basename,
          main_xsd_file_name :: secondary_xsd_file_names,
          validator_xml_configuration ->
            generate
              main_xsd_file_name
              secondary_xsd_file_names
              validator_basename
              validator_xml_configuration;
            File_in.close_opened_files ();
            File_out.close_opened_files ();
            exit 0
        | _,[],_ ->
            Message.error Error.F_GEN_primary_file_not_provided
      with
      | Stack_overflow ->
          Message.error (Error.EXE ("stack limit reached",Stack_overflow))
      | Error.E e -> Message.error e
      | Stringdata.E_at (sd,e) -> Message.error_at sd e
      | Message.E _ as e -> raise e
      | e -> Message.error (Error.EXE ("unexpected exception",e))
    end
  with
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
        | Error.XMLP_xml_space_value_invalid
        | Error.XMLNS_namespace_uri_reserved
        | Error.XMLNS_namespace_prefix_reserved
        | Error.XMLNS_XMLP_xml_attribute_not_recognised
        | Error.XMLNS_namespace_prefix_not_declared
        | Error.XMLNS_prefixed_declaration_value_empty
        | Error.XMLNS_XSDT_uri_not_well_formed
        | Error.XMLL_pi_target_not_allowed
        | Error.XMLL_entity_reference_unknown
          (* 2: not well-formed *)
          -> exit 2
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
          (* 1: invalid but well-formed *)
          -> exit 1
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
      exit 3
