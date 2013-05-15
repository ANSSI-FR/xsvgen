(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: validator.ml 1687 2012-05-30 10:23:30Z maarek $ *)

(** {1 XSD Validator}

    XSD Validator reads the command line, opens the file to validate,
    turns its content into a an XML tree and run the validating parser
    engine with to validate the tree according to the XSD validating
    table.
 *)

(** Apply the validating parser engine on the given input file *)
let validate (file_in_name : File_in.t) :
    Val_parser.vresult =
  let ic =
    File_in.to_in_channel file_in_name [File_in.Xml] in
  let xml_sd =
    Stringdata.of_in_channel ic file_in_name in
  let (xml_decl,xml_stream) =
    Xml_lexer.lex_xml Xsval.xml_configuration.Xml.error_status xml_sd in
  let tree =
    Xml_parser.parse Xsval.xml_configuration (xml_decl,xml_stream) in
  Val_parser.validate_schema tree

(** Main function which reads the command line and launch the
    validation process *)
let () =
  try
    begin
      try
        begin
          let n = Array.length Sys.argv in
          if n = 1 then
            Message.error Error.XSV_input_file_not_provided;
          if n > 2 then
            Message.error Error.XSV_multiple_input_files_not_allowed;
          if  n <> 2 then
            Message.error Error.XSV_command_line_incorrect
          else
            let file_in_name = File_in.of_string Sys.argv.(1) [File_in.Xml] in
            Message.warning Error.XSV_debug_mode;
            Message.info
              (Message.INFO_validator_configuration
                 Xsval.xml_configuration);
            match validate file_in_name with
            | Lib.KO l ->
                List.iter
                  (function
                    | (cvc,None) ->
                        Message.warning_cvc cvc
                    | (cvc,Some loc) ->
                        Message.warning_cvc_at loc cvc)
                  l;
                Message.error Error.XSV_fail
            | Lib.OK l ->
                List.iter
                  (function
                    | (cvc,None) ->
                    Message.info_cvc cvc
                    | (cvc,Some loc) ->
                    Message.info_cvc_at loc cvc)
                  l;
                Message.info Message.INFO_document_valid;
                exit Message.valid_exit_code
        end
      with
      | Stack_overflow ->
          Message.error (Error.EXE ("stack limit reached",Stack_overflow))
      | Error.E e -> Message.error e
      | Stringdata.E_at (sd,e) -> Message.error_at sd e
    end
  with
  | Message.E e ->
      exit (Message.error_exit_code e)
  | _ ->
      exit Message.default_error_exit_code
