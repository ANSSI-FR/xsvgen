(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: meta_xml_re.ml 1427 2012-04-19 07:45:33Z maarek $ *)

let (_ : unit) =
  Meta_lib.print_header "xml_re"

let (_ : unit) =
  Meta_lib.print_next_charset
    "S"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_S)

let (_ : unit) =
  Meta_lib.print_test_charset
    "EncNameStartChar"
    (Meta_lib.xsd_one_re_of_range Xml_charsets.range_EncNameStartChar)

let (_ : unit) =
  Meta_lib.print_next_charset
    "EncNameChar"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_EncNameChar)

let (_ : unit) =
  Meta_lib.print_test_charset
    "NameStartChar_NS_mDisc"
    (Meta_lib.xsd_one_re_of_range Xml_charsets.range_NameStartChar_NS_mDisc)

let (_ : unit) =
  Meta_lib.print_next_charset
    "NameChar_NS_mDisc"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_NameChar_NS_mDisc)

let (_ : unit) =
  Meta_lib.print_test_charset
    "Name_NS_mDisc"
    (Xsd.RE_regexp
       [Xsd.RE_branch
          [Xsd.RE_piece
             (Xsd.RE_atom
                Xml_charsets.range_NameStartChar_NS_mDisc,
              None);
           Xsd.RE_piece
             (Xsd.RE_atom
                Xml_charsets.range_NameChar_NS_mDisc,
              Some Xsd.RE_none_or_more)]])

let (_ : unit) =
  Meta_lib.print_next_charset
    "CharData_mDisc_1_0"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_CharData_mDisc_1_0)

let (_ : unit) =
  Meta_lib.print_next_charset
    "CharData_mDisc_1_1"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_CharData_mDisc_1_1)

let (_ : unit) =
  Meta_lib.print_next_charset
    "CharData_mDisc_no_square_closing_bracket_1_0"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_CharData_mDisc_no_square_closing_bracket_1_0)

let (_ : unit) =
  Meta_lib.print_next_charset
    "CharData_mDisc_no_square_closing_bracket_1_1"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_CharData_mDisc_no_square_closing_bracket_1_1)

let (_ : unit) =
  Meta_lib.print_next_charset
    "Char_mDisc_no_square_closing_bracket_1_0"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_Char_mDisc_no_square_closing_bracket_1_0)

let (_ : unit) =
  Meta_lib.print_next_charset
    "Char_mDisc_no_square_closing_bracket_1_1"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_Char_mDisc_no_square_closing_bracket_1_1)

let (_ : unit) =
  Meta_lib.print_next_charset
    "Char_mDisc_no_hyphen_minus_1_0"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_Char_mDisc_no_hyphen_minus_1_0)

let (_ : unit) =
  Meta_lib.print_next_charset
    "Char_mDisc_no_hyphen_minus_1_1"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_Char_mDisc_no_hyphen_minus_1_1)

let (_ : unit) =
  Meta_lib.print_next_charset
    "Char_mDisc_no_question_mark_1_0"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_Char_mDisc_no_question_mark_1_0)

let (_ : unit) =
  Meta_lib.print_next_charset
    "Char_mDisc_no_question_mark_1_1"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_Char_mDisc_no_question_mark_1_1)

let (_ : unit) =
  Meta_lib.print_next_charset
    "AttValue_singlequote_mDisc_1_0"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_AttValue_singlequote_mDisc_1_0)

let (_ : unit) =
  Meta_lib.print_next_charset
    "AttValue_singlequote_mDisc_1_1"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_AttValue_singlequote_mDisc_1_1)

let (_ : unit) =
  Meta_lib.print_next_charset
    "AttValue_doublequote_mDisc_1_0"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_AttValue_doublequote_mDisc_1_0)

let (_ : unit) =
  Meta_lib.print_next_charset
    "AttValue_doublequote_mDisc_1_1"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_AttValue_doublequote_mDisc_1_1)

let (_ : unit) =
  Meta_lib.print_test_charset
    "language"
    Xsd_datatypes.xml_language

let (_ : unit) =
  Meta_lib.print_test_charset
    "uri"
    (Meta_lib.xsd_re_of_range Xml_charsets.range_Char_mDisc_1_0)
