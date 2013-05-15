(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_charsets.ml 1453 2012-04-24 15:55:52Z maarek $ *)

(** {1 XML charsets library} *)

(** {b S} space characters *)
(* #x20 | #x9 | #xD | #xA *)
let range_S =
  Utf8.union
    [ Utf8.range_of_int 0x20;
      Utf8.range_of_int 0x9;
      Utf8.range_of_int 0xD;
      Utf8.range_of_int 0xA ]

(** {b EncName} first character *)
(* [A-Za-z] *)
let range_EncNameStartChar =
  Utf8.union
    [ Utf8.range_of_ints (Char.code 'A') (Char.code 'Z');
      Utf8.range_of_ints (Char.code 'a') (Char.code 'z') ]

(** {b EncName} next characters *)
(* [A-Za-z0-9._] | '-' *)
let range_EncNameChar =
  Utf8.union
    [ Utf8.range_of_ints (Char.code 'A') (Char.code 'Z');
      Utf8.range_of_ints (Char.code 'a') (Char.code 'z');
      Utf8.range_of_ints (Char.code '0') (Char.code '9');
      Utf8.range_of_int (Char.code '.');
      Utf8.range_of_int (Char.code '_');
      Utf8.range_of_int (Char.code '-') ]

(** Characters discouraged in XML 1.0.
    {{:http://www.w3.org/TR/2008/REC-xml-20081126/#charsets}REF}
*)
(*
[#x7F-#x84], [#x86-#x9F], [#xFDD0-#xFDEF],
[#x1FFFE-#x1FFFF], [#x2FFFE-#x2FFFF], [#x3FFFE-#x3FFFF],
[#x4FFFE-#x4FFFF], [#x5FFFE-#x5FFFF], [#x6FFFE-#x6FFFF],
[#x7FFFE-#x7FFFF], [#x8FFFE-#x8FFFF], [#x9FFFE-#x9FFFF],
[#xAFFFE-#xAFFFF], [#xBFFFE-#xBFFFF], [#xCFFFE-#xCFFFF],
[#xDFFFE-#xDFFFF], [#xEFFFE-#xEFFFF], [#xFFFFE-#xFFFFF],
[#x10FFFE-#x10FFFF].
*)
let range_Discouraged_1_0 =
  Utf8.union
    [ Utf8.range_of_ints 0x7F 0x84;
      Utf8.range_of_ints 0x86 0x9F;
      Utf8.range_of_ints 0xFDD0 0xFDEF;
      Utf8.range_of_ints 0x1FFFE 0x1FFFF;
      Utf8.range_of_ints 0x2FFFE 0x2FFFF;
      Utf8.range_of_ints 0x3FFFE 0x3FFFF;
      Utf8.range_of_ints 0x4FFFE 0x4FFFF;
      Utf8.range_of_ints 0x5FFFE 0x5FFFF;
      Utf8.range_of_ints 0x6FFFE 0x6FFFF;
      Utf8.range_of_ints 0x7FFFE 0x7FFFF;
      Utf8.range_of_ints 0x8FFFE 0x8FFFF;
      Utf8.range_of_ints 0x9FFFE 0x9FFFF;
      Utf8.range_of_ints 0xAFFFE 0xAFFFF;
      Utf8.range_of_ints 0xBFFFE 0xBFFFF;
      Utf8.range_of_ints 0xCFFFE 0xCFFFF;
      Utf8.range_of_ints 0xDFFFE 0xDFFFF;
      Utf8.range_of_ints 0xEFFFE 0xEFFFF;
      Utf8.range_of_ints 0xFFFFE 0xFFFFF;
      Utf8.range_of_ints 0x10FFFE 0x10FFFF ]



(**  Characters discouraged in XML 1.1.
{{:http://www.w3.org/TR/2006/REC-xml11-20060816/#charsets}REF}
*)
(*
[#x1-#x8], [#xB-#xC], [#xE-#x1F], [#x7F-#x84], [#x86-#x9F], [#xFDD0-#xFDDF],
[#x1FFFE-#x1FFFF], [#x2FFFE-#x2FFFF], [#x3FFFE-#x3FFFF],
[#x4FFFE-#x4FFFF], [#x5FFFE-#x5FFFF], [#x6FFFE-#x6FFFF],
[#x7FFFE-#x7FFFF], [#x8FFFE-#x8FFFF], [#x9FFFE-#x9FFFF],
[#xAFFFE-#xAFFFF], [#xBFFFE-#xBFFFF], [#xCFFFE-#xCFFFF],
[#xDFFFE-#xDFFFF], [#xEFFFE-#xEFFFF], [#xFFFFE-#xFFFFF],
[#x10FFFE-#x10FFFF].
*)
let range_Discouraged_1_1 =
  Utf8.union
    [ Utf8.range_of_ints 0x1 0x8;
      Utf8.range_of_ints 0xB 0xC;
      Utf8.range_of_ints 0xE 0x1F;
      Utf8.range_of_ints 0x7F 0x84;
      Utf8.range_of_ints 0x86 0x9F;
      Utf8.range_of_ints 0xFDD0 0xFDDF;
      Utf8.range_of_ints 0x1FFFE 0x1FFFF;
      Utf8.range_of_ints 0x2FFFE 0x2FFFF;
      Utf8.range_of_ints 0x3FFFE 0x3FFFF;
      Utf8.range_of_ints 0x4FFFE 0x4FFFF;
      Utf8.range_of_ints 0x5FFFE 0x5FFFF;
      Utf8.range_of_ints 0x6FFFE 0x6FFFF;
      Utf8.range_of_ints 0x7FFFE 0x7FFFF;
      Utf8.range_of_ints 0x8FFFE 0x8FFFF;
      Utf8.range_of_ints 0x9FFFE 0x9FFFF;
      Utf8.range_of_ints 0xAFFFE 0xAFFFF;
      Utf8.range_of_ints 0xBFFFE 0xBFFFF;
      Utf8.range_of_ints 0xCFFFE 0xCFFFF;
      Utf8.range_of_ints 0xDFFFE 0xDFFFF;
      Utf8.range_of_ints 0xEFFFE 0xEFFFF;
      Utf8.range_of_ints 0xFFFFE 0xFFFFF;
      Utf8.range_of_ints 0x10FFFE 0x10FFFF ]

(** *)
let range_Discouraged_1_0_and_1_1 =
  Utf8.union
    [ range_Discouraged_1_0;
      range_Discouraged_1_1 ]


(** *)
(* [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF] *)
let range_NameStartChar_NS_mDisc =
  Utf8.sub
    ( Utf8.union
        [ Utf8.range_of_ints (Char.code 'A') (Char.code 'Z');
          Utf8.range_of_int (Char.code '_');
          Utf8.range_of_ints (Char.code 'a') (Char.code 'z');
          Utf8.range_of_ints 0xC0 0xD6;
          Utf8.range_of_ints 0xD8 0xF6;
          Utf8.range_of_ints 0xF8 0x2FF;
          Utf8.range_of_ints 0x370 0x37D;
          Utf8.range_of_ints 0x37F 0x1FFF;
          Utf8.range_of_ints 0x200C 0x200D;
          Utf8.range_of_ints 0x2070 0x218F;
          Utf8.range_of_ints 0x2C00 0x2FEF;
          Utf8.range_of_ints 0x3001 0xD7FF;
          Utf8.range_of_ints 0xF900 0xFDCF;
          Utf8.range_of_ints 0xFDF0 0xFFFD;
          Utf8.range_of_ints 0x10000 0xEFFFF ] )
    range_Discouraged_1_0_and_1_1

let range_NameStartChar_mDisc =
  Utf8.union
    [ Utf8.range_of_int (Char.code ':');
      range_NameStartChar_NS_mDisc ]



(** *)
(* NameStartChar_NS | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040] *)
let range_NameChar_NS_mDisc =
  Utf8.sub
    (Utf8.union
       [ range_NameStartChar_NS_mDisc;
         Utf8.range_of_int (Char.code '-');
         Utf8.range_of_int (Char.code '.');
         Utf8.range_of_ints (Char.code '0') (Char.code '9');
         Utf8.range_of_int 0xB7;
         Utf8.range_of_ints 0x0300 0x036F;
         Utf8.range_of_ints 0x203F 0x2040 ] )
    range_Discouraged_1_0_and_1_1

let range_NameChar_mDisc =
  Utf8.union
    [ Utf8.range_of_int (Char.code ':');
      range_NameChar_NS_mDisc ]


(** *)
(*  #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF] *)
let range_Char_mDisc_1_0 =
  Utf8.sub
    (Utf8.union
       [ Utf8.range_of_int 0x9;
         Utf8.range_of_int 0xA;
         Utf8.range_of_int 0xD;
         Utf8.range_of_ints 0x20 0xD7FF;
         Utf8.range_of_ints 0xE000 0xFFFD;
         Utf8.range_of_ints 0x10000 0x10FFFF ] )
    range_Discouraged_1_0

(** *)
(*  ([#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]) - ([#x1-#x8] | [#xB-#xC] | [#xE-#x1F] | [#x7F-#x84] | [#x86-#x9F]) *)
let range_Char_mDisc_1_1 =
  Utf8.sub
    (Utf8.sub
       (Utf8.union
          [ Utf8.range_of_ints 0x1 0xD7FF;
            Utf8.range_of_ints 0xE000 0xFFFD;
            Utf8.range_of_ints 0x10000 0x10FFFF ] )
       ( Utf8.union
           [ Utf8.range_of_ints 0x1 0x8;
             Utf8.range_of_ints 0xB 0xC;
             Utf8.range_of_ints 0xE 0x1F;
             Utf8.range_of_ints 0x7F 0x84;
             Utf8.range_of_ints 0x86 0x9F ] ) )
    range_Discouraged_1_1

(** *)
(* Char_1_0 - ("<" | "&") *)
let range_CharData_mDisc_1_0 =
  Utf8.sub
    range_Char_mDisc_1_0
    ( Utf8.union
        [ Utf8.range_of_int (Char.code '<');
          Utf8.range_of_int (Char.code '&') ] )

(** *)
(* Char_1_1 - ("<" | "&") *)
let range_CharData_mDisc_1_1 =
  Utf8.sub
    range_Char_mDisc_1_1
    ( Utf8.union
        [ Utf8.range_of_int (Char.code '<');
          Utf8.range_of_int (Char.code '&') ] )


(** *)
(* CharData_1_0 - ("]") *)
let range_CharData_mDisc_no_square_closing_bracket_1_0 =
  Utf8.sub
    range_CharData_mDisc_1_0
    ( Utf8.range_of_int (Char.code ']') )


(** *)
(* CharData_1_1 - ("]") *)
let range_CharData_mDisc_no_square_closing_bracket_1_1 =
  Utf8.sub
    range_CharData_mDisc_1_1
    ( Utf8.range_of_int (Char.code ']') )

(** *)
(* Char_1_0 - ("]") *)
let range_Char_mDisc_no_square_closing_bracket_1_0 =
  Utf8.sub
    range_Char_mDisc_1_0
    ( Utf8.range_of_int (Char.code ']') )

(** *)
(* Char_1_1 - ("]") *)
let range_Char_mDisc_no_square_closing_bracket_1_1 =
  Utf8.sub
    range_Char_mDisc_1_1
    ( Utf8.range_of_int (Char.code ']') )


(** *)
(* Char_1_0 - ("-") *)
let range_Char_mDisc_no_hyphen_minus_1_0 =
  Utf8.sub
    range_Char_mDisc_1_0
    ( Utf8.range_of_int (Char.code '-') )

(** *)
(* Char_1_1 - ("-") *)
let range_Char_mDisc_no_hyphen_minus_1_1 =
  Utf8.sub
    range_Char_mDisc_1_1
    ( Utf8.range_of_int (Char.code '-') )


(** *)
(* Char_1_0 - ("?") *)
let range_Char_mDisc_no_question_mark_1_0 =
  Utf8.sub
    range_Char_mDisc_1_0
    ( Utf8.range_of_int (Char.code '?') )


(** *)
(* Char_1_1 - ("?") *)
let range_Char_mDisc_no_question_mark_1_1 =
  Utf8.sub
    range_Char_mDisc_1_1
    ( Utf8.range_of_int (Char.code '?') )

(** *)
(* Char_1_0 - ("<" | "&" | "'") *)
let range_AttValue_singlequote_mDisc_1_0 =
  Utf8.sub
    range_Char_mDisc_1_0
    ( Utf8.union
        [ Utf8.range_of_int (Char.code '<');
          Utf8.range_of_int (Char.code '&');
          Utf8.range_of_int (Char.code '\''); ] )


(** *)
(* Char_1_1 - ("<" | "&" | "'") *)
let range_AttValue_singlequote_mDisc_1_1 =
  Utf8.sub
    range_Char_mDisc_1_1
    ( Utf8.union
        [ Utf8.range_of_int (Char.code '<');
          Utf8.range_of_int (Char.code '&');
          Utf8.range_of_int (Char.code '\''); ] )


(** *)
(* Char_1_0 - ("<" | "&" | "\"") *)
let range_AttValue_doublequote_mDisc_1_0 =
  Utf8.sub
    range_Char_mDisc_1_0
    ( Utf8.union
        [ Utf8.range_of_int (Char.code '<');
          Utf8.range_of_int (Char.code '&');
          Utf8.range_of_int (Char.code '\"'); ] )


(* Char_1_1 - ("<" | "&" | "\"") *)
let range_AttValue_doublequote_mDisc_1_1 =
  Utf8.sub
    range_Char_mDisc_1_1
    ( Utf8.union
        [ Utf8.range_of_int (Char.code '<');
          Utf8.range_of_int (Char.code '&');
          Utf8.range_of_int (Char.code '\"'); ] )
