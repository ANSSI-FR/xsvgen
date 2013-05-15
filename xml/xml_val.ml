(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_val.ml 1646 2012-05-16 08:19:57Z maarek $ *)

(** {1 XML values} *)

let xml_namespace_prefix =
  Stringdata.of_string "xml"
let xml_namespace =
  Stringdata.of_string "http://www.w3.org/XML/1998/namespace"
let xmlns_namespace_prefix =
  Stringdata.of_string "xmlns"
let xmlns_namespace =
  Stringdata.of_string "http://www.w3.org/2000/xmlns/"
let xml_id_localname =
  Stringdata.of_string "id"
let xml_lang_localname =
  Stringdata.of_string "lang"
let xml_base_localname =
  Stringdata.of_string "base"
let xml_space_localname =
  Stringdata.of_string "space"

let e_xml_space =
  { Xml.uri = xml_namespace;
    Xml.name = xml_space_localname }
let v_xml_space_preserve = Stringdata.of_string "preserve"
let v_xml_space_default = Stringdata.of_string "default"


let xsi_namespace =
  Stringdata.of_string "http://www.w3.org/2001/XMLSchema-instance"
let e_xsi_schemaLocation =
  { Xml.uri = xsi_namespace;
    Xml.name = Stringdata.of_string "schemaLocation" }

let predefined_entities =
  [ (Stringdata.of_string "amp"), (Stringdata.of_string "&");
    (Stringdata.of_string "lt"), (Stringdata.of_string "<");
    (Stringdata.of_string "gt"), (Stringdata.of_string ">");
    (Stringdata.of_string "apos"), (Stringdata.of_string "'");
    (Stringdata.of_string "quot"), (Stringdata.of_string "\"") ]

(** {2 Predefined configurations} *)

let silent_error_status =
  { Xml.doctype = true;
    Xml.cdata = true;
    Xml.pi = true;
    Xml.comment = true;
    Xml.ascii_ref = true;
    Xml.xsi = Xml.XSI_reject_all
  }

let default_xml_configuration =
  { Xml.max_attributes = None;
    Xml.max_contents = None;
    Xml.max_depth = None;
    Xml.max_length = None;
    Xml.error_status = silent_error_status }

let debug_error_status =
  { Xml.doctype = false;
    Xml.cdata = false;
    Xml.pi = false;
    Xml.comment = false;
    Xml.ascii_ref = false;
    Xml.xsi = Xml.XSI_ignore_schemaLocation_only }

let generator_xml_configuration =
  { Xml.max_attributes = None;
    Xml.max_contents = None;
    Xml.max_depth = None;
    Xml.max_length = None;
    Xml.error_status = debug_error_status }
