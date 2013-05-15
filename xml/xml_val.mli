(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_val.mli 1646 2012-05-16 08:19:57Z maarek $ *)

val xml_namespace_prefix : Stringdata.t
val xml_namespace : Stringdata.t
val xmlns_namespace_prefix : Stringdata.t
val xmlns_namespace : Stringdata.t
val xml_id_localname : Stringdata.t
val xml_lang_localname : Stringdata.t
val xml_base_localname : Stringdata.t
val xml_space_localname : Stringdata.t

val e_xml_space : Xml.expanded_name
val v_xml_space_preserve : Stringdata.t
val v_xml_space_default : Stringdata.t

val xsi_namespace : Stringdata.t
val e_xsi_schemaLocation : Xml.expanded_name

val predefined_entities : (Stringdata.t * Stringdata.t) list

val silent_error_status : Xml.error_status
val default_xml_configuration : Xml.configuration
val debug_error_status : Xml.error_status
val generator_xml_configuration : Xml.configuration
