(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml.mli 1654 2012-05-22 17:21:28Z maarek $ *)

(** {1 XML types} *)

(** {2 XML file information} *)

type version =
  | Version_1_1
  | Version_1_0

type encoding =
  | Encoding_UTF8

type declaration =
    { version : version;
      encoding : encoding option;
      standalone : bool option }


(** {3 Unexpanded name} *)

type unexpanded_name =
    { prefix : Stringdata.t option;
      local_name : Stringdata.t }

(** {3 Expanded names} *)

(* TODO: do we need to add a list of used XML entity references? and a
   list of used IDs?  *)

type expanded_name =
    { uri : Stringdata.t;
      name : Stringdata.t }

module M_unexp : Map.S with type key = unexpanded_name
type unexpanded_attributes = Stringdata.t M_unexp.t

module M_exp : Map.S with type key = expanded_name
type expanded_attributes = Stringdata.t M_exp.t


(** {2 XML lexeme} *)

type raw_tag = unexpanded_name * unexpanded_attributes
type raw_gat = unexpanded_name

type lexeme =
  | Tag of raw_tag
  | Gat of raw_gat
  | Taggat of raw_tag
  | Data of Stringdata.t * bool (** Content data and boolean value indicating if the data contain one or more reference or CDSect *)
  | Doctype of Stringdata.t
  | PI of Stringdata.t * Stringdata.t (** Target and content *)

(** {2 XML trees} *)

(** {3 XML parametrized tree} *)

(** XML parametrized tree *)
type 'element tree =
    { declaration : declaration;
      root : 'element node
    }
and 'element node =
    { node : 'element;
      contents : 'element content list }

and 'element content =
  | Node of 'element node (** an XML tree node*)
  | Leaf of Stringdata.t (** an XML tree leaf *)


(** XML parametrized element *)
type ('name,'attributes) element =
    { element_name : 'name;
      attributes : 'attributes }

type unexpanded_element =
    (unexpanded_name,unexpanded_attributes) element
type expanded_element =
    (expanded_name,expanded_attributes) element

type expanded_content =
    expanded_element content


(******************************************************************************)

(** {3 Raw element} *)

type raw_element =
  | Element_tag_gat of raw_tag * raw_gat
  | Element_taggat of raw_tag

(* ASSERT: TAGGAT should only be in node with no children. *)


type stream =
  | Stream of (unit -> (lexeme * stream) option)


(** {3 XML trees} *)


(** XML raw tree *)
type unexpanded_tree =
    unexpanded_element tree

(** XML tree *)
type expanded_tree =
    expanded_element tree


(** {2 XML parser configuration} *)

type xsi_status =
  | XSI_reject_all
  | XSI_ignore_schemaLocation_only
  | XSI_ignore_all

type error_status =
    { doctype : bool;
      cdata : bool;
      pi : bool;
      comment : bool;
      ascii_ref : bool;
      xsi : xsi_status }

type configuration =
    { max_attributes : int option;
      max_contents : int option;
      max_depth : int option;
      max_length : int option;
      error_status : error_status }

(** Space handling value *)

type space =
  | Preserve
  | Default
