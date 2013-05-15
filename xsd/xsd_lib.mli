(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd_lib.mli 1675 2012-05-25 16:17:43Z maarek $ *)


module M_composite : Map.S with type key = Xsd.composition

val to_ncname : (Stringdata.t -> bool) -> Stringdata.t -> Xsd.ncname
val to_id : Stringdata.t -> Xsd.id
val to_uri : Stringdata.t -> Xsd.uri

val check_xsd_version : Stringdata.t option -> unit
val read_whiteSpace : Stringdata.t -> Xsd.whitespace
val read_boolean : Stringdata.t -> bool
val read_form : Stringdata.t option -> Xsd.form option
val read_use : Stringdata.t -> Xsd.use
val read_nc_variety : Stringdata.t -> Xsd.nc_variety
val read_nc_list_item : Xsd.uri option -> Stringdata.t -> Xsd.uri option option
val read_processContents : Stringdata.t -> Xsd.process_contents


val read_pattern : Stringdata.t -> Xsd.regexp
val regexp_of_regexp : Xsd.regexp -> Lxsd.regexp



val to_posint : Stringdata.t -> int
val to_posint_unbounded : Stringdata.t -> Xsd.max_occurrence_val
val to_nonNegativeInteger : Stringdata.t -> int

val empty_item_facets : Lxsd.item_facets
val empty_item_whitespace : Lxsd.whitespace
val empty_list_facets :  Lxsd.list_facets
val empty_list_whitespace : Lxsd.whitespace
val get_built_in_st_details : Lxsd.built_in_st -> Lxsd.st_resolved
