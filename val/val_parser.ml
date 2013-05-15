(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: val_parser.ml 1688 2012-05-30 16:18:53Z maarek $ *)

(** {1 Validating parser engine}

    The validatin parser engine runs the XSD validation of an XML tree
    according to the validating table {!Xsval.schema}.
 *)



(** {2 Validation environment} *)

(** Validation environment type *)
type env =
    { ns : Xml_ns.t; (** Namespace environment *)
      space : Xml.space (** 'xml:space' value *)
    }

(** Updates a validation environment given a map of attributes. The
    available attributes are updated according to the xmlns
    attributes. The value of xml:space attribute is also recorded.
    Updating an environment occurs only when reading a new element,
    see {!element_vfun}. The xml:space value is used only with
    {!Xml_lib.clear_space_leafs} when clearing empty XML leafs, the
    value is not taken into account when performing an action dictated
    by the XSD whitespace facet as the W3C recommendation ignores to
    mention xml:space.
 *)
let update_env attrs = function
  | { ns = ns;
      space = _ } ->
      { ns = Xml_ns.add_exp_attrs attrs ns;
        space = Xml_lib.get_space attrs }


(** {2 Type aliases}

These aliases are used as type annotation in this module to enhance
readability.  *)

(** Alias type for validation result *)
type vresult = ((Lxsd.cvc * Stringdata.t option) list,
                (Lxsd.cvc * Stringdata.t option) list) Lib.result

(** Alias type for simple content testing functions. *)
type tfun =
    (env * Stringdata.t) -> vresult


(** Alias type for inner parts validating functions. *)
type inner_vfun =
    unit -> vresult


(** Alias type for element validating functions. *)
type element_vfun =
    env * Xml.expanded_content -> inner_vfun option

(** Alias type for particle validating functions. *)
type particle_vfun =
    env * Xml.expanded_content list -> (inner_vfun * Xml.expanded_content list) option

(** Alias type for attribute validating functions. *)
type attribute_vfun =
    env * Xml.expanded_attributes -> (inner_vfun * Xml.expanded_attributes) option

(** Alias type for attributes validating functions. *)
type attributes_vfun =
    env * Xml.expanded_attributes -> inner_vfun option

(** Alias type for contents validating functions. *)
type contents_vfun =
    env * Xml.expanded_content list -> inner_vfun option

(** Alias type for children (attribute and content lists) partial validating
    functions. *)
type children_partial_vfun =
    env * (Xml.expanded_attributes * Xml.expanded_content list) -> (inner_vfun * Xml.expanded_content list) option

(** Alias type for children (attribute and content lists) complete validating
    functions. *)
type children_vfun =
    env * (Xml.expanded_attributes * Xml.expanded_content list) -> inner_vfun option


(** Alias type for element testing functions. *)
type element_tfun =
    env * (Stringdata.t * Xml.expanded_attributes * (env * Xml.expanded_content list)) -> vresult

(** Alias type for XML tree testing functions. *)
type tree_tfun =
    Xml.expanded_element Xml.tree -> vresult

(** {2 Combinators for validating and testing functions} *)

(** Combine inner validating functions *)
let combine_inner_vfun
    (f1 : inner_vfun)
    (f2 : inner_vfun) : inner_vfun =
  fun () ->
    Lib.list_result_combine (f1 ()) (f2 ())

(** Combine the result of an inner validating function with a list of
    validation rules *)
let combine_cvc_l_inner_vfun inner_f cvc_l =
  combine_inner_vfun
    inner_f
    (fun () -> Lib.OK cvc_l)

(** Combine a function validating a set of attributes with a function
    partially validating a list of contents into a function partially
    validating children (attributes and content lists) *)
let combine_a_vfun
    (f1 : attributes_vfun)
    (f2 : particle_vfun) : children_partial_vfun =
  (fun (env,(attrs,contents)) ->
    match f1 (env,attrs) with
    | None ->
        None
    | Some inner_f1 ->
        match f2 (env,contents) with
        | None ->
            None
        | Some (inner_f2,contents') ->
            Some ((combine_inner_vfun
                     inner_f1
                     inner_f2),
                  contents') )

(** Combine two functions partially validating a list of contents *)
let combine_vfun
    (f1 : particle_vfun)
    (f2 : particle_vfun) : particle_vfun =
  (fun (env,contents) ->
    match f1 (env,contents) with
    | None -> None
    | Some (inner_f1,contents') ->
        match f2 (env,contents') with
        | None -> None
        | Some (inner_f2,contents'') ->
            Some ((combine_inner_vfun
                     inner_f1
                     inner_f2),
                  contents'') )

(** Combine the result of a function partially validating a list
    of contents with a list of validation rules *)
let combine_cvc_l_vfun
    (f : particle_vfun)
    cvc_l : particle_vfun =
  combine_vfun
    f
    (fun (_,contents) ->
      Some ((fun () -> Lib.OK cvc_l),contents))

(** Combine the result of a function validating a list of contents
    with a list of validation rules *)
let combine_cvc_l_contents_vfun
    (f : contents_vfun)
    cvc_l : contents_vfun =
  (fun (env,contents) ->
    match f (env,contents) with
    | None -> None
    | Some inner_f ->
        Some (combine_cvc_l_inner_vfun
                inner_f
                cvc_l))

(** Combine the result of a function validating a set of attributes
    with a list of validation rules adn with the results of a function
    validating a list of contents *)
let combine_cvc_l_a_vfun
    (af : attributes_vfun)
    cvc_l
    (vf : contents_vfun) : children_vfun =
  (fun (env,(attrs,contents)) ->
    match af (env,attrs) with
    | None ->
        None
    | Some inner_f1 ->
        let inner_f1' =
          combine_cvc_l_inner_vfun
            inner_f1
            cvc_l
        in
        match vf (env,contents) with
        | None ->
            None
        | Some inner_f2 ->
            Some (combine_inner_vfun
                    inner_f1'
                    inner_f2))

(** Combine the result of a function validating children
    with a list of validation rules *)
let combine_cvc_l_children_vfun
    (f : children_vfun)
    cvc_l : children_vfun =
    (fun (env,(attrs,contents)) ->
      match f (env,(attrs,contents)) with
      | None -> None
      | Some inner_f ->
          Some (combine_cvc_l_inner_vfun
                  inner_f
                  cvc_l))

(** Return a function validating a list of contents out of a function
    partially validating children *)
let contents_only_vfun
    (f : children_partial_vfun) : particle_vfun =
  (fun (env,contents) ->
    f (env,(Xml.M_exp.empty,contents)))


(** Sort a list of contents into groups of XML nodes formed by
    elements sharing the same QName. The function drops the XML leafs
    contained in the contents list. *)
let sort_contents contents =
  let contents_map =
    List.fold_left
      (fun m content ->
        match content with
        | Xml.Node
            { Xml.node =
              { Xml.element_name = expanded_name;
                Xml.attributes = _ };
              Xml.contents = _ } ->
                let pre_content_list =
                  try Xml.M_exp.find expanded_name m with
                  | Not_found -> [] in
                let content_list =
                  content::pre_content_list
                in
                Xml.M_exp.add expanded_name content_list m
        | Xml.Leaf _ ->
            (* We do not treat data mixed with all as its
               presence/absence it checked by
               Lxsd.CT_mixed/Lxsd.CT_element_only *)
              m)
      Xml.M_exp.empty
      contents
  in
  List.map snd (Xml.M_exp.bindings contents_map)

(** Test if a given content list matches the given all-model group
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#all-mg}REF}. The
    all-model group is composed of a set of particle declarations and a
    set of wildcard declarations (limitation: the wildcard set should
    contain one item at most). The testing is as follows:
    - sort and group the contents (elements only) by element QName
    - for each particle declaration test if it matches one of the group
      and remove the group if it does; if it does not match any group,
      test if it matches the empty content
    - apply the wildcard declaration to the remaining groups
    - if all the content groups were matched, the contents match the
      all-mode.

    Termination:
    - termination of local function [aux]: the second argument
      decreases in length at each recursive call.
    - termination of local function [all]: the left part of the third
      argument decreases in length at each recursive call.
    - termination of the functions of type {!particle_vfun} given as
      arguments. These functions result from the application of
      {!particle_list_vfun} in {!modelGroup_vfun}.
 *)
let all_contents
    (element_vfun_list : particle_vfun list)
    (wildcard_vfun_list : particle_vfun list) : particle_vfun =
  let match_fully_apply_remove opt_apply (env,sorted_l_l) =
    let rec aux left_l_l = function
      | [] ->
          (* no sequence is matching,
             the case might match the empty sequence*)
          begin
            match opt_apply (env,[]) with
            | None ->
                None
            | Some (inner_f,[]) ->
                Some (inner_f,left_l_l)
            | Some (_,_) ->
                Message.error (Error.A_function "Val_parser.all_contents")
          end
      | l::t_l ->
          match opt_apply (env,l) with
          | Some (inner_f,[]) ->
              (* the sequence is matching, we return the
                 validation function and the sequences left *)
              Some (inner_f,left_l_l@t_l)
          | None
          | Some (_,_::_) ->
              (* the sequence is not matching or does not match
                 the entire list (i.e., has an 0 minOccurs),
                 we recurse on the other sequences *)
              aux (l::left_l_l) t_l
    in
    aux [] sorted_l_l
  in
  let rec all env cont = function
    | [],sorted_l_l ->
        (* nothing expected, something or nothing is left, we flattent
           the list of left contents *)
        cont (Some
                ((fun () ->
                  Lib.OK [(Lxsd.CVC_model_group_all,None);
                          (Lxsd.CVC_model_group,None)]),
                 List.flatten sorted_l_l))
    | (expected_h::expected_tail),sorted_l_l ->
        match match_fully_apply_remove expected_h (env,sorted_l_l)  with
        | None ->
            (* the first expected element(s) does not match,
               the entire sequence does not match *)
            None
        | Some (inner_vfun,left_l_l) ->
            (* the first expected element(s) matches one of
               the sub sequences *)
            all env
              ( function
                | None -> None
                | Some (remainder_inner_vfun,left_contents) ->
                    cont
                      (Some
                         ((combine_inner_vfun
                             inner_vfun
                             remainder_inner_vfun),
                          left_contents)))
              (expected_tail,left_l_l)
  in
  fun (env,contents) ->
    (* We first sort the contents so that the multiple occurrences of
       an element are joint in a single list, each element particle of
       the all-model being therefore provided at once with all the
       matching elements. The sorted contents is a list of content
       list. *)
    let sorted_l_l = sort_contents contents in
    (* We then search for the matching element particles *)
    match all env (fun x -> x) (element_vfun_list,sorted_l_l) with
    | None ->
        None
    | Some (inner_fun,left_contents) as res ->
        begin
          match wildcard_vfun_list with
          | [] ->
              (* as the left contents are sorted, we only return
                 it if it has been fully treated. *)
              if left_contents = []
              then res
              else None
          | [wildcard_vfun] ->
              begin
               match wildcard_vfun (env,left_contents) with
               | None -> None
               | Some (w_inner_fun,[]) ->
                   Some ((combine_inner_vfun inner_fun w_inner_fun),[])
               | Some _ ->
                   (* as the left contents are sorted, we only return
                      it if it has been fully treated. *)
                   None
              end
          | _ ->
              Message.error
                (Error.XSDL_XSV_LIM_more_than_one_wildcard_in_all_model)
        end

(** Test if a given content list matches the given choice-model
    group. The testing is as follows:
    - test if for one of the particle declarations matches the
      beginning of the content list
    - if it does not test if one of the particle declarations matches
      the empty content

    Termination:
    - termination of local function [choice]: the second argument
      decreases in length at each recursive call.
    - termination of the functions of type {!particle_vfun} given as
      arguments. These functions result from the application of
      {!particle_list_vfun} in {!modelGroup_vfun}.
   *)
let choice_contents
    (vfun_list : particle_vfun list) : particle_vfun =
  fun (env,contents) ->
    let rec choice accept_empty = function
      | [] ->
          None
      | h::t ->
          match h (env,contents) with
          | None -> choice accept_empty t
          | Some (vfun,left_list) ->
              if (not accept_empty)
                  && List.length left_list = List.length contents
              then
                choice accept_empty t
              else
                let vfun' =
                  combine_cvc_l_inner_vfun
                    vfun
                    [(Lxsd.CVC_model_group_choice,None);
                     (Lxsd.CVC_model_group,None)]
                in
                Some (vfun',left_list)
    in
    (* we first test if one of the choice is matching the content
       and making it to decrease *)
    match choice false vfun_list with
    | None ->
        (* if all choices fail, we try again if one of the choice
           is matching without making the content to decrease *)
        choice true vfun_list
    | (Some _) as res -> res

(** Test if a given content list matches the given sequence-model
    group. The testing is as follows:
    - test in a sequence each of the particle declarations. Each
      particle declaration is tested on the remaining content list
      left by the previous particle declaration of the sequence

    Termination:
    - termination of local function [sequence]: the first part of the
      third argument decreases in length at each recursive call.
    - termination of the functions of type {!particle_vfun} given as
      arguments. These functions result from the application of
      {!particle_list_vfun} in {!modelGroup_vfun}.
   *)
let sequence_contents
    (vfun_list : particle_vfun list) : particle_vfun =
  let rec sequence env cont = function
    | [],left_list ->
        (* nothing expected, nothing or something is left *)
        cont (Some
                ((fun () ->
                  Lib.OK [(Lxsd.CVC_model_group_sequence,None);
                          (Lxsd.CVC_model_group,None);]),
                 left_list))
    | (vf_h::vf_t),c_list ->
        begin
          match vf_h (env,c_list) with
          | None ->
              (* things expected, content not matching,
                 the sequence does not match *)
              None
          | Some (inner_vfun,c_left) ->
              (* thing expected, content (possibly empty) is matching,
                 we continue in the sequence *)
              sequence env
                ( function
                  | None ->
                      None
                  | Some (remainder_inner_vfun,left_list) ->
                      cont
                        (Some
                           ((combine_inner_vfun
                               inner_vfun
                               remainder_inner_vfun),
                            left_list)))
                (vf_t,c_left)
        end in
  fun (env,contents) ->
    sequence env (fun x -> x) (vfun_list,contents)

(** {2 Functions retrieving definition} *)

(** Test the equality between two ids.

    Note: the function is not used systematically at each id comparison *)
let eq_id =
  (=)

(** Get the URI value associated to a given URI id *)
let get_uri (Lxsd.Id_uri i as uri_id) =
  try List.assoc uri_id Xsval.schema.Lxsd.uris with
  | Not_found ->
      Message.error
        (Error.XSV_uri_table_corrupted i)

(** Test if a given URI is in a given list of URI ids, the function uses
    {!get_uri} to retrieve the URI values associated with each URI id *)
let is_in_uri_list uri_id_list uri =
  List.exists
    (fun uri_id ->
      Stringdata.eq uri (get_uri uri_id))
    uri_id_list

(** Get the type declaration of a given id *)
let get_td (Lxsd.Id_td i as td_id) =
  Message.info (Message.INFO_fetching_td i);
  try List.assoc td_id Xsval.schema.Lxsd.tds with
  | Not_found ->
      Message.error
        (Error.XSV_type_table_corrupted i)

(** Get the element declaration of a given id *)
let get_ed (Lxsd.Id_ed i as ed_id) =
  Message.info (Message.INFO_fetching_ed i);
  try List.assoc ed_id Xsval.schema.Lxsd.eds with
  | Not_found ->
      Message.error
        (Error.XSV_element_table_corrupted i)

(** Get the attribute declaration of a given id *)
let get_ad (Lxsd.Id_ad i as ad_id) =
  Message.info (Message.INFO_fetching_ad i);
  try List.assoc ad_id Xsval.schema.Lxsd.ads with
  | Not_found ->
      Message.error
        (Error.XSV_attribute_table_corrupted i)

(** Get the attribute group declaration of a given id *)
let get_agd (Lxsd.Id_agd i as agd_id) =
  Message.info (Message.INFO_fetching_agd i);
  try List.assoc agd_id Xsval.schema.Lxsd.agds with
  | Not_found ->
      Message.error
        (Error.XSV_attribute_group_table_corrupted i)

(** Get the model group declaration of a given id *)
let get_mgd (Lxsd.Id_mgd i as mgd_id) =
  Message.info (Message.INFO_fetching_mgd i);
  try List.assoc mgd_id Xsval.schema.Lxsd.mgds with
  | Not_found ->
      Message.error
        (Error.XSV_model_group_table_corrupted i)

(** {2 Primitive simple type test functions} *)

(** Whitespace pre-processing function *)
let whitespace_processing = function
  | Lxsd.W_replace ->
      (fun sd ->
        Stringdata.whitespace_replace sd)
  | Lxsd.W_collapse ->
      (fun sd ->
        Stringdata.whitespace_collapse sd)
  | Lxsd.W_preserve ->
      (fun sd -> sd)

(** Return the test function of a given primitive type. The test is a
    regular expression test and a test of the value space representation
    if applicable. *)
let simpleType_primitive_tfun = function
  | Lxsd.PT_string ->
      (fun (_env,sd) -> Val_re.test_st_string sd)
  | Lxsd.PT_boolean ->
      (fun (_env,sd) -> Val_re.test_st_boolean sd)
  | Lxsd.PT_decimal ->
      (fun (_env,sd) -> Val_re.test_st_decimal sd)
  | Lxsd.PT_float ->
      (fun (_env,sd) ->
        let b = Val_re.test_st_float sd in
        b &&
        (match Stringdata.to_float sd with
        | None -> false
        | Some f ->
           (* Reducing the value space to single precision
              floating points.

              f = m x 2^e

              where |m| < 2^24 , m integer
                and -149 <= e <= 104

              which we translate into:

              |f| = m' x 2^e'

              where 0.5 <= m' < 1.0
                and -125 <= e' <= 128
            *)
          match classify_float f with
          | FP_normal
          | FP_subnormal
              (* This is only applicable for comparing values

                 ->
              let (m',e') =
                frexp (abs_float f) in
              (compare 0.5 m') <= 0 &&
              (compare m' 1.) < 0 &&
              -125 <= e' &&
              e' <= 128
               *)
          | FP_zero
          | FP_infinite
          | FP_nan ->
              true))
  | Lxsd.PT_double ->
      (fun (_env,sd) ->
        let b = Val_re.test_st_double sd in
        b &&
        (match Stringdata.to_float sd with
        | None -> false
        | Some f ->
           (* double corresponds to OCaml's float

              f = m x 2^e

              where |m| < 2^53 , m integer
                and -1074 <= e <= 971
            *)
          match classify_float f with
          | FP_normal
          | FP_subnormal
          | FP_zero
          | FP_infinite
          | FP_nan ->
              true))
  | Lxsd.PT_duration ->
      (fun (_env,sd) ->
        Val_re.test_st_duration sd)
  | Lxsd.PT_dateTime ->
      (fun (_env,sd) ->
        Val_re.test_st_dateTime sd)
  | Lxsd.PT_time ->
      (fun (_env,sd) ->
        Val_re.test_st_time sd)
  | Lxsd.PT_date ->
      (fun (_env,sd) ->
        Val_re.test_st_date sd)
  | Lxsd.PT_gYearMonth ->
      (fun (_env,sd) ->
        Val_re.test_st_gYearMonth sd)
  | Lxsd.PT_gYear ->
      (fun (_env,sd) ->
        Val_re.test_st_gYear sd)
  | Lxsd.PT_gMonthDay ->
      (fun (_env,sd) ->
        Val_re.test_st_gMonthDay sd)
  | Lxsd.PT_gDay ->
      (fun (_env,sd) ->
        Val_re.test_st_gDay sd)
  | Lxsd.PT_gMonth ->
      (fun (_env,sd) ->
        Val_re.test_st_gMonth sd)
  | Lxsd.PT_hexBinary ->
      (fun (_env,sd) -> Val_re.test_st_hexBinary sd)
  | Lxsd.PT_base64Binary ->
      (fun (_env,sd) -> Val_re.test_st_base64Binary sd)
  | Lxsd.PT_anyURI ->
      (fun (_env,sd) -> Val_re.test_st_anyURI sd)
  | Lxsd.PT_QName ->
      (fun (env,sd) ->
        match Xml_lexer.lex_qname_ns (env.ns,sd) with
        | None -> false
        | Some _ -> true )
  | Lxsd.PT_NOTATION ->
      Message.error Error.XSDL_XSV_LIM_notation_not_implemented

(** Return the test function of a given non primitive built-in
    type. The test is a regular expression test and a test of the
    value space representation if applicable. *)
let simpleType_non_primitive_tfun = function
  | Lxsd.NPT_normalizedString ->
      (fun (_env,sd) -> Val_re.test_st_string sd)
  | Lxsd.NPT_token ->
      (fun (_env,sd) -> Val_re.test_st_string sd)
  | Lxsd.NPT_language ->
      (fun (_env,sd) -> Xml_re.test_language sd)
  | Lxsd.NPT_NMTOKEN ->
      (fun (_env,sd) -> Val_re.test_st_NMTOKEN sd)
  | Lxsd.NPT_NMTOKENS
  | Lxsd.NPT_IDREFS
  | Lxsd.NPT_ENTITIES ->
      Message.error
        (Error.A_function "Val_parser.simpleType_non_primitive_tfun")
  | Lxsd.NPT_Name ->
      (fun (_env,sd) -> Val_re.test_st_Name sd)
  | Lxsd.NPT_NCName ->
      (fun (_env,sd) -> Val_re.test_st_NCName sd)
  | Lxsd.NPT_ID ->
      (* Uniqueness validation is not performed here *)
      (fun (_env,sd) -> Val_re.test_st_NCName sd)
  | Lxsd.NPT_IDREF ->
      (fun (_env,sd) -> Val_re.test_st_NCName sd)
  | Lxsd.NPT_ENTITY ->
      (fun (_env,sd) -> Val_re.test_st_NCName sd)
  | Lxsd.NPT_integer ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_nonPositiveInteger ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_negativeInteger ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_long ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_int ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_short ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_byte ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_nonNegativeInteger ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_unsignedLong ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_unsignedInt ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_unsignedShort ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_unsignedByte ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_positiveInteger ->
      (fun (_env,sd) -> Val_re.test_st_integer sd)
  | Lxsd.NPT_yearMonthDuration ->
      (fun (env,sd) ->
        (simpleType_primitive_tfun Lxsd.PT_duration (env,sd))
          && (Val_re.test_st_yearMonthDuration sd))
  | Lxsd.NPT_dayTimeDuration ->
      (fun (env,sd) ->
        (simpleType_primitive_tfun Lxsd.PT_duration (env,sd))
          && (Val_re.test_st_dayTimeDuration sd))
  | Lxsd.NPT_dateTimeStamp ->
      (fun (env,sd) ->
        (simpleType_primitive_tfun Lxsd.PT_dateTime (env,sd))
          && (Val_re.test_st_explicit_timestamp sd))


(** Double's order relation.

    Both XSD's
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-2-20110721/#double}REF}
    and OCaml's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#6_Floatingpointarithmetic}REF}
    floating-point numbers follow the IEEE 754 standard. The precision
    (single or double precision is checked by the simple type test of
    each floating point XSD types ({!simpleType_primitive_tfun}).
*)
let order_float sd1 sd2 =
  let float_1_opt = Stringdata.to_float sd1 in
  let float_2_opt = Stringdata.to_float sd2 in
  match float_1_opt,float_2_opt with
  | None,_
  | _,None -> None
  | (Some float_1),(Some float_2) ->
      Lib.compare_float float_1 float_2

(** Integer's order relation *)
let order_int sd1 sd2 =
  let int_1_opt = Stringdata.to_int sd1 in
  let int_2_opt = Stringdata.to_int sd2 in
  match int_1_opt,int_2_opt with
  | None,_
  | _,None -> None
  | (Some int_1),(Some int_2) ->
      Some (compare int_1 int_2)

(** {2 Facet test functions} *)

(** Return a function testing if a length is in the given
    boundaries. This function returns [false] if the boundaries are
    not respected. If both the upper- and lower- boundaries are [None]
    an error is emitted, the case should never occur, see
    {!Xsd_lineariser.combine_facets}.  *)
let test_of_length_bounds = function
  | Lxsd.Bounds (None,None) ->
      Message.error (Error.A_function "test_of_length_bounds")
  | Lxsd.Bounds ((Some min),None) ->
      (fun l -> l >= min)
  | Lxsd.Bounds (None,(Some max)) ->
      (fun l -> l <= max)
  | Lxsd.Bounds ((Some min),(Some max)) ->
      (fun l ->
        l >= min && l <= max)

(** Return a test function testing if a value is in the given
    boundaries according to a given value space order function. *)
let test_of_value_bounds order_fun = function
  | Lxsd.Bounds (min_bounds,max_bounds) ->
      let min_tests =
        List.map
          (function
            | Lxsd.Inclusive bound ->
                (fun sd ->
                  match order_fun sd bound with
                  | None ->
                      Lib.KO [(Lxsd.CVC_minInclusive_valid,None)]
                  | Some order ->
                      if order >= 0
                      then Lib.OK [(Lxsd.CVC_minInclusive_valid,None)]
                      else Lib.KO [(Lxsd.CVC_minInclusive_valid,None)])
            | Lxsd.Exclusive bound ->
                (fun sd ->
                  match order_fun sd bound with
                  | None ->
                      Lib.KO [(Lxsd.CVC_minExclusive_valid,None)]
                  | Some order ->
                      if order > 0
                      then Lib.OK [(Lxsd.CVC_minExclusive_valid,None)]
                      else Lib.KO [(Lxsd.CVC_minExclusive_valid,None)]))
          min_bounds
      in
      let max_tests =
        List.map
          (function
            | Lxsd.Inclusive bound ->
                (fun sd ->
                  match order_fun sd bound with
                  | None ->
                      Lib.KO [(Lxsd.CVC_maxInclusive_valid,None)]
                  | Some order ->
                      if order <= 0
                      then Lib.OK [(Lxsd.CVC_maxInclusive_valid,None)]
                      else Lib.KO [(Lxsd.CVC_maxInclusive_valid,None)])
            | Lxsd.Exclusive bound ->
                (fun sd ->
                  match order_fun sd bound with
                  | None ->
                      Lib.KO [(Lxsd.CVC_maxExclusive_valid,None)]
                  | Some order ->
                      if order < 0
                      then Lib.OK [(Lxsd.CVC_maxExclusive_valid,None)]
                      else Lib.KO [(Lxsd.CVC_maxExclusive_valid,None)]))
          max_bounds
      in
      Lib.list_test_all
        (min_tests @ max_tests)


(** Return the list of test functions testing the given facets and a
    length testing function. This function is to be used only by
    {!atomic_facets_tfun} and {!list_facets_tfun}. *)
let facets_tfun_fun length_tfun = function
  | { Lxsd.f_length = f_length;
      Lxsd.f_pattern = re_l;
      Lxsd.f_enumeration = f_enumeration;
      Lxsd.f_value = f_value } ->
        let f_length_funs =
          length_tfun f_length
        in
        let f_pattern_funs =
          List.map
            (function
              | Lxsd.RE_regexp (Lxsd.RegExp re) ->
                  (fun (__env,sd) ->
                    Message.info_at sd (Message.INFO_pattern re);
                    if Stringdata.match_regexp re sd
                    then
                      Lib.OK [(Lxsd.CVC_pattern_valid,Some sd);
                              (Lxsd.CVC_datatype_valid_1,Some sd)]
                    else
                      Lib.KO [(Lxsd.CVC_pattern_valid,Some sd);
                              (Lxsd.CVC_datatype_valid_1,Some sd)])
              | Lxsd.RE_primitive_type p ->
                  let tfun = simpleType_primitive_tfun p in
                  (fun (env,sd) ->
                    if tfun (env,sd)
                    then Lib.OK [((Lxsd.CVC_simple_type_p (Some p)),Some sd)]
                    else Lib.KO [((Lxsd.CVC_simple_type_p (Some p)),Some sd)])
              | Lxsd.RE_non_primitive_type np ->
                  let tfun = simpleType_non_primitive_tfun np in
                  (fun (env,sd) ->
                    if tfun (env,sd)
                    then Lib.OK [(Lxsd.CVC_simple_type_np (Some np),Some sd)]
                    else Lib.KO [(Lxsd.CVC_simple_type_np (Some np),Some sd)]))
            re_l
        in
        let f_enumeration_funs =
          match f_enumeration with
          | None ->
              []
          | Some sd_l ->
              [(fun (_,sd) ->
                if Lib.list_mem (Stringdata.eq sd) sd_l
                then
                  Lib.OK [(Lxsd.CVC_enumeration_valid,Some sd)]
                else
                  Lib.KO [(Lxsd.CVC_enumeration_valid,Some sd)]) ]
        in
        let f_value_funs =
          match f_value with
          | None -> []
          | Some (order,value_bounds) ->
              let order_fun =
                match order with
                | Lxsd.Order_float ->
                    order_float
                | Lxsd.Order_int ->
                    order_int
              in
              let order_test =
                test_of_value_bounds order_fun value_bounds
              in
              [(fun (_,sd) ->
                order_test sd)]
        in
        let tfun =
          Lib.list_test_all
            (f_length_funs @
             f_pattern_funs @
             f_enumeration_funs @
             f_value_funs) in
        (fun (env,sd) ->
          Lib.list_result_combine
            (tfun (env,sd))
            (Lib.OK [(Lxsd.CVC_datatype_valid,Some sd);
                     (Lxsd.CVC_facet_valid,Some sd)]))

(** Return the list of test functions testing an item's facets.
    Instance of {!facets_tfun_fun}. *)
let atomic_facets_tfun =
  let item_length_tfun f_length =
    match f_length with
    | None -> []
    | Some (len_counter,len_constraint) ->
        let length_fun =
          match len_counter with
          | Lxsd.Counter_hex ->
              (fun sd ->
                let l = Stringdata.byte_length sd in
                (* we do not check the remainder as, if it is not zero,
                   {!Val_re.st_hexBinary} would fail *)
                l / 2)
          | Lxsd.Counter_base64 ->
              (fun sd ->
                let l = Stringdata.byte_length sd in
                let n = 3 * l / 4 in
                (* we do not check the remainder as, if it is not zero,
                   {!Val_re.st_base64Binary} would fail *)
                (* TODO this solution is not efficient *)
                if
                  Stringdata.match_regexp (Stringdata.of_string "[^=]*=") sd
                then n - 1
                else if
                  Stringdata.match_regexp (Stringdata.of_string "[^=]*==") sd
                then n - 2
                else n)
          | Lxsd.Counter_char ->
              Stringdata.utf8_length
        in
        let length_test =
          test_of_length_bounds len_constraint
        in
        [(fun (_,sd) ->
          if length_test (length_fun sd)
          then
            Lib.OK [(Lxsd.CVC_length_valid,Some sd)]
          else
            Lib.KO [(Lxsd.CVC_length_valid,Some sd)])]
  in
  facets_tfun_fun item_length_tfun


(** Return the list of test functions testing a list's facets.
    Instance of {!facets_tfun_fun}. The list length testing is
    performed when decomposing the list into items, see
    {!simpleType_tfun}. *)
let list_facets_tfun =
  facets_tfun_fun (fun _ -> [])


(** Return a test function testing the given member and a whitespace
    pre-processing function. *)
let member_tfun whitespace_processing_fun member =
  match member with
  | Lxsd.LSTD_atomic facets ->
      let facets_tfun =
        atomic_facets_tfun facets in
      (fun (env,sd) ->
        let sd_preprocessed =
          whitespace_processing_fun sd in
        Lib.list_result_combine
          (Lib.OK [(Lxsd.CVC_datatype_valid_2_1,Some sd)])
          (facets_tfun (env,sd_preprocessed)))
  | Lxsd.LSTD_list (list_facets,item_facets) ->
      let item_tfun =
        atomic_facets_tfun item_facets in
      let list_tfun =
        list_facets_tfun list_facets in
      let test_list_length =
        match list_facets.Lxsd.f_length with
        | None ->
            (fun _ -> Lib.OK [])
        | Some length_bounds ->
            let test =
              test_of_length_bounds length_bounds in
            (fun i ->
              if test i
              then
                Lib.OK [(Lxsd.CVC_length_valid,None)]
              else
                Lib.KO [(Lxsd.CVC_length_valid,None)])
      in
      (fun (env,sd) ->
        let sd_preprocessed =
          whitespace_processing_fun sd in
        let items =
          Xml_lexer.lex_list sd_preprocessed in
        (* Test result of a list datatype:
           - items' testing result
           - list length's testing result
           - global testing result *)
        Lib.list_result_combine
          (Lib.OK [(Lxsd.CVC_datatype_valid_2_2,Some sd)])
          (Lib.list_result_combine
             (Lib.list_result_combine
                (test_list_length (List.length items))
                (List.fold_left
                   (fun r1 item ->
                     Lib.list_result_combine
                       r1
                       (item_tfun (env,item)))
                   (Lib.OK [])
                   items))
             (list_tfun (env,sd_preprocessed))) )


(** {2 Validation functions} *)


let xsi_attrs_filter attrs =
  match Xsval.xml_configuration.Xml.error_status.Xml.xsi with
  | Xml.XSI_reject_all ->
      if Xml.M_exp.exists
          (fun attr _ ->
            Stringdata.eq Xml_val.xsi_namespace attr.Xml.uri)
          attrs
      then
        Message.error Error.XSV_SEC_xsi_attribute_not_allowed
      else
        attrs
  | Xml.XSI_ignore_schemaLocation_only ->
      let attrs_minus_xsi_schemaLocation =
        if Xml.M_exp.exists
            (fun attr _ ->
              Xml_lib.eq_name
                Xml_val.e_xsi_schemaLocation
                attr)
            attrs
        then
          begin
            Message.warning Error.XSV_SEC_xsi_schemaLocation_ignored;
            Xml.M_exp.remove Xml_val.e_xsi_schemaLocation attrs
          end
        else
          attrs
      in
      if Xml.M_exp.exists
          (fun attr _ ->
            Stringdata.eq Xml_val.xsi_namespace attr.Xml.uri)
          attrs_minus_xsi_schemaLocation
      then
        Message.error Error.XSV_SEC_xsi_attribute_not_allowed
      else
        attrs_minus_xsi_schemaLocation
  | Xml.XSI_ignore_all ->
      if Xml.M_exp.exists
          (fun attr _ ->
            Stringdata.eq Xml_val.xsi_namespace attr.Xml.uri)
          attrs
      then
        begin
          Xml.M_exp.iter
            (fun attr _ ->
              if Stringdata.eq Xml_val.xsi_namespace attr.Xml.uri
              then
                Message.warning_at attr.Xml.name
                  Error.XSV_SEC_xsi_attribute_ignored)
            attrs;
          Xml.M_exp.filter
            (fun attr _ ->
              not (Stringdata.eq Xml_val.xsi_namespace attr.Xml.uri))
            attrs
        end
      else
        attrs

let tests_occurrence = function
  | { Lxsd.min_occurs = min_occurs;
      Lxsd.max_occurs = max_occur_val } ->
        (fun i -> min_occurs <= i ),
        (match max_occur_val with
        | Lxsd.Occur max_occurs ->
            (fun i -> i <= max_occurs)
        | Lxsd.Unbounded ->
            (fun _ -> true) )




(** Return a function testing a content data
    according to a given simpleType declaration *)
let simpleType_tfun std : tfun =
  match std with
  | { Lxsd.std_details =
      { Lxsd.lstd_whitespace = whitespace;
        Lxsd.lstd_members = members } } ->
          let whitespace_processing_fun =
            whitespace_processing whitespace in
          begin
            match members with
            | [] ->
                Message.error Error.A_empty_members_list
            | [ single_member ] ->
                let tfun =
                  member_tfun whitespace_processing_fun single_member in
                (fun (env,sd) ->
                  tfun (env,sd))
            | union_members ->
                (* Union type *)
                let members_tfun =
                  Lib.list_test_choice
                    (Lxsd.CVC_datatype_valid_2_3,None)
                    (List.map
                       (member_tfun whitespace_processing_fun)
                       union_members)
                in
                (fun (env,sd) ->
                  members_tfun (env,sd))
          end


(** Return from a given attribute declaration ad_id the followings:
    - the expected attribute name,
    - a function validating the type of the attribute's value
    - a function validating the constraints on the attribute's value
 *)
let attr_details ad_id =
  match get_ad ad_id with
  | { Lxsd.ad_name = name;
      Lxsd.ad_target = uri_id;
      Lxsd.ad_std = td_id;
      Lxsd.ad_value_constraint = __value_constraint } ->
        let vfun =
          match get_td td_id with
          | Lxsd.CTD _ ->
              Message.error (Error.A_function "Val_parser.attr_details")
          | Lxsd.STD std ->
              simpleType_tfun std in
        { Xml.uri = get_uri uri_id;
          Xml.name = name },
        vfun,
        (Stringdata.eq)

(** Return a function validating a list of contents
    according to a given simpleType declaration *)
let simpleType_vfun std : contents_vfun =
  let tfun =
    simpleType_tfun std
  in
  fun (env,contents) ->
    match contents with
    | [Xml.Leaf sd] ->
        Some
          (combine_cvc_l_inner_vfun
             (fun () -> tfun (env,sd))
             [(Lxsd.CVC_type_3_1_2,Some sd)])
    | [] ->
        Some
          (combine_cvc_l_inner_vfun
             (fun () -> tfun (env,(Stringdata.of_string "")))
             [(Lxsd.CVC_type_3_1_2,None)])
    | [Xml.Node _] | _::_::_ ->
        Some (fun () -> Lib.KO [(Lxsd.CVC_type_3_1_2,None)])


(** Return an function validating a content according to a given
    element declaration.

    Termination:
    - recursive call (via {!validate_ctd_ref}) on the sub-contents of
    the element being validated

 *)
let rec element_vfun ed : element_vfun =
  match ed with
  | { Lxsd.ed_ncname = ed_ncname;
      Lxsd.ed_target = uri_id;
      Lxsd.ed_td = ctd_ref;
      Lxsd.ed_value_constraint = v_c_opt } ->
        let c_vfun =
          validate_ctd_ref ctd_ref in
        (fun (env,content) ->
          match (Xml_lib.next_elt
                   { Xml.uri = get_uri uri_id;
                     Xml.name = ed_ncname } content) with
          | None ->
              None
          | Some (name,attrs_with_xsi,contents) ->
              Message.info_at name (Message.INFO_validating_element name);
              let attrs = xsi_attrs_filter attrs_with_xsi in
              let env' = update_env attrs env in
              let tfun =
                match v_c_opt with
                | None ->
                    (fun () ->
                      Message.info_at name
                        (Message.INFO_testing_element name);
                      c_vfun
                        (env,(name,attrs,(env',contents))))
                | Some (Lxsd.A_default sd) ->
                    begin
                      match contents with
                      | [] ->
                          (fun () ->
                            Message.info_at name
                              (Message.INFO_testing_element name);
                            c_vfun
                              (env,(name,attrs,(env',[Xml.Leaf sd]))))
                      | _ ->
                          (fun () ->
                            Message.info_at name
                              (Message.INFO_testing_element name);
                            c_vfun
                              (env,(name,attrs,(env',contents))))
                    end
                | Some (Lxsd.A_fixed sd_fixed) ->
                    begin
                      match contents with
                      | [] ->
                          (fun () ->
                            Message.info_at name
                              (Message.INFO_testing_element name);
                            c_vfun
                              (env,(name,attrs,(env',[Xml.Leaf sd_fixed]))))
                      | [Xml.Leaf sd] ->
                          if Stringdata.eq sd_fixed sd
                          then
                            (fun () ->
                              Message.info_at name
                                (Message.INFO_testing_element name);
                              c_vfun
                                (env,(name,attrs,(env',contents))))
                          else
                            (fun () -> Lib.KO [(Lxsd.CVC_elt_5_2_2,Some name)])
                      | [Xml.Node _] | _::_::_ ->
                          (fun () ->
                            Message.info_at name
                              (Message.INFO_testing_element name);
                            Lib.KO [(Lxsd.CVC_elt_5_2_2,Some name)])
                    end
              in
              let tfun' =
                fun () ->
                  let res = tfun () in
                  begin
                    match res with
                    | Lib.OK _ ->
                        Message.info_at name (Message.INFO_valid_element name)
                    | Lib.KO _ ->
                        Message.info_at name (Message.INFO_invalid_element name)
                  end;
                  res
              in
              Some (combine_cvc_l_inner_vfun
                      tfun'
                      [(Lxsd.CVC_elt_5_2_1,Some name);
                       (Lxsd.CVC_elt_5_2_2,Some name);
                       (Lxsd.CVC_elt,Some name);
                       (Lxsd.CVC_assess_elt_1,Some name);
                       (Lxsd.CVC_assess_elt,Some name)]))

(** Return an function validating a content according to a given
    wildcard declaration.

    Termination:
    - recursive call (via {!root_vfun}) on sub-elements of
    the element being validated.
 *)
and wildcard_vfun w : element_vfun =
  match w with
  | { Lxsd.w_nc = nc;
      Lxsd.w_pc = process_contents } ->
        let w_tfun =
          match nc with
          | Lxsd.NC_any [] ->
              (fun (_env,_content) ->
                [(Lxsd.CVC_wildcard_namespace_1,None);
                 (Lxsd.CVC_wildcard_namespace,None);
                 (Lxsd.CVC_wildcard_name_1,None);
                 (Lxsd.CVC_wildcard_name,None);
                 (Lxsd.CVC_wildcard_1,None);
                 (Lxsd.CVC_wildcard,None)],
                true)
          | Lxsd.NC_not (uri_id_l,[]) ->
              (fun (_env,content) ->
                [(Lxsd.CVC_wildcard_namespace_2,None);
                 (Lxsd.CVC_wildcard_namespace,None);
                 (Lxsd.CVC_wildcard_name_1,None);
                 (Lxsd.CVC_wildcard_name,None);
                 (Lxsd.CVC_wildcard_1,None);
                 (Lxsd.CVC_wildcard,None)],
                match content with
                | Xml.Leaf _ ->
                    false
                | Xml.Node node ->
                    not
                      (is_in_uri_list
                         uri_id_l
                         (node.Xml.node.Xml.element_name.Xml.uri)))
          | Lxsd.NC_enumeration (uri_id_l,[]) ->
              (fun (_env,content) ->
                [(Lxsd.CVC_wildcard_namespace_3,None);
                 (Lxsd.CVC_wildcard_namespace,None);
                 (Lxsd.CVC_wildcard_name_1,None);
                 (Lxsd.CVC_wildcard_name,None);
                 (Lxsd.CVC_wildcard_1,None);
                 (Lxsd.CVC_wildcard,None)],
                match content with
                | Xml.Leaf _ ->
                    false
                | Xml.Node node ->
                    is_in_uri_list
                      uri_id_l
                      (node.Xml.node.Xml.element_name.Xml.uri))
          | Lxsd.NC_any _
          | Lxsd.NC_not (_,_)
          | Lxsd.NC_enumeration (_,_) ->
              Message.error (Error.LIM_nc_disallowed)
        in
        let w_vfun cvc_l =
          match process_contents with
          | Lxsd.Skip ->
              (fun (_env,_content) ->
                Lib.OK (cvc_l @ [(Lxsd.Skip_wildcard_valid,None);
                                 (Lxsd.CVC_assess_elt_2,None);
                                 (Lxsd.CVC_assess_elt,None)]))
          | Lxsd.Strict ->
              let cvc_strict_l =
                cvc_l @ [(Lxsd.Strict_wildcard_valid,None);
                         (Lxsd.CVC_assess_elt_1,None);
                         (Lxsd.CVC_assess_elt,None)]
              in
              (fun (env,content) ->
                match root_vfun () (env,content) with
                | None ->
                    Lib.KO cvc_strict_l
                | Some inner_vfun ->
                    Lib.list_result_combine
                      (inner_vfun ())
                      (Lib.OK cvc_strict_l))
          | Lxsd.Lax ->
              let cvc_lax_l =
                cvc_l @ [(Lxsd.Lax_wildcard_valid,None);
                         (Lxsd.CVC_assess_elt_3,None);
                         (Lxsd.CVC_assess_elt,None)]
              in
              (fun (env,content) ->
                match root_vfun () (env,content) with
                | None ->
                    Lib.OK cvc_lax_l
                | Some inner_vfun ->
                    Lib.list_result_combine
                      (inner_vfun ())
                      (Lib.OK cvc_lax_l))
        in
        (fun (env,content) ->
          let cvc_l,b = w_tfun (env,content) in
          if b
          then
            (Some (fun () -> w_vfun cvc_l (env,content)))
          else
            None)

(** Return a function validating a list of contents according to a given
    particle declaration.

    Termination:
    - the term's validation function is applied recursively on the
    list of contents (local recursive function [aux]), the list of
    contents decreases in length at each recursion step
 *)
and particle_vfun p : particle_vfun =
  match p with
  | { Lxsd.p_occurrence = occurrence;
      Lxsd.p_term = t } ->
        let cvc,t_vfun =
          (* One term's validation function *)
          match t with
          | Lxsd.T_w w ->
              let w_vfun = wildcard_vfun w in
              (Lxsd.CVC_accept_1,None),
              (fun (env,contents) ->
                match contents with
                | [] -> None
                | content::left_list ->
                    begin
                      match w_vfun (env,content) with
                      | None ->
                          None
                      | Some vfun ->
                          Some (vfun,left_list)
                    end)
          | Lxsd.T_ed ed_id ->
              let e_vfun = element_vfun (get_ed ed_id) in
              (Lxsd.CVC_accept_2,None),
              (fun (env,contents) ->
                match contents with
                | [] ->
                    None
                | content::left_list ->
                    begin
                      match e_vfun (env,content) with
                      | None ->
                          None
                      | Some vfun ->
                          Some (vfun,left_list)
                    end)
          | Lxsd.T_mgd mgd_id ->
              let mg_vfun = modelGroup_vfun (get_mgd mgd_id) in
              (Lxsd.CVC_accept_3,None),
              (fun (env,content) ->
                mg_vfun (env,content))
        in
        let test_min,test_max =
          tests_occurrence occurrence in
        let p_vfun (env,contents) =
          (* Particle's validation function *)
          let rec aux counter vfun env cts =
            if test_max counter
            then
              begin
                match t_vfun (env,cts) with
                | None ->
                    if test_min counter && test_max counter
                    then
                      Some (vfun,cts)
                    else
                      None
                | Some (term_vfun,cts') ->
                    let counter' = counter + 1 in
                    if List.length cts = List.length cts'
                    then
                      (* the contents list did not decrease *)
                      if test_min counter && test_max counter
                      then
                        (* the previous matches was already correct,
                           we do not include this recursion
                           as it is empty *)
                        Some (vfun,cts)
                      else
                        if test_min counter' && test_max counter'
                        then
                          (* the current empty match means the term matches *)
                          Some (vfun,cts')
                        else
                          (* the term does not match *)
                        None
                    else
                      let vfun' =
                        combine_inner_vfun vfun term_vfun in
                      if test_max (counter' + 1)
                      then
                        (* We attempt to recurse if we are not yet
                           reaching the max *)
                        aux counter' vfun' env cts'
                      else
                        if test_min counter' && test_max counter'
                        then
                          (* otherwise we stop *)
                          Some (vfun',cts')
                        else
                          (* we do not match *)
                          None
              end
            else
              None
          in
            aux
              0
              (fun () ->
                Lib.OK [cvc;
                        (Lxsd.CVC_accept,None);
                        (Lxsd.CVC_particle,None)])
              env
              contents
        in
        (fun (env,contents) ->
          p_vfun (env,contents) )


(** Return a list of functions validating contents according to a given
    list of particle declarations.

    Termination:
    - termination of {!particle_vfun}
 *)
and particle_list_vfun p_list : particle_vfun list =
  List.map particle_vfun p_list


(** [attribute_use_vfun au] returns a function validating a map of
    attributes according to an attribute use declaration.  The returned
    function:
    - takes as first argument the map of attributes to validate,
    - takes as second argument the environment and the map of attributes
    that have not yet been validated,
    - returns an option tuple composed of a function testing the value of
    one attribute and the map of attributes still not validated.

    Termination:
    - recursive call to {!simpleType_tfun} via {!attr_details} is
    performed on a sub-structure (the attribute's value)
 *)
and attribute_use_vfun au : Xml.expanded_attributes -> attribute_vfun =
  match au with
  | { Lxsd.au_required = _;
      Lxsd.au_ad = ad_id;
      Lxsd.au_value_constraint =
      Some (Lxsd.A_default _) } ->
        (* Default value provided *)
        let attr_expanded_name,st_vfun,_equality_fun =
          attr_details ad_id in
        (fun attrs ->
          (fun (env,to_visit) ->
            try
              let sd =
                Xml.M_exp.find attr_expanded_name attrs in
              Some ((combine_cvc_l_inner_vfun
                       (fun () -> st_vfun (env,sd))
                       [(Lxsd.CVC_attribute,Some sd)]),
                    (Xml.M_exp.remove attr_expanded_name to_visit))
            with
            | Not_found ->
                Some ((fun () -> Lib.OK []),to_visit) ))
  | { Lxsd.au_required = _;
      Lxsd.au_ad = ad_id;
      Lxsd.au_value_constraint =
      Some (Lxsd.A_fixed sd_fixed) } ->
        (* Fixed value provided *)
        let attr_expanded_name,st_vfun,equality_fun =
          attr_details ad_id in
        (fun attrs ->
          (fun (env,to_visit) ->
            try
              let sd =
                Xml.M_exp.find attr_expanded_name attrs in
              let vfun =
                combine_inner_vfun
                  (fun () ->
                    if equality_fun sd_fixed sd
                    then Lib.OK [(Lxsd.CVC_au,Some sd);
                                 (Lxsd.CVC_attribute,Some sd)]
                    else Lib.KO [(Lxsd.CVC_au,Some sd);
                                 (Lxsd.CVC_attribute,Some sd)])
                  (fun () ->
                    st_vfun (env,sd))
              in
              Some (vfun,
                    (Xml.M_exp.remove attr_expanded_name to_visit))
            with
            | Not_found ->
                Some ((fun () -> Lib.OK []),to_visit) ))
  | { Lxsd.au_required = true;
      Lxsd.au_ad = ad_id;
      Lxsd.au_value_constraint =
      None } ->
        (* Attribute required *)
        let attr_expanded_name,st_vfun,__equality_fun =
          attr_details ad_id in
        (fun attrs ->
          (fun (env,to_visit) ->
            try
              let sd =
                Xml.M_exp.find attr_expanded_name attrs in
              Some ((combine_cvc_l_inner_vfun
                       (fun () -> st_vfun (env,sd))
                       [(Lxsd.CVC_attribute,Some sd)]),
                    (Xml.M_exp.remove attr_expanded_name to_visit))
            with
            | Not_found ->
                None ))
  | { Lxsd.au_required = false;
      Lxsd.au_ad = ad_id;
      Lxsd.au_value_constraint =
      None } ->
        let attr_expanded_name,st_vfun,_equality_fun =
          attr_details ad_id in
        (fun attrs ->
          (fun (env,to_visit) ->
            try
              let sd =
                Xml.M_exp.find attr_expanded_name attrs in
              let to_visit' =
                Xml.M_exp.remove attr_expanded_name to_visit in
              Some ((combine_cvc_l_inner_vfun
                       (fun () -> st_vfun (env,sd))
                       [(Lxsd.CVC_attribute,Some sd)]),
                    to_visit')
            with
            | Not_found ->
                Some ((fun () -> Lib.OK []),to_visit) ))

(** Return a list of functions validating attributes according to a given
    attribute group declarations.

    Termination:
    - termination of {!attribute_use_vfun}
 *)
and attribute_group_vfun agd :
    (Xml.expanded_attributes -> attribute_vfun) list =
  match agd with
  | { Lxsd.agd_uses = Lxsd.AU_resolved au_l;
      Lxsd.agd_w = None } ->
        List.map attribute_use_vfun au_l
  | { Lxsd.agd_uses = Lxsd.AU_resolved _;
      Lxsd.agd_w = Some _ } ->
        Message.error Error.XSDL_XSV_LIM_attribute_group_wildcard_not_implemented

(** Return a function validating attributes according to a given list
    of attribute group declaration ids. The returned function when
    applied on a map of attributes returns an option pair composed of
    a function testing the values of the attributes matching the
    declarations and a map of the attributes that are yet to be
    validated.

    Termination:
    - termination of {!attribute_group_vfun}
 *)
and agd_id_list_vfun agd_id_l : attribute_vfun =
  let au_vfun_list =
    List.flatten
      (List.map
         (function agd_id ->
           attribute_group_vfun (get_agd agd_id))
         agd_id_l)
  in
  let vfun (env,attrs) =
    List.fold_left
      (fun res au_vfun ->
        match res with
        | None -> None
        | Some (f1,to_visit) ->
            match au_vfun attrs (env,to_visit) with
            | None -> None
            | Some (f2,to_visit') ->
                Some ((combine_inner_vfun f1 f2),to_visit'))
      (Some ((fun () -> Lib.OK[]),attrs))
      au_vfun_list
  in
  (fun (env,attrs) ->
    vfun (env,attrs))

(** Return a function validating a map of remaining attributes
    according to a given wildcard declaration.

    Not implemented.
 *)
and attribute_wildcard_vfun _w : 'a =
  (* when implementing attribute_wildcard_vfun, we should use it
     to validate the attributes associated to an element of type
     anyType, see {!validate_ctd_ref} *)
  Message.error Error.XSDL_XSV_LIM_attribute_group_wildcard_not_implemented


(** Return a function validating a map of attributes according to a
    given list of attribute group declaration ids and to a wildcard
    declaration. The function checks that all the attributes were
    treated by calling {!Xml_lib.has_only_xml_xmlns_attrs}.

    Termination:

    - termination of ({!agd_id_list_vfun}) which eventually calls
    {!simpleType_tfun} on the attributes values (see
    {!attribute_use_vfun}).
 *)
and attributes_vfun (agd_id_l,w_opt) : attributes_vfun =
  let attrs_vfun =
    agd_id_list_vfun agd_id_l
  in
  let w_vfun (env,remainder_attrs0) =
    let remainder_attrs1 =
    match w_opt with
    | None ->
        remainder_attrs0
    | Some w ->
        attribute_wildcard_vfun w (env,remainder_attrs0)
    in
    if Xml_lib.has_only_xml_xmlns_attrs remainder_attrs1
    then
      Some
        (fun () -> Lib.OK [(Lxsd.CVC_assess_attr,None)])
    else
      Some
        (fun () -> Lib.KO [(Lxsd.CVC_assess_attr,None)])
  in
  let vfun (env,attrs) =
    match attrs_vfun (env,attrs) with
    | None -> None
    | Some (f1,remainder_attrs) ->
        match w_vfun (env,remainder_attrs) with
        | None -> None
        | Some f2 ->
            Some (combine_inner_vfun f1 f2)
  in
  (fun (env,attrs) ->
    vfun (env,attrs))

(** Return a function validating contents according to
    of a model group declaration.

    Termination:
    - termination of {!all_contents}
    - termination of {!choice_contents}
    - termination of {!sequence_contents}
    - termination of {!particle_list_vfun}
*)
and modelGroup_vfun mgd : particle_vfun =
  match mgd with
  | Lxsd.MG_all p_l ->
      let element_p_l,wildcard_p_l =
        List.partition
          (fun { Lxsd.p_occurrence = _;
                 Lxsd.p_term = t } ->
                   match t with
                   | Lxsd.T_ed _ -> true
                   | Lxsd.T_w _ -> false
                   | Lxsd.T_mgd (Lxsd.Id_mgd i) ->
                       Message.error
                         (Error.XSV_model_group_table_corrupted i))
          p_l in
      all_contents
        (particle_list_vfun element_p_l)
        (particle_list_vfun wildcard_p_l)
  | Lxsd.MG_choice p_l ->
      choice_contents (particle_list_vfun p_l)
  | Lxsd.MG_sequence p_l ->
      sequence_contents (particle_list_vfun p_l)



(** Return a function validating a list of contents according to a
    given contentType.

    Termination:
    - termination of {!particle_vfun}
*)
and contents_vfun ct : contents_vfun =
  match ct with
  | Lxsd.CT_anyType ->
      let p_vfun =

        (* in
           {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#builtin-ctd}REF}
           anyType is presented as a mixed sequence containing a sequence
           of 0-unbounded 'any' element, we represent it here as a mixed
           'any' element with 0-unbounded occurrence *)
        particle_vfun { Lxsd.p_occurrence =
                        { Lxsd.min_occurs = 0;
                          Lxsd.max_occurs = Lxsd.Unbounded };
                        Lxsd.p_term = Lxsd.T_w
                          {Lxsd.w_nc = Lxsd.NC_any [];
                           Lxsd.w_pc = Lxsd.Lax } }
      in
      combine_cvc_l_contents_vfun
        (fun (env,contents) ->
          let contents =
            Xml_lib.clear_leafs contents in
          match p_vfun (env,contents) with
          | None -> None
          | Some (f,[]) -> Some f
          | Some _ -> None)
        [(Lxsd.AnyType_valid,None)]
  | Lxsd.CT_empty ->
      combine_cvc_l_contents_vfun
        (fun (env,contents) ->
          let contents =
            Xml_lib.clear_space_leafs env.space contents in
          match contents with
          | [] -> Some (fun () -> Lib.OK [])
          | _ ->  None )
        [(Lxsd.CVC_complex_type_1_1,None)]
  | Lxsd.CT_simple td_id ->
      let st_vfun =
        match get_td td_id with
        | Lxsd.CTD _ ->
            Message.error (Error.A_function "Val_parser.complexType_vfun simpleContent")
        | Lxsd.STD std ->
            simpleType_tfun std
      in
      combine_cvc_l_contents_vfun
        (fun (env,contents) ->
          match contents with
          | [Xml.Leaf sd] ->
              Some (fun () -> st_vfun (env,sd))
          | [] ->
              Some (fun () ->
                st_vfun (env,(Stringdata.of_string "")))
          | [Xml.Node _]
          | _::_::_ ->
              None )
        [(Lxsd.CVC_complex_type_1_2,None)]
  | Lxsd.CT_element_only p ->
      let p_vfun =
        particle_vfun p in
      combine_cvc_l_contents_vfun
      (fun (env,contents) ->
        let contents =
          Xml_lib.clear_space_leafs env.space contents in
        match p_vfun (env,contents) with
        | None -> None
        | Some (f,[]) -> Some f
        | Some _ -> None)
        [(Lxsd.CVC_complex_type_1_3,None);
         (Lxsd.CVC_complex_type_1_4,None)]
  | Lxsd.CT_mixed p ->
      let p_vfun =
        particle_vfun p in
      combine_cvc_l_contents_vfun
        (fun (env,contents) ->
        let contents =
          Xml_lib.clear_leafs contents in
        match p_vfun (env,contents) with
        | None -> None
        | Some (f,[]) -> Some f
        | Some _ -> None)
        [(Lxsd.CVC_complex_type_1_4,None)]


(** Return a function validating a list of children (a map of
    attributes and a list of contents) according to a given complexType
    declaration.

    Termination:
    - termination of {!attributes_vfun}
    - termination of {!contents_vfun}
 *)
and complexType_vfun ctd : children_vfun =
  match ctd with
  | { Lxsd.ctd_au = agd_id_l;
      Lxsd.ctd_aw = w_opt;
      Lxsd.ctd_ct = ct } ->
        let ct_vfun =
          contents_vfun ct
        in
        let vfun =
          let attrs_vfun =
            attributes_vfun (agd_id_l,w_opt)
          in
          combine_cvc_l_a_vfun
            attrs_vfun
            [(Lxsd.CVC_complex_type_2,None);
             (Lxsd.CVC_complex_type_3,None);
             (Lxsd.CVC_complex_type_4,None)]
            ct_vfun
        in
        (fun (env,(attrs,contents)) ->
          match vfun (env,(attrs,contents)) with
          | None -> None
          | Some f ->
              Some (combine_cvc_l_inner_vfun
                      f
                      [(Lxsd.CVC_complex_type,None)]))



(** Return a function validating a list of children (a map of
    attributes and a list of contents) according to a given type
    declaration.

    Termination:
    - termination of {!complexType_vfun}
*)
and type_vfun td : children_vfun =
      match td with
      | Lxsd.STD std ->
          let vfun =
            combine_cvc_l_contents_vfun
              (simpleType_vfun std)
              [(Lxsd.CVC_type_3_1_1,None);
               (Lxsd.CVC_type_3_1,None);
               (Lxsd.CVC_type,None)]
          in
          (fun (env,(attrs,contents)) ->
            if Xml_lib.has_only_xml_xmlns_attrs attrs
            then
              vfun (env,contents)
            else
              Some (fun () ->
                Lib.KO [(Lxsd.CVC_type_3_1_1,None);
                        (Lxsd.CVC_type_3_1,None);
                        (Lxsd.CVC_type,None)]))
      | Lxsd.CTD ctd ->
          let vfun =
            combine_cvc_l_children_vfun
              (complexType_vfun ctd)
              [(Lxsd.CVC_type_3_2,None);
               (Lxsd.CVC_type,None)]
          in
          (fun (env,(attrs,contents)) ->
            vfun (env,(attrs,contents)))

(** Return a function validating an element content according to a
    reference to a complexType.

    Termination:
    - termination of {!type_vfun}
 *)
and validate_ctd_ref ctd_ref : element_tfun =
  function (__env,(name,attrs,(env',contents))) ->
    let cvc =  [(Lxsd.CVC_complex_content_1,Some name);
                (Lxsd.CVC_complex_content,Some name)]
    in
    match ctd_ref with
    | Lxsd.CTD_anyType ->
        (* when implementing attribute_wildcard_vfun, we should use it
         to validate the attributes associated to an element of type
         anyType *)
        let attrs_vfun =
          (fun (__env,__attrs) -> Some (fun () -> Lib.OK []))
        in
        let ct_vfun =
          contents_vfun Lxsd.CT_anyType
        in
        let vfun =
          combine_cvc_l_a_vfun
            attrs_vfun
            []
            ct_vfun in
        begin
          match vfun (env',(attrs,contents)) with
          | None ->
              Lib.KO cvc
          | Some vfun ->
              Lib.list_result_combine
                (vfun ())
                (Lib.OK cvc)
        end
    | Lxsd.CTD_td_ref td_id ->
        begin
          let td = get_td td_id in
          match type_vfun td (env',(attrs,contents)) with
          | None ->
              Lib.KO cvc
          | Some vfun ->
              Lib.list_result_combine
                (vfun ())
                (Lib.OK cvc)
        end


(** Return a function validating an element content according to the
    root element declarations of the XSD grammar.

    Termination:
    - termination of {!element_vfun}
*)
and root_vfun () : element_vfun =
  let root_choice_funs =
    List.map
      element_vfun
      (Lib.list_assocs
        eq_id
         Xsval.schema.Lxsd.roots
         Xsval.schema.Lxsd.eds)
  in
  (fun (env,content) ->
    match content with
    | Xml.Leaf _ ->
        None
    | Xml.Node _ ->
        Lib.choice
          root_choice_funs
          (env,content))

(** Validate a given XML abstract document according to the XSD grammar. *)
let validate_schema : tree_tfun = function
  | { Xml.declaration = _;
      Xml.root = { Xml.node =
                   { Xml.element_name = root_name;
                     Xml.attributes = _ };
                   Xml.contents = _ } as root_node
    } ->
      if not (Stringdata.eq
                root_name.Xml.uri
                (get_uri Xsval.schema.Lxsd.target))
      then
        Message.error Error.XSV_uri_not_matching;
      let env = { ns = Xml_ns.default;
                  space = Xml.Default } in
      match root_vfun () (env,Xml.Node root_node) with
      | None ->
          Lib.KO [(Lxsd.Schema_root_valid,None)]
      | Some inner_vfun ->
          Lib.list_result_labelise
            (inner_vfun ())
            [(Lxsd.Schema_root_valid,None)]
