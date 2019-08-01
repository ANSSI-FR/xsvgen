(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_generator.ml 1692 2012-05-31 09:47:17Z maarek $ *)

(** {1 Code generator for lambda-XSD}

    The code generator for lambda-XSD provides functions that
    transform a value of the types defined in {!Lxsd} into strings
    containing an OCaml source code defining the value.
 *)

let gen_Stringdata f s =
  Format.fprintf f "%s%s%s"
    "Stringdata.of_string \""
    (Stringdata.to_code_escaped_string s)
    "\""


let xml s =
  "Xml." ^ s

let lxsd s =
  "Lxsd." ^ s

let rec gen_list f (func, l, sep) =
  match l with
  | [] -> ()
  | [h] ->
      Format.fprintf f "%a"
        func h
  | h :: t ->
      Format.fprintf f "%a%s@ %a"
        func h
        sep
        gen_list (func, t, sep)

let wrap fprintf v =
  fun f () -> fprintf f v

let gen_Int f i =
  Format.fprintf f "%i" i

let gen_Bool f b =
  Format.fprintf f "%s"
    (if b then "true" else "false")

let gen_Param f wrapped_fprintf =
  Format.fprintf f "@[%a@]"
    wrapped_fprintf ()

let gen_Record_field f (label,wrapped_fprintf) =
  Format.fprintf f "@[<hv>%s =@,@[ %a@]@]"
    label
    wrapped_fprintf ()

let gen_Record f (record_fields) =
  Format.fprintf f "{ @[<hv>%a }@]"
    gen_list (gen_Record_field,record_fields,";")

let gen_List f (fprintf,l) =
  Format.fprintf f "[ @[<hv>%a ]@]"
    gen_list (fprintf,l,";")

let gen_Constr f = function
  | (constructor,[]) ->
      Format.fprintf f "%s@ "
        constructor
  | (constructor,params) ->
      Format.fprintf f "%s@ (@[%a@])"
        constructor
        gen_list (gen_Param,params,",")

let gen_Tuple f = function
  | [] | [_] ->
      Message.error (Error.A_function "Code_generator.gen_tuple")
  | params ->
      Format.fprintf f "(@[%a@])"
        gen_list (gen_Param,params,",")


let gen_Pair f ((fprintf1,fprintf2),(val1,val2)) =
  Format.fprintf f "(@[<hv>%a,@ %a@])"
    fprintf1 val1
    fprintf2 val2

let gen_Option f (fprintf,opt) =
  match opt with
  | None ->
      Format.fprintf f "%s"
        "None"
  | Some value  ->
      Format.fprintf f "%s@ (@[%a@])"
        "Some"
        fprintf value

let gen_id f (id_name,id) =
  Format.fprintf f "(%s%s %i)"
    (lxsd "Id_")
    id_name
    id

let gen_uri_id f (Lxsd.Id_uri uri_id) =
  gen_id f ("uri",uri_id)

let gen_ed_id f (Lxsd.Id_ed ed_id) =
  gen_id f ("ed",ed_id)

let gen_td_id f (Lxsd.Id_td td_id) =
  gen_id f ("td",td_id)

let gen_ad_id f (Lxsd.Id_ad ad_id) =
  gen_id f ("ad",ad_id)

let gen_agd_id f (Lxsd.Id_agd agd_id) =
  gen_id f ("agd",agd_id)

let gen_mgd_id f (Lxsd.Id_mgd mgd_id) =
  gen_id f ("mgd",mgd_id)

let gen_target f target =
  gen_uri_id f target

let gen_uri f (uri_id,uri_sd) =
  gen_Pair f ((gen_uri_id,gen_Stringdata),(uri_id,uri_sd))

let gen_uris f uris =
  gen_List f (gen_uri,uris)


let gen_regexp f = function
  | Lxsd.RegExp sd ->
      gen_Constr f (lxsd "RegExp",[wrap gen_Stringdata sd])

let gen_whitespace f = function
  | Lxsd.W_preserve -> gen_Constr f (lxsd "W_preserve",[])
  | Lxsd.W_replace -> gen_Constr f (lxsd "W_replace",[])
  | Lxsd.W_collapse -> gen_Constr f (lxsd "W_collapse",[])


let gen_primitive f = function
  | Lxsd.PT_string ->
      gen_Constr f (lxsd "PT_string",[])
  | Lxsd.PT_boolean ->
      gen_Constr f (lxsd "PT_boolean",[])
  | Lxsd.PT_decimal ->
      gen_Constr f (lxsd "PT_decimal",[])
  | Lxsd.PT_float ->
      gen_Constr f (lxsd "PT_float",[])
  | Lxsd.PT_double ->
      gen_Constr f (lxsd "PT_double",[])
  | Lxsd.PT_duration ->
      gen_Constr f (lxsd "PT_duration",[])
  | Lxsd.PT_dateTime ->
      gen_Constr f (lxsd "PT_dateTime",[])
  | Lxsd.PT_time ->
      gen_Constr f (lxsd "PT_time",[])
  | Lxsd.PT_date ->
      gen_Constr f (lxsd "PT_date",[])
  | Lxsd.PT_gYearMonth ->
      gen_Constr f (lxsd "PT_gYearMonth",[])
  | Lxsd.PT_gYear ->
      gen_Constr f (lxsd "PT_gYear",[])
  | Lxsd.PT_gMonthDay ->
      gen_Constr f (lxsd "PT_gMonthDay",[])
  | Lxsd.PT_gDay ->
      gen_Constr f (lxsd "PT_gDay",[])
  | Lxsd.PT_gMonth ->
      gen_Constr f (lxsd "PT_gMonth",[])
  | Lxsd.PT_hexBinary ->
      gen_Constr f (lxsd "PT_hexBinary",[])
  | Lxsd.PT_base64Binary ->
      gen_Constr f (lxsd "PT_base64Binary",[])
  | Lxsd.PT_anyURI ->
      gen_Constr f (lxsd "PT_anyURI",[])
  | Lxsd.PT_QName ->
      gen_Constr f (lxsd "PT_QName",[])
  | Lxsd.PT_NOTATION ->
      gen_Constr f (lxsd "PT_NOTATION",[])

let gen_non_primitive f = function
  | Lxsd.NPT_normalizedString ->
      gen_Constr f (lxsd "NPT_normalizedString",[])
  | Lxsd.NPT_token ->
      gen_Constr f (lxsd "NPT_token",[])
  | Lxsd.NPT_language ->
      gen_Constr f (lxsd "NPT_language",[])
  | Lxsd.NPT_IDREFS ->
      gen_Constr f (lxsd "NPT_IDREFS",[])
  | Lxsd.NPT_ENTITIES ->
      gen_Constr f (lxsd "NPT_ENTITIES",[])
  | Lxsd.NPT_NMTOKEN ->
      gen_Constr f (lxsd "NPT_NMTOKEN",[])
  | Lxsd.NPT_NMTOKENS ->
      gen_Constr f (lxsd "NPT_NMTOKENS",[])
  | Lxsd.NPT_Name ->
      gen_Constr f (lxsd "NPT_Name",[])
  | Lxsd.NPT_NCName ->
      gen_Constr f (lxsd "NPT_NCName",[])
  | Lxsd.NPT_ID ->
      gen_Constr f (lxsd "NPT_ID",[])
  | Lxsd.NPT_IDREF ->
      gen_Constr f (lxsd "NPT_IDREF",[])
  | Lxsd.NPT_ENTITY ->
      gen_Constr f (lxsd "NPT_ENTITY",[])
  | Lxsd.NPT_integer ->
      gen_Constr f (lxsd "NPT_integer",[])
  | Lxsd.NPT_nonPositiveInteger ->
      gen_Constr f (lxsd "NPT_nonPositiveInteger",[])
  | Lxsd.NPT_negativeInteger ->
      gen_Constr f (lxsd "NPT_negativeInteger",[])
  | Lxsd.NPT_long ->
      gen_Constr f (lxsd "NPT_long",[])
  | Lxsd.NPT_int ->
      gen_Constr f (lxsd "NPT_int",[])
  | Lxsd.NPT_short ->
      gen_Constr f (lxsd "NPT_short",[])
  | Lxsd.NPT_byte ->
      gen_Constr f (lxsd "NPT_byte",[])
  | Lxsd.NPT_nonNegativeInteger ->
      gen_Constr f (lxsd "NPT_nonNegativeInteger",[])
  | Lxsd.NPT_unsignedLong ->
      gen_Constr f (lxsd "NPT_unsignedLong",[])
  | Lxsd.NPT_unsignedInt ->
      gen_Constr f (lxsd "NPT_unsignedInt",[])
  | Lxsd.NPT_unsignedShort ->
      gen_Constr f (lxsd "NPT_unsignedShort",[])
  | Lxsd.NPT_unsignedByte ->
      gen_Constr f (lxsd "NPT_unsignedByte",[])
  | Lxsd.NPT_positiveInteger ->
      gen_Constr f (lxsd "NPT_positiveInteger",[])
  | Lxsd.NPT_yearMonthDuration ->
      gen_Constr f (lxsd "NPT_yearMonthDuration",[])
  | Lxsd.NPT_dayTimeDuration ->
      gen_Constr f (lxsd "NPT_dayTimeDuration",[])
  | Lxsd.NPT_dateTimeStamp ->
      gen_Constr f (lxsd "NPT_dateTimeStamp",[])



let gen_length_counter f = function
  | Lxsd.Counter_hex ->
      gen_Constr f
        (lxsd "Counter_hex",[])
  | Lxsd.Counter_base64 ->
      gen_Constr f
        (lxsd "Counter_base64",[])
  | Lxsd.Counter_char ->
      gen_Constr f
        (lxsd "Counter_char",[])

let gen_order_fun f = function
  | Lxsd.Order_float ->
      gen_Constr f
        (lxsd "Order_float",[])
  | Lxsd.Order_int ->
      gen_Constr f
        (lxsd "Order_int",[])

let gen_length_bound f i_opt =
  gen_Option f (gen_Int,i_opt)

let gen_value_bound f = function
  | Lxsd.Inclusive (sd) ->
      gen_Constr f
        (lxsd "Inclusive",[(wrap gen_Stringdata sd)])
  | Lxsd.Exclusive (sd) ->
      gen_Constr f
        (lxsd "Exclusive",[(wrap gen_Stringdata sd)])

let gen_value_bound_list f l =
  gen_List f (gen_value_bound,l)

let gen_bounds f (gen_bound,Lxsd.Bounds (b_min,b_max)) =
  gen_Constr f
    (lxsd "Bounds",[(wrap gen_bound b_min);
                    (wrap gen_bound b_max)])

let gen_length_bounds f length_bounds =
  gen_bounds f (gen_length_bound,length_bounds)

let gen_value_bounds f value_bounds =
  gen_bounds f (gen_value_bound_list,value_bounds)

let gen_item_length f (length_counter,length_bounds) =
  gen_Tuple f
    [(wrap gen_length_counter length_counter);
     (wrap gen_length_bounds length_bounds)]

let gen_list_length f length_bounds =
  gen_length_bounds f length_bounds

let gen_f_value_val f (order_fun,value_bounds) =
  gen_Tuple f
    [(wrap gen_order_fun order_fun);
     (wrap gen_value_bounds value_bounds)]

let gen_Stringdata_list f l =
  gen_List f (gen_Stringdata,l)


let gen_ext_regexp f = function
  | Lxsd.RE_regexp re ->
      gen_Constr f (lxsd "RE_regexp",[(wrap gen_regexp re)])
  | Lxsd.RE_primitive_type p ->
      gen_Constr f (lxsd "RE_primitive_type",[(wrap gen_primitive p)])
  | Lxsd.RE_non_primitive_type p ->
      gen_Constr f (lxsd "RE_non_primitive_type",[(wrap gen_non_primitive p)])

let gen_facets f = function
  | gen_length_fun,
    { Lxsd.f_length = f_length;
      Lxsd.f_pattern = re_l;
      Lxsd.f_enumeration = sd_l;
      Lxsd.f_value = f_value } ->
        gen_Record f
          [(lxsd "f_length",
            (wrap gen_Option (gen_length_fun,f_length)));
           (lxsd "f_pattern",
            (wrap gen_List
                 (gen_ext_regexp,re_l)));
           (lxsd "f_enumeration",
            (wrap gen_Option
               (gen_Stringdata_list,sd_l)));
           (lxsd "f_value",
             (wrap gen_Option
                 (gen_f_value_val,f_value)) )]

let gen_item_facets f facets =
  gen_facets f (gen_item_length,facets)

let gen_list_facets f facets =
  gen_facets f (gen_list_length,facets)

let gen_lstd_member f = function
  | Lxsd.LSTD_atomic facets ->
      gen_Constr f (lxsd "LSTD_atomic",[wrap gen_item_facets facets])
  | Lxsd.LSTD_list (list_facets,item_facets) ->
      gen_Constr f (lxsd "LSTD_list",[wrap gen_list_facets list_facets;
                                      wrap gen_item_facets item_facets])

let gen_std_details f = function
  | { Lxsd.lstd_whitespace = whitespace;
      Lxsd.lstd_members = lstd_members } ->
        gen_Record f
          [ (lxsd "lstd_whitespace",(wrap gen_whitespace whitespace));
            (lxsd "lstd_members",(wrap
                                    gen_List
                                    (gen_lstd_member,lstd_members))) ]

let gen_std f = function
  | { Lxsd.std_details = details } ->
        gen_Record f
          [ (lxsd "std_details",(wrap gen_std_details details)) ]


let gen_expanded_name f = function
  | { Xml.uri = uri;
      Xml.name = name } ->
        gen_Record f
          [ (xml "uri",(wrap gen_Stringdata uri));
            (xml "name",(wrap gen_Stringdata name)) ]

let gen_process_contents f = function
  | Lxsd.Skip ->
      gen_Constr f (lxsd "Skip",[])
  | Lxsd.Strict ->
      gen_Constr f (lxsd "Strict",[])
  | Lxsd.Lax ->
      gen_Constr f (lxsd "Lax",[])

let gen_nc_disallowed f = function
  | Lxsd.NC_qname expanded_name ->
      gen_Constr f (lxsd "NC_qname",[wrap gen_expanded_name expanded_name])
  | Lxsd.NC_defined ->
      gen_Constr f (lxsd "NC_defined",[])
  | Lxsd.NC_sibling ->
      gen_Constr f (lxsd "NC_sibling",[])


let gen_nc f = function
  | Lxsd.NC_any nc_d_l ->
      gen_Constr f (lxsd "NC_any",
                    [(wrap gen_List (gen_nc_disallowed,nc_d_l))])
  | Lxsd.NC_enumeration (uri_id_l,nc_d_l) ->
      gen_Constr f (lxsd "NC_enumeration",
                    [(wrap gen_List (gen_uri_id,uri_id_l));
                     (wrap gen_List (gen_nc_disallowed,nc_d_l))])
  | Lxsd.NC_not (uri_id_l,nc_d_l) ->
      gen_Constr f (lxsd "NC_not",
                    [(wrap gen_List (gen_uri_id,uri_id_l));
                     (wrap gen_List (gen_nc_disallowed,nc_d_l))])

let gen_max_occurrence_val f = function
  | Lxsd.Occur i ->
      gen_Constr f (lxsd "Occur",[wrap gen_Int i])
  | Lxsd.Unbounded ->
      gen_Constr f (lxsd "Unbounded",[])


let gen_occurrence f = function
  | { Lxsd.min_occurs = min;
      Lxsd.max_occurs = max } ->
        gen_Record f [ (lxsd "min_occurs",(wrap gen_Int min));
                       (lxsd "max_occurs",(wrap gen_max_occurrence_val max)) ]

let gen_w f = function
  | { Lxsd.w_nc = nc;
      Lxsd.w_pc = pc } ->
        gen_Record f
          [ (lxsd "w_nc",(wrap gen_nc nc));
            (lxsd "w_pc",(wrap gen_process_contents pc)) ]


let gen_t f = function
  | Lxsd.T_ed ed_id ->
      gen_Constr f (lxsd "T_ed",[wrap gen_ed_id ed_id])
  | Lxsd.T_mgd mgd_id ->
      gen_Constr f (lxsd "T_mgd",[wrap gen_mgd_id mgd_id])
  | Lxsd.T_w w ->
      gen_Constr f (lxsd "T_w",[wrap gen_w w])

let gen_p f = function
  | { Lxsd.p_occurrence = occurrence;
      Lxsd.p_term = t } ->
        gen_Record f [ (lxsd "p_occurrence",(wrap gen_occurrence occurrence));
                       (lxsd "p_term",(wrap gen_t t)) ]


let gen_ct f = function
  | Lxsd.CT_anyType ->
      gen_Constr f (lxsd "CT_anyType",[])
  | Lxsd.CT_empty ->
      gen_Constr f (lxsd "CT_empty",[])
  | Lxsd.CT_simple td_id ->
      gen_Constr f (lxsd "CT_simple",[wrap gen_td_id td_id])
  | Lxsd.CT_element_only p ->
      gen_Constr f (lxsd "CT_element_only",
                    [wrap gen_p p])
  | Lxsd.CT_mixed p ->
      gen_Constr f (lxsd "CT_mixed",
                    [wrap gen_p p])

let gen_value_constraint f = function
  | Lxsd.A_default sd ->
      gen_Constr f (lxsd "A_default",[wrap gen_Stringdata sd])
  | Lxsd.A_fixed sd ->
      gen_Constr f (lxsd "A_fixed",[wrap gen_Stringdata sd])

let gen_au f = function
  | { Lxsd.au_required = required;
      Lxsd.au_ad = ad_id;
      Lxsd.au_value_constraint = v_c_opt } ->
        gen_Record f [ (lxsd "au_required",(wrap gen_Bool required));
                       (lxsd "au_ad",(wrap gen_ad_id ad_id));
                       (lxsd "au_value_constraint",(wrap gen_Option (gen_value_constraint,v_c_opt))) ]

let gen_au_resolved f = function
  | Lxsd.AU_resolved au_l ->
      gen_Constr f (lxsd "AU_resolved",[wrap gen_List (gen_au,au_l)])


let gen_ctd_ref f = function
  | Lxsd.CTD_anyType ->
      gen_Constr f (lxsd "CTD_anyType",[])
  | Lxsd.CTD_td_ref td_id ->
      gen_Constr f (lxsd "CTD_td_ref",[wrap gen_td_id td_id])

let gen_ctd f = function
  | { Lxsd.ctd_au = agd_id_list;
      Lxsd.ctd_aw = w_opt;
      Lxsd.ctd_ct = ct } ->
        gen_Record f [ (lxsd "ctd_au",(wrap gen_List
                                         (gen_agd_id,agd_id_list)));
                       (lxsd "ctd_aw",(wrap gen_Option (gen_w,w_opt)));
                       (lxsd "ctd_ct",(wrap gen_ct ct))]

let gen_td f = function
  | Lxsd.STD std ->
      gen_Constr f (lxsd "STD",[wrap gen_std std])
  | Lxsd.CTD ctd ->
      gen_Constr f (lxsd "CTD",[wrap gen_ctd ctd])

let gen_td_pair f (td_id,td) =
  gen_Pair f ((gen_td_id,gen_td),(td_id,td))

let gen_tds f tds =
  gen_List f (gen_td_pair, tds)

let gen_ad f = function
  | { Lxsd.ad_name = sd;
      Lxsd.ad_target = uri_id;
      Lxsd.ad_std = td_id;
      Lxsd.ad_value_constraint = v_c_opt } ->
        gen_Record f [ (lxsd "ad_name",(wrap gen_Stringdata sd));
                       (lxsd "ad_target",(wrap gen_uri_id uri_id));
                       (lxsd "ad_std",(wrap gen_td_id td_id));
                       (lxsd "ad_value_constraint",(wrap gen_Option (gen_value_constraint,v_c_opt))) ]

let gen_ad_pair f (ad_id,ad) =
  gen_Pair f ((gen_ad_id,gen_ad),(ad_id,ad))

let gen_ads f ads =
  gen_List f (gen_ad_pair, ads)

let gen_ed_ncname f ed_ncname_sd =
  gen_Stringdata f ed_ncname_sd

let gen_ed_target f ed_target =
  gen_target f ed_target

let gen_ed_td f ed_td =
  gen_td_id f ed_td

let gen_ed f = function
  | { Lxsd.ed_ncname = ed_ncname;
      Lxsd.ed_target = ed_target;
      Lxsd.ed_td = ed_td_ref;
      Lxsd.ed_value_constraint = v_c_opt } ->
        gen_Record f [ (lxsd "ed_ncname",(wrap gen_ed_ncname ed_ncname));
                       (lxsd "ed_target",(wrap gen_ed_target ed_target));
                       (lxsd "ed_td",(wrap gen_ctd_ref ed_td_ref));
                       (lxsd "ed_value_constraint",
                        (wrap gen_Option (gen_value_constraint,v_c_opt))) ]

let gen_ed_pair f (id_ed,ed) =
  gen_Pair f ((gen_ed_id,gen_ed), (id_ed,ed))

let gen_eds f eds =
  gen_List f (gen_ed_pair, eds)

let gen_agd f = function
  | { Lxsd.agd_uses = au_resolved;
      Lxsd.agd_w = w_opt } ->
        gen_Record f [ (lxsd "agd_uses",
                        (wrap gen_au_resolved au_resolved));
                       (lxsd "agd_w",
                        (wrap gen_Option (gen_w,w_opt))) ]

let gen_agd_pair f (id_agd,agd) =
  gen_Pair f ((gen_agd_id,gen_agd), (id_agd,agd))

let gen_agds f agds =
  gen_List f (gen_agd_pair, agds)

let gen_mgd f = function
  | Lxsd.MG_all p_l ->
      gen_Constr f (lxsd "MG_all",[wrap gen_List (gen_p,p_l)])
  | Lxsd.MG_choice p_l ->
      gen_Constr f (lxsd "MG_choice",[wrap gen_List (gen_p,p_l)])
  | Lxsd.MG_sequence p_l ->
      gen_Constr f (lxsd "MG_sequence",[wrap gen_List (gen_p,p_l)])

let gen_mgd_pair f (id_mgd,mgd) =
  gen_Pair f ((gen_mgd_id,gen_mgd), (id_mgd,mgd))

let gen_mgds f mgds =
  gen_List f (gen_mgd_pair, mgds)

let gen_root f root =
  gen_ed_id f root

let gen_roots f roots =
  gen_List f (gen_root, roots)

let gen_schema f = function
  | { Lxsd.target = target;
      Lxsd.uris = uris;
      Lxsd.tds = tds;
      Lxsd.ads = ads;
      Lxsd.eds = eds;
      Lxsd.agds = agds;
      Lxsd.mgds = mgds;
      Lxsd.roots = roots } ->
        gen_Record f [ (lxsd "target",(wrap gen_target target));
                       (lxsd "uris",(wrap gen_uris uris));
                       (lxsd "tds",(wrap gen_tds tds));
                       (lxsd "ads",(wrap gen_ads ads));
                       (lxsd "eds",(wrap gen_eds eds));
                       (lxsd "agds",(wrap gen_agds agds));
                       (lxsd "mgds",(wrap gen_mgds mgds));
                       (lxsd "roots",(wrap gen_roots roots)) ]

let gen_xsi_status f = function
  | Xml.XSI_reject_all ->
      gen_Constr f (xml "XSI_reject_all",[])
  | Xml.XSI_ignore_schemaLocation_only ->
      gen_Constr f (xml "XSI_ignore_schemaLocation_only",[])
  | Xml.XSI_ignore_all ->
      gen_Constr f (xml "XSI_ignore_all",[])


let gen_error_status f = function
  | { Xml.doctype = doctype;
      Xml.cdata = cdata;
      Xml.pi = pi;
      Xml.comment = comment;
      Xml.ascii_ref = ascii_ref;
      Xml.xsi = xsi } ->
        gen_Record f [ (xml "doctype",(wrap gen_Bool doctype));
                       (xml "cdata",(wrap gen_Bool cdata));
                       (xml "pi",(wrap gen_Bool pi));
                       (xml "comment",(wrap gen_Bool comment));
                       (xml "ascii_ref",(wrap gen_Bool ascii_ref));
                       (xml "xsi",(wrap gen_xsi_status xsi)) ]


let gen_configuration f = function
  | { Xml.max_attributes = m_a;
      Xml.max_contents = m_c;
      Xml.max_depth = m_d;
      Xml.max_length = m_l;
      Xml.error_status = e_s } ->
        gen_Record f [ (xml "max_attributes",(wrap gen_Option (gen_Int,m_a)));
                       (xml "max_contents",(wrap gen_Option (gen_Int,m_c)));
                       (xml "max_depth",(wrap gen_Option (gen_Int,m_d)));
                       (xml "max_length",(wrap gen_Option (gen_Int,m_l)));
                       (xml "error_status",(wrap gen_error_status e_s)) ]



let generate_ml f configuration schema =
  Format.fprintf f "@[<v>%s@,%s@,%s@, @[<v2>%a@]@,%s@, @[<v2>%a@]@]@."
    ("(* Generated by XSVGen, the XML Schema validator Generator *)")
    "(* To be compiled using the associated shell script *)"
    "let xml_configuration ="
    gen_configuration configuration
    "let schema ="
    gen_schema schema

let generate_sh f file_basename =
  let options = "-w +a -warn-error +a -strict-sequence -noautolink -cclib -lcamlstr" in
  let includes = "-I lib_src -I val_src" in
  let libs =
    "str.cmxa lib_src/error.mli lib_src/error.ml lib_src/lib.mli lib_src/lib.ml lib_src/utf8.mli lib_src/utf8.ml lib_src/file_in.mli lib_src/file_in.ml lib_src/stringdata.mli lib_src/stringdata.ml lib_src/xml.mli lib_src/xml.ml lib_src/lxsd.mli val_src/xsval.mli val_src/xsval.ml lib_src/message.mli lib_src/message.ml lib_src/xml_val.mli lib_src/xml_val.ml lib_src/xml_re.mli lib_src/xml_re.ml lib_src/xml_ns.mli lib_src/xml_ns.ml lib_src/xml_lexer.mli lib_src/xml_lexer.ml lib_src/xml_lib.mli lib_src/xml_lib.ml lib_src/xml_parser.mli lib_src/xml_parser.ml lib_src/val_re.mli lib_src/val_re.ml lib_src/val_parser.mli lib_src/val_parser.ml lib_src/validator.mli lib_src/validator.ml" in
  (* Shell header *)
  let header =
    "#!/bin/sh" in
  let compiling ff () =
    (* Compilation command *)
    Format.fprintf ff "%s %a %s %s %s@."
      "ocamlopt -o"
      File_out.fprintf file_basename
      options
      includes
      libs in
  Format.fprintf f "@[<v>%s@,%a@]"
    header
    compiling ()
