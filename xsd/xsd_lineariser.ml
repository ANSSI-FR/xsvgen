(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd_lineariser.ml 1686 2012-05-30 09:00:22Z maarek $ *)

(** {1 Lineariser from XSD components to XSD parsing table} *)

(** {2 Linearisation environment} *)


module Env : sig
  type t
  val empty : t

  val new_uri : Xsd.uri -> t -> t * Lxsd.uri_id
  val get_uris : t -> (Lxsd.uri_id * Stringdata.t) list

  val new_td_id : Xsd.uri * Xsd.ncname -> t -> t * Lxsd.td_id
  val new_td : (Xsd.uri * Xsd.ncname) option -> (Lxsd.st_unresolved,Lxsd.ct_unresolved) Lxsd.td -> t -> t * Lxsd.td_id

  type primitive_t =
    | ST_p of Lxsd.primitive_type
    | ST_np of Lxsd.non_primitive_type
    | ST_list
    | ST_union_without_list
    | ST_union_with_list
    | ST_no_primitive
    | CT

  module M_tds_order :
      sig
        type 'a t

        (** Fold function which follows the type definitions'
            dependency order. *)
        val fold :
            (int * Lxsd.td_id -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      end
  val get_tds :
      t -> (primitive_t * (Lxsd.st_unresolved,Lxsd.ct_unresolved) Lxsd.td) M_tds_order.t

  val new_ad_id : Xsd.uri * Xsd.ncname -> t -> t * Lxsd.ad_id
  val new_ad_global : Xsd.uri * Xsd.ncname -> Lxsd.ad -> t -> t * Lxsd.ad_id
  val new_ad_local : Xsd.uri * Xsd.ncname -> Lxsd.ad -> t -> t * Lxsd.ad_id
  val get_ads : t -> bool * (Lxsd.ad_id * Lxsd.ad) list

  val new_ed_id : Xsd.uri * Xsd.ncname -> t -> t * Lxsd.ed_id
  val new_ed : (Xsd.uri * Xsd.ncname) option -> Lxsd.ed -> t -> t * Lxsd.ed_id
  val new_root_ed_id : Xsd.uri * Xsd.ncname -> t -> t * Lxsd.ed_id
  val new_root_ed : (Xsd.uri * Xsd.ncname) option -> Lxsd.ed -> t -> t * Lxsd.ed_id
  val get_eds : t -> bool * (Lxsd.ed_id * Lxsd.ed) list

  val new_agd_id : Xsd.uri * Xsd.ncname -> t -> t * Lxsd.agd_id
  val new_agd : (Xsd.uri * Xsd.ncname) option -> Lxsd.au_unresolved Lxsd.agd -> t -> t * Lxsd.agd_id
  module M_agds : Map.S with type key = Lxsd.agd_id
  val get_agds : t -> bool * Lxsd.au_unresolved Lxsd.agd M_agds.t

  val new_mgd_id : Xsd.uri * Xsd.ncname -> t -> t * Lxsd.mgd_id
  val new_mgd : (Xsd.uri * Xsd.ncname) option -> Lxsd.mgd -> t -> t * Lxsd.mgd_id
  module M_mgds : Map.S with type key = Lxsd.mgd_id
  val get_mgds : t -> bool * Lxsd.mgd M_mgds.t

  val get_roots : t -> Lxsd.ed_id list

  val add_std_dependency : (Lxsd.td_id * Lxsd.std_variety) -> t -> t
  val add_ctd_dependency : (Lxsd.td_id * Lxsd.ctd_ref_un) -> t -> t

end = struct

  module M_uri =
    Map.Make
      (struct
        type t = Xsd.uri
        let compare (Xsd.Uri sd1) (Xsd.Uri sd2) =
          Stringdata.comp sd1 sd2
      end)

  module S_ed_id =
    Set.Make (struct type t = Lxsd.ed_id let compare = compare end)

  module M_type_table =
    Map.Make
      (struct
        type t = Lxsd.td_id
        let compare = compare
      end)

  module type Type =
    sig
      type id
      type def
      val id : int -> id
      val def_error : Error.xsdl_definition
    end

  module Make :
  functor (T : Type) ->
    sig
      type t
      val empty : t
      val new_id : (Lxsd.uri_id * Xsd.ncname) -> t -> T.id * t
      val new_d : (Lxsd.uri_id * Xsd.ncname) option -> T.def -> t -> T.id * t
      val dump : t -> bool * (T.id * T.def) list
      module M_d : Map.S with type key = T.id
      val map_dump : t -> bool * T.def M_d.t
      val has_missing_elements : t -> bool
      val get_d : T.id -> t -> T.def option
    end =
    functor (T: Type) ->
    struct
      module M_id =
        Map.Make
          (struct
            type t = Lxsd.uri_id * Xsd.ncname
            let compare
                (uri_id_1,Xsd.Ncname ncname_sd_1)
                (uri_id_2,Xsd.Ncname ncname_sd_2) =
              match compare uri_id_1 uri_id_2 with
              | 0 ->
                  Stringdata.comp ncname_sd_1 ncname_sd_2
              | n -> n
          end)
      module M_d =
        Map.Make (struct type t = T.id let compare = compare end)

      type t = int * T.id M_id.t * T.def M_d.t

      let empty =
        (0, M_id.empty, M_d.empty)

      let fresh_id (i,m_id,m_d) =
        if i = max_int
        then Message.error Error.ML_max_int_reached
        else
          let i' = i + 1 in
          (T.id i'),(i',m_id,m_d)

      let new_id (uri_id,ncname) (i,m_id,m_d) =
        try
          let this_id = M_id.find (uri_id,ncname) m_id in
          (this_id, (i,m_id,m_d))
        with
        | Not_found ->
            let this_id,(i',m_id',m_d') = fresh_id (i,m_id,m_d) in
            this_id,
            (i', (M_id.add (uri_id,ncname) this_id m_id'), m_d')

      let new_d opt (def : T.def) (i,m_id,m_d) =
        match opt with
        | None ->
            let this_id,(i',m_id',m_d') = fresh_id (i,m_id,m_d) in
            this_id, (i', m_id', (M_d.add this_id def m_d'))
        | Some (uri_id,ncname) ->
            let this_id,(i',m_id',m_d') =
              new_id (uri_id,ncname) (i,m_id,m_d) in
            if M_d.mem this_id m_d
            then
              begin
                let Xsd.Ncname loc = ncname in
                Message.warning_at loc
                  (Error.XSDL_multiple_definition T.def_error)
              end;
            this_id, (i', m_id', (M_d.add this_id def m_d'))

      let get_d id (_,_,m_d) =
        try Some (M_d.find id m_d)
        with
        | Not_found -> None

      let has_missing_elements (_,m_id,m_d) =
        let missings =
          M_id.filter
            (fun (_,_) this_id ->
              not (M_d.mem this_id m_d))
            m_id in
        M_id.iter
          (fun (_,Xsd.Ncname ncname) _ ->
            Message.warning_at ncname
              (Error.XSDL_missing_definition T.def_error) )
          missings;
        M_id.cardinal missings <> 0

      let dump (i,m_id,m_d) =
        (has_missing_elements (i,m_id,m_d)),
        (M_d.bindings m_d)

      let map_dump (i,m_id,m_d) =
        (has_missing_elements (i,m_id,m_d)),
        m_d

    end

  module D_td = Make
      (struct
        type id = Lxsd.td_id
        type def = (Lxsd.st_unresolved,Lxsd.ct_unresolved) Lxsd.td
        let id i = Lxsd.Id_td i
        let def_error = Error.XSDLDEF_type
      end)

  module D_ad = Make
      (struct
        type id = Lxsd.ad_id
        type def = Lxsd.ad
        let id i = Lxsd.Id_ad i
        let def_error = Error.XSDLDEF_attribute
      end)

  module D_ed = Make
      (struct
        type id = Lxsd.ed_id
        type def = Lxsd.ed
        let id i = Lxsd.Id_ed i
        let def_error = Error.XSDLDEF_element
      end)

  module D_agd = Make
      (struct
        type id = Lxsd.agd_id
        type def = Lxsd.au_unresolved Lxsd.agd
        let id i = Lxsd.Id_agd i
        let def_error = Error.XSDLDEF_attribute_group
      end)

  module D_mgd = Make
      (struct
        type id = Lxsd.mgd_id
        type def = Lxsd.mgd
        let id i = Lxsd.Id_mgd i
        let def_error = Error.XSDLDEF_model_group
      end)

  type t =
      { uris : Lxsd.uri_id M_uri.t;
        tds : D_td.t;
        ads : D_ad.t;
        eds : D_ed.t;
        agds : D_agd.t;
        mgds : D_mgd.t;
        roots : S_ed_id.t;
        type_tables : Lxsd.std_variety M_type_table.t * Lxsd.ctd_ref_un M_type_table.t }

  let empty =
    { uris = M_uri.empty;
      tds = D_td.empty;
      ads = D_ad.empty;
      eds = D_ed.empty;
      agds = D_agd.empty;
      mgds = D_mgd.empty;
      roots = S_ed_id.empty;
      type_tables = M_type_table.empty,M_type_table.empty }

  let add_root ed_id = function
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          { uris = uris;
            tds = tds;
            ads = ads;
            eds = eds;
            agds = agds;
            mgds = mgds;
            roots = (S_ed_id.add ed_id roots);
            type_tables = type_tables }

  let new_uri_get_id uri env =
    match env with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          begin
            try env, (M_uri.find uri uris) with
            | Not_found ->
                let i = M_uri.cardinal uris in
                let i' =
                  if i = max_int
                  then Message.error Error.ML_max_int_reached
                  else i + 1 in
                let uri_id =
                  Lxsd.Id_uri i' in
                { uris = M_uri.add uri uri_id uris;
                  tds = tds;
                  ads = ads;
                  eds = eds;
                  agds = agds;
                  mgds = mgds;
                  roots = roots;
                  type_tables = type_tables },
                uri_id
          end

  let new_uri = new_uri_get_id

  let uri_ncname_key (uri,ncname) env1 =
    let env2,uri_id =
      new_uri_get_id uri env1 in
    env2,uri_id,ncname

  let opt_uri_ncname_key opt env1 =
    match opt with
    | None -> env1,None
    | Some (uri,ncname) ->
        let env2,uri_id,ncname' =
          uri_ncname_key (uri,ncname) env1 in
        env2, Some (uri_id,ncname')

  let get_uris t =
    List.map
      (fun ((Xsd.Uri uri_sd),id) -> (id,uri_sd))
      (M_uri.bindings t.uris)

  let new_td_id (uri,ncname) env =
    let env',uri_id,_ =
      uri_ncname_key (uri,ncname) env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let td_id,tds' = D_td.new_id (uri_id,ncname) tds in
          { uris = uris;
            tds = tds';
            ads = ads;
            eds = eds;
            agds = agds;
            mgds = mgds;
            roots = roots;
            type_tables = type_tables },
          td_id

  let new_td opt td env =
    let env',opt' =
      opt_uri_ncname_key opt env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let td_id,tds' = D_td.new_d opt' td tds in
          { uris = uris;
            tds = tds';
            ads = ads;
            eds = eds;
            agds = agds;
            mgds = mgds;
            roots = roots;
            type_tables = type_tables },
          td_id

  let new_ad_id (uri,ncname) env =
    let env',uri_id,_ =
      uri_ncname_key (uri,ncname) env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let ad_id,ads' = D_ad.new_id (uri_id,ncname) ads in
          { uris = uris;
            tds = tds;
            ads = ads';
            eds = eds;
            agds = agds;
            mgds = mgds;
            roots = roots;
            type_tables = type_tables },
          ad_id

  let get_ads env1 =
    D_ad.dump env1.ads

  let new_ad_global (uri,ncname) ad env =
    let env',uri_id,_ =
      uri_ncname_key (uri,ncname) env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let ad_id,ads' = D_ad.new_d (Some (uri_id,ncname)) ad ads in
          { uris = uris;
            tds = tds;
            ads = ads';
            eds = eds;
            agds = agds;
            mgds = mgds;
            roots = roots;
            type_tables = type_tables },
          ad_id

  let new_ad_local (uri,_ncname) ad env =
    let env',_uri_id =
      new_uri_get_id uri env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let ad_id,ads' = D_ad.new_d None ad ads in
          { uris = uris;
            tds = tds;
            ads = ads';
            eds = eds;
            agds = agds;
            mgds = mgds;
            roots = roots;
            type_tables = type_tables },
          ad_id


  let new_ed_id_fun (uri,ncname) env =
    let env',uri_id,_ =
      uri_ncname_key (uri,ncname) env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let ed_id,eds' = D_ed.new_id (uri_id,ncname) eds in
          { uris = uris;
            tds = tds;
            ads = ads;
            eds = eds';
            agds = agds;
            mgds = mgds;
            roots = roots;
            type_tables = type_tables },
          ed_id

  let new_ed_id = new_ed_id_fun

  let new_root_ed_id (uri,ncname) env =
    let env',ed_id =
      new_ed_id_fun (uri,ncname) env in
    (add_root ed_id env'),ed_id

  let new_ed_fun opt ed env =
    let env',opt' =
      opt_uri_ncname_key opt env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let ed_id,eds' = D_ed.new_d opt' ed eds in
          { uris = uris;
            tds = tds;
            ads = ads;
            eds = eds';
            agds = agds;
            mgds = mgds;
            roots = roots;
            type_tables = type_tables },
          ed_id

  let new_ed = new_ed_fun

  let new_root_ed opt ed env =
    let env',ed_id =
      new_ed_fun opt ed env in
    (add_root ed_id env'),ed_id

  let get_eds env1 =
    D_ed.dump env1.eds


  let new_agd_id (uri,ncname) env =
    let env',uri_id,_ =
      uri_ncname_key (uri,ncname) env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let agd_id,agds' = D_agd.new_id (uri_id,ncname) agds in
          { uris = uris;
            tds = tds;
            ads = ads;
            eds = eds;
            agds = agds';
            mgds = mgds;
            roots = roots;
            type_tables = type_tables },
          agd_id

  let new_agd opt agd env =
    let env',opt' =
      opt_uri_ncname_key opt env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let agd_id,agds' = D_agd.new_d opt' agd agds in
          { uris = uris;
            tds = tds;
            ads = ads;
            eds = eds;
            agds = agds';
            mgds = mgds;
            roots = roots;
            type_tables = type_tables },
          agd_id

  module M_agds = D_agd.M_d
  let get_agds env1 =
    D_agd.map_dump env1.agds

  let new_mgd_id (uri,ncname) env =
    let env',uri_id,_ =
      uri_ncname_key (uri,ncname) env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let mgd_id,mgds' = D_mgd.new_id (uri_id,ncname) mgds in
          { uris = uris;
            tds = tds;
            ads = ads;
            eds = eds;
            agds = agds;
            mgds = mgds';
            roots = roots;
            type_tables = type_tables },
          mgd_id

  let new_mgd opt mgd env =
    let env',opt' =
      opt_uri_ncname_key opt env in
    match env' with
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = type_tables } ->
          let mgd_id,mgds' = D_mgd.new_d opt' mgd mgds in
          { uris = uris;
            tds = tds;
            ads = ads;
            eds = eds;
            agds = agds;
            mgds = mgds';
            roots = roots;
            type_tables = type_tables },
          mgd_id

  module M_mgds = D_mgd.M_d
  let get_mgds env1 =
    D_mgd.map_dump env1.mgds

  let get_roots env =
    if S_ed_id.is_empty env.roots
    then Message.error Error.XSDL_SEC_no_root_element_declared
    else S_ed_id.elements env.roots


  let add_std_dependency (td_id,variety) = function
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = std_type_table,ctd_type_table } ->
          let std_type_table' = M_type_table.add td_id variety std_type_table in
          { uris = uris;
            tds = tds;
            ads = ads;
            eds = eds;
            agds = agds;
            mgds = mgds;
            roots = roots;
            type_tables = std_type_table',ctd_type_table }

  let add_ctd_dependency (td_id,ctd_ref) = function
    | { uris = uris;
        tds = tds;
        ads = ads;
        eds = eds;
        agds = agds;
        mgds = mgds;
        roots = roots;
        type_tables = std_type_table,ctd_type_table } ->
          let ctd_type_table' = M_type_table.add td_id ctd_ref ctd_type_table in
          { uris = uris;
            tds = tds;
            ads = ads;
            eds = eds;
            agds = agds;
            mgds = mgds;
            roots = roots;
            type_tables = std_type_table,ctd_type_table' }


  module M_td_id =
    Map.Make (struct type t = Lxsd.td_id let compare = compare end)


  module S_td_id =
    Set.Make (struct type t = Lxsd.td_id let compare = compare end)



  (** Dependency type *)
  type dependency =
    | Based_on of Lxsd.td_id (** the type is based on another type *)
    | Union_of of bool * S_td_id.t (** the type is an union based on
                                       a set of types, the boolean value
                                       indicates if the set contains
                                       a type based on a list *)
    | List_of of Lxsd.td_id (** the type is a list bases on a type *)

  type primitive_t =
    | ST_p of Lxsd.primitive_type
    | ST_np of Lxsd.non_primitive_type
    | ST_list
    | ST_union_without_list
    | ST_union_with_list
    | ST_no_primitive
    | CT

  (** Type definition dependency order's map *)
  module M_tds_order = Map.Make (struct
    type t = int * Lxsd.td_id
    let compare (level_1,td_id_1) (level_2,td_id_2) =
      match compare level_1 level_2 with
      | 0 ->
          (* arbitrary order between non dependent types to have each
             type definition corresponding to one entry in the map *)
          compare td_id_1 td_id_2
      | n ->
          (* order mainly depends on the level in the dependency tree,
             the level is 0 for a type which depends on no defined types,
             n for a type which depends on a defined type of level n *) n
  end)


  let rec transitive_closure env level td_id_set type_table dependencies =
    if S_td_id.is_empty td_id_set
    then
      if M_td_id.is_empty dependencies
      then
        level,type_table
      else
        Message.error(* TODO locate the recursive definitions *)
          Error.XSDL_recursive_definitions
    else
      let current_level,dependencies' =
        M_td_id.partition
          (fun _ dependency ->
            match dependency with
            | Based_on td_id ->
                S_td_id.mem td_id td_id_set
            | Union_of (_,deps_set) ->
                S_td_id.subset deps_set td_id_set
            | List_of td_id ->
                S_td_id.mem td_id td_id_set)
          dependencies
      in
      let type_table',new_td_id_set =
        M_td_id.fold
          (fun td_id dep (types,ids) ->
            let def =
              (* we get the recorded definition of the type *)
              match D_td.get_d td_id env.tds with
              | None ->
                  Message.error Error.XSDL_missing_definition_s
              | Some def -> def
            in
            let primitive_base =
              match dep with
              | Based_on base_td_id ->
                  begin
                    try
                      match M_tds_order.find
                          (level-1,base_td_id)
                          type_table with
                      | (p_opt,_) -> p_opt
                    with
                    | Not_found ->
                        Message.error
                          (Error.A_function
                             "Xsd.linearise.transitive_closure")
                  end
              | Union_of (true,_) -> ST_union_with_list
              | Union_of (false,_) -> ST_union_without_list
              | List_of _ -> ST_list
            in
            (M_tds_order.add
               (level,td_id)
               (primitive_base,def)
               types),
            (S_td_id.add td_id ids) )
          current_level
          (type_table,S_td_id.empty)
      in
      let dependencies'' =
        M_td_id.map
          (function
            | (Based_on _ | List_of _) as dep -> dep
            | Union_of (has_list,deps_set) ->
                (* we remove from the set of types on which the union
                   depends, the types that were added to the current
                   level; we update the boolean indicating if the union
                   contains a type based on a list *)
                let has_list' =
                  has_list ||
                  S_td_id.exists
                    (fun td_id ->
                      try
                        let p,_ =
                          M_tds_order.find (level-1,td_id) type_table' in
                        match p with
                        | ST_list
                        | ST_union_with_list
                          -> true
                        | ST_p _
                        | ST_np _
                        | ST_union_without_list
                        | ST_no_primitive
                        | CT
                          -> false
                      with
                      | Not_found ->
                          Message.error
                            (Error.A_function
                               "Xsd.linearise.transitive_closure")
                    )
                    td_id_set
                in
                Union_of (has_list',(S_td_id.diff deps_set td_id_set)))
          dependencies'
      in
      transitive_closure env (level + 1) new_td_id_set type_table' dependencies''


  let get_tds env1 =
    if D_td.has_missing_elements env1.tds
    then
      Message.error Error.XSDL_missing_definition_s;
    let std_type_table,ctd_type_table =
      env1.type_tables in
    let autonomous_std_table,std_dependency_table =
      M_type_table.fold
        (fun td_id variety (autonomous_types,dependencies) ->
          let def =
            (* we get the recorded definition of the type *)
            match D_td.get_d td_id env1.tds with
            | None ->
                Message.error Error.XSDL_missing_definition_s
            | Some def -> def
          in
          match variety with
          | Lxsd.STD_atomic std_ref ->
              begin
                match std_ref with
                | Lxsd.STD_built_in Lxsd.STD_anySimpleType
                | Lxsd.STD_built_in Lxsd.STD_anyAtomicType ->
                    (* we add types with no base *)
                    (M_tds_order.add
                       (0,td_id)
                       (ST_no_primitive,def)
                       autonomous_types),
                    dependencies
                | Lxsd.STD_built_in (Lxsd.STD_primitive_type p) ->
                    (* we add types with a primitive type as base *)
                    (M_tds_order.add
                       (0,td_id)
                       ((ST_p p),def)
                       autonomous_types),
                    dependencies
                | Lxsd.STD_built_in (Lxsd.STD_non_primitive_type np) ->
                    (* we add types with a non primitive type as base *)
                    (M_tds_order.add
                       (0,td_id)
                       ((ST_np np),def)
                       autonomous_types),
                    dependencies
                | Lxsd.STD_td_ref base_td_id ->
                    (* we add the dependency between a type and its base *)
                    autonomous_types,
                    (M_td_id.add td_id (Based_on base_td_id) dependencies)
              end
          | Lxsd.STD_list std_ref ->
              begin
                match std_ref with
                | Lxsd.STD_built_in _ ->
                    (* we add the list type which does not depend on
                       another defined type *)
                    (M_tds_order.add
                       (0,td_id)
                       (ST_list,def)
                       autonomous_types),
                    dependencies
                | Lxsd.STD_td_ref founding_td_id ->
                    (* we add the dependency between a list type and its
                       founding type *)
                    autonomous_types,
                    (M_td_id.add
                       td_id
                       (List_of founding_td_id)
                       dependencies)
              end
          | Lxsd.STD_union std_ref_l ->
              begin
                let has_np_list,founding_td_id_set =
                  List.fold_left
                    (fun (b,set) ->
                      function
                        | Lxsd.STD_built_in Lxsd.STD_anySimpleType
                        | Lxsd.STD_built_in Lxsd.STD_anyAtomicType
                        | Lxsd.STD_built_in (Lxsd.STD_primitive_type _) ->
                            b,set
                        | Lxsd.STD_built_in (Lxsd.STD_non_primitive_type np) ->
                            if b
                            then
                              b,set
                            else
                              let b' =
                                match np with
                                | Lxsd.NPT_NMTOKENS
                                | Lxsd.NPT_IDREFS
                                | Lxsd.NPT_ENTITIES
                                    -> true
                                | Lxsd.NPT_normalizedString
                                | Lxsd.NPT_token
                                | Lxsd.NPT_language
                                | Lxsd.NPT_NMTOKEN
                                | Lxsd.NPT_Name
                                | Lxsd.NPT_NCName
                                | Lxsd.NPT_ID
                                | Lxsd.NPT_IDREF
                                | Lxsd.NPT_ENTITY
                                | Lxsd.NPT_integer
                                | Lxsd.NPT_nonPositiveInteger
                                | Lxsd.NPT_negativeInteger
                                | Lxsd.NPT_long
                                | Lxsd.NPT_int
                                | Lxsd.NPT_short
                                | Lxsd.NPT_byte
                                | Lxsd.NPT_nonNegativeInteger
                                | Lxsd.NPT_unsignedLong
                                | Lxsd.NPT_unsignedInt
                                | Lxsd.NPT_unsignedShort
                                | Lxsd.NPT_unsignedByte
                                | Lxsd.NPT_positiveInteger
                                | Lxsd.NPT_yearMonthDuration
                                | Lxsd.NPT_dayTimeDuration
                                | Lxsd.NPT_dateTimeStamp
                                    -> false
                              in
                              b',set
                        | Lxsd.STD_td_ref founding_td_id ->
                            b,(S_td_id.add founding_td_id set) )
                    (false,S_td_id.empty)
                    std_ref_l
                in
                if S_td_id.is_empty founding_td_id_set
                then
                  let primitive =
                    if has_np_list
                    then ST_union_with_list
                    else ST_union_without_list in
                  (* we add the union type which depends on no other
                     defined types *)
                    (M_tds_order.add
                       (0,td_id)
                       (primitive,def)
                       autonomous_types),
                    dependencies
                else
                  (* we add the dependency between a union type and its
                     founding types *)
                  autonomous_types,
                  (M_td_id.add
                     td_id
                     (Union_of (has_np_list,founding_td_id_set))
                     dependencies)
              end)
        std_type_table
        (M_tds_order.empty,M_td_id.empty)
    in
    let autonomous_std_id_set =
      M_tds_order.fold
        (fun (_,td_id) _ set -> S_td_id.add td_id set)
        autonomous_std_table
        S_td_id.empty
    in
    (* we now compute the transitive closure of the dependencies to
       complete the type table *)
    let ctd_0,ordered_std_table =
      transitive_closure
        env1
        1
        autonomous_std_id_set
        autonomous_std_table
        std_dependency_table
    in
    let autonomous_ctd_table,ctd_dependency_table =
      M_type_table.fold
        (fun td_id ctd_ref (autonomous_types,dependencies) ->
          let def =
            (* we get the recorded definition of the type *)
            match D_td.get_d td_id env1.tds with
            | None ->
                Message.error Error.XSDL_missing_definition_s
            | Some def -> def
          in
          match ctd_ref with
          | Lxsd.CTD_anyType_un ->
              (M_tds_order.add
                 (ctd_0,td_id)
                 (CT,def)
                autonomous_types),
              dependencies
          | Lxsd.CTD_td_ref_un base_td_id ->
              autonomous_types,
              (M_td_id.add
                 td_id
                 (Based_on base_td_id)
                 dependencies)
          | Lxsd.CTD_simpleContentType_un ->
              (M_tds_order.add
                 (ctd_0,td_id)
                 (CT,def)
                 autonomous_types),
              dependencies )
        ctd_type_table
        (M_tds_order.empty,M_td_id.empty)
    in
    let autonomous_ctd_id_set =
      M_tds_order.fold
        (fun (_,td_id) _ set -> S_td_id.add td_id set)
        autonomous_ctd_table
        S_td_id.empty
    in
    let std_autonomous_ctd =
      M_tds_order.merge
        (fun _ opt1 opt2 ->
          match opt1,opt2 with
          | (Some v),None
          | None,(Some v) -> Some v
          | None,None
          | (Some _),(Some _) ->
              Message.error (Error.A_function "Xsd_lineariser.get_tds") )
        ordered_std_table
        autonomous_ctd_table
    in
    (* we now compute the transitive closure of the dependencies to complete the type table *)
    let _,ordered_td_table =
      transitive_closure
        env1
        (ctd_0 + 1)
        autonomous_ctd_id_set
        std_autonomous_ctd
        ctd_dependency_table
    in
    ordered_td_table


end

(** {2 Shared functions} *)

let expand_ncname uri (Xsd.Ncname t) =
  { Xml.uri = uri;
    Xml.name = t }

let dm_of_dm = function
  | Xsd.Extension -> Lxsd.Extension_un
  | Xsd.Restriction -> Lxsd.Restriction_un

let pc_of_pc = function
  | Xsd.Skip -> Lxsd.Skip
  | Xsd.Strict -> Lxsd.Strict
  | Xsd.Lax -> Lxsd.Lax


let value_constraint_of_value_constraint = function
  | Xsd.A_default sd ->
      Lxsd.A_default sd
  | Xsd.A_fixed sd ->
      Lxsd.A_fixed sd

let occurrence_of_occurrence = function
  | { Xsd.min_occurs = min;
      Xsd.max_occurs = max } ->
        { Lxsd.min_occurs = min;
          Lxsd.max_occurs =
          (function
            | Xsd.Occur i -> Lxsd.Occur i
            | Xsd.Unbounded -> Lxsd.Unbounded)
            max }

let aus_partition aus_l =
  Lib.list_partition
    (function
      | Xsd.AUS_au _ -> true
      | Xsd.AUS_agd_ref _ -> false)
    (function
      | Xsd.AUS_au au -> au
      | Xsd.AUS_agd_ref _ ->
          Message.error (Error.A_function "Xsd_linearise.aus_partition"))
    (function
      | Xsd.AUS_au _ ->
          Message.error (Error.A_function "Xsd_linearise.aus_partition")
      | Xsd.AUS_agd_ref agd_ref -> agd_ref)
    aus_l


(** {2 Linearisation functions} *)

let linearise_whitespace = function
  | Xsd.W_collapse ->
      Lxsd.W_collapse
  | Xsd.W_preserve ->
      Lxsd.W_preserve
  | Xsd.W_replace ->
      Lxsd.W_replace

let linearise_facet = function
  | Xsd.CF_pattern (re,b) ->
      Lxsd.CF_pattern ((Xsd_lib.regexp_of_regexp re),b)
  | Xsd.CF_length (i,b) ->
      Lxsd.CF_length (i,b)
  | Xsd.CF_minLength (i,b) ->
      Lxsd.CF_minLength (i,b)
  | Xsd.CF_maxLength (i,b) ->
      Lxsd.CF_maxLength (i,b)
  | Xsd.CF_whiteSpace (w,b) ->
      Lxsd.CF_whiteSpace (linearise_whitespace w,b)
  | Xsd.CF_enumeration _ ->
      Message.error (Error.A_function "Xsd_lineariser.linearise_facet")
  | Xsd.CF_maxInclusive (sd,b) ->
      Lxsd.CF_maxInclusive (sd,b)
  | Xsd.CF_maxExclusive (sd,b) ->
      Lxsd.CF_maxExclusive (sd,b)
  | Xsd.CF_minExclusive (sd,b) ->
      Lxsd.CF_minExclusive (sd,b)
  | Xsd.CF_minInclusive (sd,b) ->
      Lxsd.CF_minInclusive (sd,b)
  | Xsd.CF_totalDigits
  | Xsd.CF_fractionDigits
  | Xsd.CF_assertions
  | Xsd.CF_explicitTimezone ->
      Message.error Error.XSDL_LIM_facet_not_implemented

let enumeration_facet = function
  | Xsd.CF_enumeration _ ->
      true
  | Xsd.CF_pattern _
  | Xsd.CF_length _
  | Xsd.CF_minLength _
  | Xsd.CF_maxLength _
  | Xsd.CF_whiteSpace _
  | Xsd.CF_maxInclusive _
  | Xsd.CF_maxExclusive _
  | Xsd.CF_minExclusive _
  | Xsd.CF_minInclusive _
  | Xsd.CF_totalDigits
  | Xsd.CF_fractionDigits
  | Xsd.CF_assertions
  | Xsd.CF_explicitTimezone ->
      false

let implemented_facet = function
  | Xsd.CF_pattern _
  | Xsd.CF_length _
  | Xsd.CF_minLength _
  | Xsd.CF_maxLength _
  | Xsd.CF_whiteSpace _
  | Xsd.CF_enumeration _
  | Xsd.CF_maxInclusive _
  | Xsd.CF_maxExclusive _
  | Xsd.CF_minExclusive _
  | Xsd.CF_minInclusive _ ->
      true
  | Xsd.CF_totalDigits
  | Xsd.CF_fractionDigits
  | Xsd.CF_assertions
  | Xsd.CF_explicitTimezone ->
      false



let linearise_primitive_type = function
  | Xsd.PT_string -> Lxsd.PT_string
  | Xsd.PT_boolean -> Lxsd.PT_boolean
  | Xsd.PT_decimal -> Lxsd.PT_decimal
  | Xsd.PT_float -> Lxsd.PT_float
  | Xsd.PT_double -> Lxsd.PT_double
  | Xsd.PT_duration -> Lxsd.PT_duration
  | Xsd.PT_dateTime -> Lxsd.PT_dateTime
  | Xsd.PT_time -> Lxsd.PT_time
  | Xsd.PT_date -> Lxsd.PT_date
  | Xsd.PT_gYearMonth -> Lxsd.PT_gYearMonth
  | Xsd.PT_gYear -> Lxsd.PT_gYear
  | Xsd.PT_gMonthDay -> Lxsd.PT_gMonthDay
  | Xsd.PT_gDay -> Lxsd.PT_gDay
  | Xsd.PT_gMonth -> Lxsd.PT_gMonth
  | Xsd.PT_hexBinary -> Lxsd.PT_hexBinary
  | Xsd.PT_base64Binary -> Lxsd.PT_base64Binary
  | Xsd.PT_anyURI -> Lxsd.PT_anyURI
  | Xsd.PT_QName -> Lxsd.PT_QName
  | Xsd.PT_NOTATION -> Lxsd.PT_NOTATION

let linearise_non_primitive_type = function
  | Xsd.NPT_normalizedString -> Lxsd.NPT_normalizedString
  | Xsd.NPT_token -> Lxsd.NPT_token
  | Xsd.NPT_language -> Lxsd.NPT_language
  | Xsd.NPT_IDREFS -> Lxsd.NPT_IDREFS
  | Xsd.NPT_ENTITIES -> Lxsd.NPT_ENTITIES
  | Xsd.NPT_NMTOKEN -> Lxsd.NPT_NMTOKEN
  | Xsd.NPT_NMTOKENS -> Lxsd.NPT_NMTOKENS
  | Xsd.NPT_Name -> Lxsd.NPT_Name
  | Xsd.NPT_NCName -> Lxsd.NPT_NCName
  | Xsd.NPT_ID -> Lxsd.NPT_ID
  | Xsd.NPT_IDREF -> Lxsd.NPT_IDREF
  | Xsd.NPT_ENTITY -> Lxsd.NPT_ENTITY
  | Xsd.NPT_integer -> Lxsd.NPT_integer
  | Xsd.NPT_nonPositiveInteger -> Lxsd.NPT_nonPositiveInteger
  | Xsd.NPT_negativeInteger -> Lxsd.NPT_negativeInteger
  | Xsd.NPT_long -> Lxsd.NPT_long
  | Xsd.NPT_int -> Lxsd.NPT_int
  | Xsd.NPT_short -> Lxsd.NPT_short
  | Xsd.NPT_byte -> Lxsd.NPT_byte
  | Xsd.NPT_nonNegativeInteger -> Lxsd.NPT_nonNegativeInteger
  | Xsd.NPT_unsignedLong -> Lxsd.NPT_unsignedLong
  | Xsd.NPT_unsignedInt -> Lxsd.NPT_unsignedInt
  | Xsd.NPT_unsignedShort -> Lxsd.NPT_unsignedShort
  | Xsd.NPT_unsignedByte -> Lxsd.NPT_unsignedByte
  | Xsd.NPT_positiveInteger -> Lxsd.NPT_positiveInteger
  | Xsd.NPT_yearMonthDuration -> Lxsd.NPT_yearMonthDuration
  | Xsd.NPT_dayTimeDuration -> Lxsd.NPT_dayTimeDuration
  | Xsd.NPT_dateTimeStamp -> Lxsd.NPT_dateTimeStamp






let linearise_built_in_type = function
  | Xsd.STD_anySimpleType ->
      Lxsd.STD_anySimpleType
  | Xsd.STD_anyAtomicType ->
      Lxsd.STD_anyAtomicType
  | Xsd.STD_primitive_type p ->
      let p' =
        linearise_primitive_type p in
      Lxsd.STD_primitive_type p'
  | Xsd.STD_non_primitive_type np ->
      let np' =
        linearise_non_primitive_type np in
      Lxsd.STD_non_primitive_type np'

let rec linearise_ed env1 = function
  | { Xsd.ed_ncname = (Xsd.Ncname ed_ncname_sd) as ed_ncname;
      Xsd.ed_td = ctd_ref;
      Xsd.ed_scope = scope;
      Xsd.ed_target = target;
      Xsd.ed_value_constraint = v_c_opt } ->
        let env2,uri_id =
          Env.new_uri target env1 in
        let env3,ctd_ref' =
          linearise_ctd_ref env2 ctd_ref in
        let v_c_opt' =
          Lib.opt_apply value_constraint_of_value_constraint v_c_opt in
        let ed =
          { Lxsd.ed_ncname = ed_ncname_sd;
            Lxsd.ed_target = uri_id;
            Lxsd.ed_td = ctd_ref';
            Lxsd.ed_value_constraint = v_c_opt' } in
        match scope with
        | Xsd.Local ->
            Env.new_ed (Some (target,ed_ncname)) ed env3
        | Xsd.Global ->
            Env.new_root_ed (Some (target,ed_ncname)) ed env3

and linearise_td_ref env1 = function
  | Xsd.Val td -> linearise_td env1 td
  | Xsd.Ref { Xml.uri = uri; Xml.name = name }  ->
      Env.new_td_id ((Xsd_lib.to_uri uri), (Xsd_lib.to_ncname Xml_lexer.is_ncname name)) env1

and linearise_td env1  = function
  | Xsd.STD std ->
      linearise_std env1 std
  | Xsd.CTD ctd ->
      linearise_ctd env1 ctd

and linearise_std env1 = function
  | { Xsd.std_name_or_context = std_name_or_context;
      Xsd.std_target = target;
      Xsd.std_facets = facets;
      Xsd.std_ffacets = __ffacets;
      Xsd.std_variety = std_variety } ->
        let env2,std_variety' =
          linearise_std_variety env1 std_variety in
        let enumeration_facets,other_facets =
          List.partition
            enumeration_facet
            facets
        in
        (* we lienarise the non-enumeration facets *)
        let other_facets' =
          Lib.list_partial_map
            implemented_facet
            linearise_facet
            other_facets in
        (* we combine the enumerations facets *)
        let enumerations =
          List.fold_left
            (fun enums facet ->
              match facet with
              | Xsd.CF_minInclusive (_, _) | Xsd.CF_minExclusive (_, _)
              | Xsd.CF_maxExclusive (_, _) | Xsd.CF_maxInclusive (_, _)
              | Xsd.CF_whiteSpace (_, _) | Xsd.CF_pattern (_, _)
              | Xsd.CF_maxLength (_, _) | Xsd.CF_minLength (_, _)
              | Xsd.CF_length (_, _) | Xsd.CF_explicitTimezone
              | Xsd.CF_assertions | Xsd.CF_fractionDigits
              | Xsd.CF_totalDigits ->
                  Message.error (Error.A_function "Xsd_lineariser.linearise_facet")
              | Xsd.CF_enumeration sd ->
                  sd::enums)
            []
            enumeration_facets
        in
        (* we add the enumerations into a single enumeration facet *)
        let facets' =
          match enumerations with
          | [] -> other_facets'
          | enums ->
              (Lxsd.CF_enumeration enums)::other_facets'
        in
        let td =
          Lxsd.STD
            { Lxsd.std_details =
              { Lxsd.std_facets = facets';
                Lxsd.std_variety = std_variety' } }
        in
        let env3,td_id =
          match std_name_or_context with
          | Xsd.STD_name ncname ->
              Env.new_td (Some (target,ncname)) td env2
          | Xsd.STD_context ->
              Env.new_td None td env2
        in
        let env4 =
          Env.add_std_dependency (td_id,std_variety') env3
        in
        env4,td_id

and linearise_std_variety env1 = function
  | Xsd.STD_atomic std_ref ->
      let env2,std_ref' =
        linearise_std_ref env1 std_ref in
      env2, (Lxsd.STD_atomic std_ref')
  | Xsd.STD_list std_ref ->
      let env2,std_ref' =
        linearise_std_ref env1 std_ref in
      env2, (Lxsd.STD_list std_ref')
  | Xsd.STD_union std_ref_l ->
      let env2,reverse_std_ref_l' =
        List.fold_left
          (fun (env,l) std_ref ->
            let env',std_ref' =
              linearise_std_ref env std_ref in
            env', (std_ref' :: l) )
          (env1,[])
          std_ref_l in
      let std_ref_l' = List.rev reverse_std_ref_l' in
      env2, (Lxsd.STD_union std_ref_l')

and linearise_std_ref env1 = function
  | Xsd.STD_built_in st ->
      let st' =
        linearise_built_in_type st in
      env1,(Lxsd.STD_built_in st')
  | Xsd.STD_td_ref std_ref ->
      let (env2,td_id) =
        linearise_td_ref env1 std_ref in
      env2, (Lxsd.STD_td_ref td_id)

and linearise_ad env1 = function
  | { Xsd.ad_name = Xsd.Ncname name_sd as ncname;
      Xsd.ad_target = target;
      Xsd.ad_std = std_ref;
      Xsd.ad_scope = scope;
      Xsd.ad_value_constraint = v_c_opt } ->
        let env2,uri_id =
          Env.new_uri target env1 in
        let env3,td_id =
          linearise_td env2
            (* we create a type definition out of the simple type
               definition reference *)
            (Xsd.STD
               { Xsd.std_name_or_context = Xsd.STD_context;
                 Xsd.std_target = target;
                 Xsd.std_facets = [];
                 Xsd.std_ffacets = [];
                 Xsd.std_variety = Xsd.STD_atomic std_ref } ) in
        let v_c_opt' =
          Lib.opt_apply value_constraint_of_value_constraint v_c_opt in
        let ad = { Lxsd.ad_name = name_sd;
                   Lxsd.ad_target = uri_id;
                   Lxsd.ad_std = td_id;
                   Lxsd.ad_value_constraint = v_c_opt' } in
        match scope with
        | Xsd.Global ->
            Env.new_ad_global (target,ncname) ad env3
        | Xsd.Local ->
            Env.new_ad_local (target,ncname) ad env3

and linearise_ad_ref env1 = function
  | Xsd.Val ad -> linearise_ad env1 ad
  | Xsd.Ref { Xml.uri = uri; Xml.name = name }  ->
      Env.new_ad_id ((Xsd_lib.to_uri uri), (Xsd_lib.to_ncname Xml_lexer.is_ncname name)) env1

and linearise_au env1 = function
  | { Xsd.au_use = Xsd.Prohibited;
      Xsd.au_ad = _;
      Xsd.au_value_constraint = _ } ->
        env1,None
  | { Xsd.au_use = (Xsd.Optional | Xsd.Required) as use;
      Xsd.au_ad = ad_ref;
      Xsd.au_value_constraint = v_c_opt } ->
        let required = match use with
        | Xsd.Optional -> false
        | Xsd.Required -> true
        | Xsd.Prohibited ->
            Message.error (Error.A_function "Xsd_lineariser.linearise_au")
        in
        let env2,ad_id =
          linearise_ad_ref env1 ad_ref in
        let v_c_opt' =
          Lib.opt_apply value_constraint_of_value_constraint v_c_opt in
        env2,
        (Some { Lxsd.au_required = required;
                Lxsd.au_ad = ad_id;
                Lxsd.au_value_constraint = v_c_opt' })

(** Linearise a list of XSD attribute uses into a list of LXSD
    attribute group ids. Used by the linearisation of complex type
    declaration. *)
and linearise_ctd_au env1 aus_l :
    Env.t * Lxsd.agd_id list =
  let au_l,agd_ref_l =
    aus_partition aus_l in
  (* we create an anonymous group containing the attribute uses of the CTD *)
  let env2,au_l' =
    List.fold_left
      (fun (env,l) au ->
        match linearise_au env au with
        | env',None -> env',l
        | env',(Some au') ->
            env',(au'::l))
      (env1,[])
      au_l
  in
  let env3,agd_id =
    Env.new_agd
      None
      { Lxsd.agd_uses = Lxsd.AU_unresolved (au_l',[]);
        Lxsd.agd_w = None
          (* TODO when implementating anyAttribute, this value should be
             informed *) }
      env2
  in
  (* we linearise the attributeGroup references *)
  let env4,agd_ref_l' =
    List.fold_left
      (fun (env,l) { Xml.uri = uri; Xml.name = name } ->
        let env',agd_id =
          Env.new_agd_id
            ((Xsd_lib.to_uri uri),
             (Xsd_lib.to_ncname Xml_lexer.is_ncname name))
            env in
        env',(agd_id::l) )
      (env3,[])
      agd_ref_l
  in
  env4,(agd_id::agd_ref_l')

(** Linearise a list of XSD attribute uses into a list of LXSD
    attribute uses. Used by the linearisation of attribute group
    declaration. *)
and linearise_agd_uses env1 aus_l :
    Env.t * Lxsd.au_unresolved =
  let au_l,agd_ref_l =
    aus_partition aus_l in
  (* we linearise the attribute uses *)
  let env2,au_l' =
    List.fold_left
      (fun (env,l) au ->
        match linearise_au env au with
        | env',None -> env',l
        | env',(Some au') ->
            env',(au'::l))
      (env1,[])
      au_l
  in
  (* we linearise the attributeGroup references *)
  let env3,agd_ref_l' =
    List.fold_left
      (fun (env,l) { Xml.uri = uri; Xml.name = name } ->
        let env',agd_id =
          Env.new_agd_id
            ((Xsd_lib.to_uri uri),
             (Xsd_lib.to_ncname Xml_lexer.is_ncname name))
            env in
        env',(agd_id::l) )
      (env2,[])
      agd_ref_l
  in
  env3,(Lxsd.AU_unresolved (au_l',agd_ref_l'))


and linearise_agd env1 = function
  | { Xsd.agd_name = _;
      Xsd.agd_target = _;
      Xsd.agd_uses = _;
      Xsd.agd_w = Some _ } ->
        Message.error
          Error.XSDL_XSV_LIM_attribute_group_wildcard_not_implemented
  | { Xsd.agd_name = ncname;
      Xsd.agd_target = target;
      Xsd.agd_uses = aus_l;
      Xsd.agd_w = w_opt } ->
        let env2,aus_l' =
          linearise_agd_uses env1 aus_l
        in
        let env3,w_opt' =
          match w_opt with
          | None -> env2,None
          | Some (w,__i) ->
              (* TODO take into account the document order when
                 implementing anyAttribute *)
              let env2',w' = linearise_w env2 w in
              env2',(Some w')
        in
        let agd' =
          { Lxsd.agd_uses = aus_l';
            Lxsd.agd_w = w_opt' }
        in
        Env.new_agd (Some (target,ncname)) agd' env3

and linearise_ctd env1 = function
  | { Xsd.ctd_name_or_context = ctd_name_or_context;
      Xsd.ctd_target = target;
      Xsd.ctd_base = ctd_ref;
      Xsd.ctd_dm = dm;
      Xsd.ctd_au = aus_l;
      Xsd.ctd_aw = aw_opt;
      Xsd.ctd_ct = ct } ->
        let env2,ct' =
          linearise_ct env1 ct in
        let env3,ctd_ref' =
          linearise_ctd_ref_un env2 ctd_ref in
        let dm' =
          dm_of_dm dm in
        let env4,agd_ref_l' =
          linearise_ctd_au env3 aus_l
        in
        let env6,aw_opt' =
          match aw_opt with
          | None -> env4,None
          | Some (w,__i) ->
              (* TODO take into account the document order when
                 implementing anyAttribute *)
              let env5,w' = linearise_w env4 w in
              env5,(Some w')
        in
        let td =
          Lxsd.CTD
            { Lxsd.ctd_au = agd_ref_l';
              Lxsd.ctd_aw = aw_opt';
              Lxsd.ctd_ct =
              { Lxsd.ctd_base_un = ctd_ref';
                Lxsd.ctd_dm_un = dm';
                Lxsd.ctd_variety_un = ct' } }
        in
        let env7,td_id =
          match ctd_name_or_context with
          | Xsd.CTD_name ncname ->
              Env.new_td (Some (target,ncname)) td env6
          | Xsd.CTD_context ->
              Env.new_td None td env6
        in
        let env8 =
          Env.add_ctd_dependency (td_id,ctd_ref') env7
        in
       env8,td_id


and linearise_ctd_ref env1 = function
  | Xsd.CTD_anyType ->
      env1, Lxsd.CTD_anyType
  | Xsd.CTD_td_ref td_ref ->
      let env2,td_id =
        linearise_td_ref env1 td_ref in
      env2, (Lxsd.CTD_td_ref td_id)
  | Xsd.CTD_simpleContentType ->
      Message.error (Error.A_function "Xsd_lineariser.linearise_ctd_ref")
  | Xsd.CTD_built_in_simpleType st ->
      let st' = linearise_built_in_type st in
      (*
        Alternative if we were to have Lxsd.CTD_built_in_simpleType

        env1,Lxsd.CTD_built_in_simpleType st'
       *)
      let std_variety =
        Lxsd.STD_atomic (Lxsd.STD_built_in st') in
      let env2,td_id =
        Env.new_td
          None
          (Lxsd.STD
             { Lxsd.std_details =
               { Lxsd.std_facets = [];
                 Lxsd.std_variety = std_variety } } )
          env1
      in
      let env3 =
        Env.add_std_dependency (td_id,std_variety) env2  in
      env3,(Lxsd.CTD_td_ref td_id)

and linearise_ctd_ref_un env1 = function
  | Xsd.CTD_anyType ->
      env1, Lxsd.CTD_anyType_un
  | Xsd.CTD_td_ref td_ref ->
      let env2,td_id =
        linearise_td_ref env1 td_ref in
      env2, (Lxsd.CTD_td_ref_un td_id)
  | Xsd.CTD_simpleContentType ->
      env1,Lxsd.CTD_simpleContentType_un
  | Xsd.CTD_built_in_simpleType st ->
      let st' = linearise_built_in_type st in
      (*
        Alternative if we were to have Lxsd.CTD_built_in_simpleType

        env1,Lxsd.CTD_built_in_simpleType_un st'
       *)
      let std_variety =
        Lxsd.STD_atomic (Lxsd.STD_built_in st') in
      let env2,td_id =
        Env.new_td
          None
          (Lxsd.STD
             { Lxsd.std_details =
               { Lxsd.std_facets = [];
                 Lxsd.std_variety = std_variety } } )
          env1
      in
      let env3 =
        Env.add_std_dependency (td_id,std_variety) env2  in
      env3,(Lxsd.CTD_td_ref_un td_id)




and linearise_ct env1 = function
  | Xsd.CT_empty ->
      env1,Lxsd.CT_empty_un
  | Xsd.CT_simple std ->
      let env2,td_id =
        linearise_std env1 std in
      env2,(Lxsd.CT_simple_un td_id)
  | Xsd.CT_element_only p ->
      let env2,p' =
        linearise_p env1 p in
      env2,(Lxsd.CT_element_only_un p')
  | Xsd.CT_mixed p ->
      let env2,p' =
        linearise_p env1 p in
      env2,(Lxsd.CT_mixed_un p')

and linearise_p env1 = function
  | { Xsd.p_occurrence = occurrence;
      Xsd.p_term = t } ->
        let env2,t' =
          linearise_t env1 t in
        let occurrence' =
          occurrence_of_occurrence occurrence in
        env2,
        { Lxsd.p_occurrence = occurrence';
          Lxsd.p_term = t' }

and linearise_t env1 = function
  | Xsd.T_ed ed_ref ->
      let env2,ed_id = linearise_ed_ref env1 ed_ref in
      env2, (Lxsd.T_ed ed_id)
  | Xsd.T_mg mg ->
      let env2,mg' = linearise_mg env1 mg in
      let env3,mg_id = Env.new_mgd None mg' env2 in
      env3,(Lxsd.T_mgd mg_id)
  | Xsd.T_mgd mgd_ref ->
      let env2,mgd_id =
        linearise_mgd_ref env1 mgd_ref in
      env2,(Lxsd.T_mgd mgd_id)
  | Xsd.T_w w ->
      let env2,w' =
        linearise_w env1 w in
      env2,(Lxsd.T_w w')

and linearise_w env1 = function
  | { Xsd.w_nc = nc;
      Xsd.w_pc = pc } ->
        let env2,nc' =
          linearise_nc env1 nc in
        let pc' = pc_of_pc pc in
        env2,
        { Lxsd.w_nc = nc';
          Lxsd.w_pc = pc' }



and linerarise_uri_opt_l env1 uri_opt_l =
  List.fold_left
    (fun (env,l) uri_opt ->
      let uri =
        Lib.opt_default
          (Xsd.Uri Xml_ns.empty_expanded_ns)
          uri_opt in
      let env',uri_id =
        Env.new_uri uri env in
      env',(uri_id::l)
    )
    (env1,([] : Lxsd.uri_id list))
    uri_opt_l

and linearise_nc_disallowed_list env1 nc_d_list =
  List.fold_left
    (fun (env,l) ncd ->
      let env',ncd' =
        linearise_nc_disallowed env ncd in
      env',(ncd'::l))
    (env1,[])
    nc_d_list

and linearise_nc env1 = function
  | Xsd.NC_any nc_d_l ->
      let env2,nc_d_l' =
        linearise_nc_disallowed_list env1 nc_d_l in
      env2,Lxsd.NC_any nc_d_l'
  | Xsd.NC_enumeration (uri_opt_l, nc_d_l) ->
      let env2,nc_d_l' =
        linearise_nc_disallowed_list env1 nc_d_l in
      let env3,uri_id_l =
        linerarise_uri_opt_l env2 uri_opt_l in
      env3,Lxsd.NC_enumeration (uri_id_l, nc_d_l')
  | Xsd.NC_not (uri_opt_l,nc_d_l) ->
      let env2,nc_d_l' =
        linearise_nc_disallowed_list env1 nc_d_l in
      let env3,uri_id_l =
        linerarise_uri_opt_l env2 uri_opt_l in
      env3,Lxsd.NC_not (uri_id_l,nc_d_l')


and linearise_nc_disallowed env1 = function
  | Xsd.NC_qname qname ->
      let env2,_ = Env.new_uri (Xsd.Uri qname.Xml.uri) env1 in
      env2,(Lxsd.NC_qname qname)
  | Xsd.NC_defined ->
      env1,Lxsd.NC_defined
  | Xsd.NC_sibling ->
      env1,Lxsd.NC_sibling

and linearise_ed_ref env1 = function
  | Xsd.Val ed -> linearise_ed env1 ed
  | Xsd.Ref { Xml.uri = uri; Xml.name = name } ->
      Env.new_ed_id ((Xsd_lib.to_uri uri),(Xsd_lib.to_ncname Xml_lexer.is_ncname name)) env1

and linearise_mgd_ref env1 = function
  | Xsd.Val mgd -> linearise_mgd env1 mgd
  | Xsd.Ref { Xml.uri = uri; Xml.name = name } ->
      Env.new_mgd_id ((Xsd_lib.to_uri uri),(Xsd_lib.to_ncname Xml_lexer.is_ncname name)) env1

and linearise_mg env1 = function
  | Xsd.MG_all p_l ->
      let env2,p_l' = linearise_p_l env1 p_l in
      env2,(Lxsd.MG_all p_l')
  | Xsd.MG_choice p_l ->
      let env2,p_l' = linearise_p_l env1 p_l in
      env2,(Lxsd.MG_choice p_l')
  | Xsd.MG_sequence p_l ->
      let env2,p_l' = linearise_p_l env1 p_l in
      env2,(Lxsd.MG_sequence p_l')

and linearise_p_l env1 p_l1 =
  let env2,rev_p_l2 =
    List.fold_left
      (fun (env,p_l) p ->
        let env',p' = linearise_p env p in
        env', p'::p_l)
      (env1,[])
      p_l1 in
  env2, (List.rev rev_p_l2)

and linearise_mgd env1 = function
  | { Xsd.mgd_name = ncname;
      Xsd.mgd_target = target;
      Xsd.mgd_mg = mg } ->
        let env2,mg' = linearise_mg env1 mg in
        Env.new_mgd  (Some (target,ncname)) mg' env2

let linearise_definition env1 = function
  | Xsd.ED ed ->
      let env2,_ = linearise_ed env1 ed in
      env2
  | Xsd.TD td ->
      let env2,_ = linearise_td env1 td in
      env2
  | Xsd.AD ad ->
      let env2,_ = linearise_ad env1 ad in
      env2
  | Xsd.AGD agd ->
      let env2,_ = linearise_agd env1 agd in
      env2
  | Xsd.MGD mgd ->
      let env2,_ = linearise_mgd env1 mgd in
      env2




(** {2 XSD determinism test functions} *)

module S_ed_id =
  Set.Make (struct type t = Lxsd.ed_id let compare = compare end)


(** Test element definition determinism *)
let test_determinism_ed =
  let _deterministic_ed = S_ed_id.empty in
  fun _lxsd _ed_id ->
    Message.warning Error.XSDL_LIM_determinism_check_not_implemented;
    true


(** Test linearised XSD's determinism *)
let test_determinism lxsd =
  match lxsd with
  | { Lxsd.target = _uri_id;
      Lxsd.uris = _uris;
      Lxsd.tds = _tds;
      Lxsd.ads = _ads;
      Lxsd.eds = _eds;
      Lxsd.agds = _agds;
      Lxsd.mgds = _mgds;
      Lxsd.roots = roots } ->
        test_determinism_ed lxsd roots


(** {2 Facets inheritance} *)

module M_tds =
  Map.Make (struct type t = Lxsd.td_id let compare = compare end)

(** Combine whitespace facets. *)
let combine_whitespace w1 w2 =
  if w2 >= w1
  then w2
  else
    Message.error Error.XSDL_whitespace_values_not_combinable

(** Combine length bounds. *)
let combine_length_bounds
    (Lxsd.Bounds (min1_opt,max1_opt))
    (Lxsd.Bounds (min2_opt,max2_opt)) =
  let min_opt =
    match min1_opt,min2_opt with
    | None,None -> None
    | (Some i),None | None,(Some i) ->
        Some i
    | (Some i),(Some j) ->
        Some (max i j)
  in
  let max_opt =
    match max1_opt,max2_opt with
    | None,None -> None
    | (Some i),None | None,(Some i) ->
        Some i
    | (Some i),(Some j) ->
        Some (min i j)
  in
  Lxsd.Bounds (min_opt,max_opt)

(** Combine value bounds. *)
let combine_value_bounds
    (Lxsd.Bounds (min1,max1))
    (Lxsd.Bounds (min2,max2)) : Lxsd.value_bounds =
  Lxsd.Bounds
    ((min1 @ min2), (max1 @ max2))

(** Get the length counter for a given primitive simple type. *)
let get_length_counter = function
  | Env.ST_p
      (Lxsd.PT_string | Lxsd.PT_anyURI)
  | Env.ST_np
      (Lxsd.NPT_normalizedString | Lxsd.NPT_token | Lxsd.NPT_language |
      Lxsd.NPT_NMTOKEN | Lxsd.NPT_Name | Lxsd.NPT_NCName |
      Lxsd.NPT_ID | Lxsd.NPT_IDREF | Lxsd.NPT_ENTITY) ->
        Some Lxsd.Counter_char
  | Env.ST_p Lxsd.PT_hexBinary ->
      Some Lxsd.Counter_hex
  | Env.ST_p  Lxsd.PT_base64Binary ->
      Some Lxsd.Counter_base64
  | Env.ST_p (Lxsd.PT_QName | Lxsd.PT_NOTATION) ->
      None
  | Env.ST_list
  | Env.ST_np
      (Lxsd.NPT_IDREFS | Lxsd.NPT_ENTITIES | Lxsd.NPT_NMTOKENS) ->
        (* List length counter are dealt with separately. *)
        None
  | Env.ST_union_with_list
  | Env.ST_union_without_list
  | Env.ST_no_primitive
  | Env.CT
  | Env.ST_p
      (Lxsd.PT_boolean | Lxsd.PT_decimal | Lxsd.PT_float |
       Lxsd.PT_double | Lxsd.PT_duration | Lxsd.PT_dateTime |
       Lxsd.PT_time | Lxsd.PT_date | Lxsd.PT_gYearMonth | Lxsd.PT_gYear |
       Lxsd.PT_gMonthDay | Lxsd.PT_gDay | Lxsd.PT_gMonth)
  | Env.ST_np
      (Lxsd.NPT_integer | Lxsd.NPT_nonPositiveInteger |
      Lxsd.NPT_negativeInteger | Lxsd.NPT_long | Lxsd.NPT_int |
      Lxsd.NPT_short | Lxsd.NPT_byte | Lxsd.NPT_nonNegativeInteger |
      Lxsd.NPT_unsignedLong | Lxsd.NPT_unsignedInt |
      Lxsd.NPT_unsignedShort | Lxsd.NPT_unsignedByte |
      Lxsd.NPT_positiveInteger | Lxsd.NPT_yearMonthDuration |
      Lxsd.NPT_dayTimeDuration | Lxsd.NPT_dateTimeStamp) ->
        Message.error Error.XSDL_counter_not_applicable

(** Get the order for a given primitive simple type. *)
let get_order = function
  | Env.ST_p (Lxsd.PT_decimal | Lxsd.PT_double | Lxsd.PT_float) ->
      Some Lxsd.Order_float
  | Env.ST_np
      (Lxsd.NPT_integer | Lxsd.NPT_nonPositiveInteger |
      Lxsd.NPT_negativeInteger | Lxsd.NPT_long |
      Lxsd.NPT_int | Lxsd.NPT_short | Lxsd.NPT_byte |
      Lxsd.NPT_nonNegativeInteger | Lxsd.NPT_unsignedLong |
      Lxsd.NPT_unsignedInt | Lxsd.NPT_unsignedShort |
      Lxsd.NPT_unsignedByte | Lxsd.NPT_positiveInteger) ->
      Some Lxsd.Order_int
  | Env.ST_union_with_list
  | Env.ST_union_without_list
  | Env.ST_list
  | Env.ST_no_primitive
  | Env.CT
  | Env.ST_p
      (Lxsd.PT_string | Lxsd.PT_boolean | Lxsd.PT_hexBinary |
      Lxsd.PT_base64Binary | Lxsd.PT_anyURI | Lxsd.PT_QName |
      Lxsd.PT_NOTATION)
  | Env.ST_np
      (Lxsd.NPT_normalizedString | Lxsd.NPT_token | Lxsd.NPT_language |
      Lxsd.NPT_NMTOKEN | Lxsd.NPT_Name | Lxsd.NPT_NCName | Lxsd.NPT_ID |
      Lxsd.NPT_IDREF | Lxsd.NPT_ENTITY | Lxsd.NPT_IDREFS |
      Lxsd.NPT_ENTITIES | Lxsd.NPT_NMTOKENS) ->
        Message.error Error.XSDL_order_not_applicable
  | Env.ST_p
      (Lxsd.PT_duration | Lxsd.PT_dateTime |
       Lxsd.PT_time | Lxsd.PT_date | Lxsd.PT_gYearMonth | Lxsd.PT_gYear |
       Lxsd.PT_gMonthDay | Lxsd.PT_gDay | Lxsd.PT_gMonth)
  | Env.ST_np
      (Lxsd.NPT_yearMonthDuration | Lxsd.NPT_dayTimeDuration |
      Lxsd.NPT_dateTimeStamp) ->
        Message.error Error.XSDL_LIM_order_not_implemented

(** Combine item length facets. *)
let combine_item_length
    (f_length : (Lxsd.length_counter * int option Lxsd.bounds) option )
    (bounds : int option Lxsd.bounds )
    (primitive_type : Env.primitive_t )
    : (Lxsd.length_counter * int option Lxsd.bounds) option
    =
  match f_length with
  | None ->
      begin
        match get_length_counter primitive_type with
        | None ->
            None
        | Some length_counter ->
            Some (length_counter, bounds)
      end
  | Some (f,pre_bounds) ->
      Some (f,combine_length_bounds pre_bounds bounds)

let combine_list_length
    (f_length : (int option Lxsd.bounds) option )
    (bounds : int option Lxsd.bounds )
    (primitive_type : Env.primitive_t )
    : (int option Lxsd.bounds) option
    =
  match primitive_type with
  | Env.ST_list
  | Env.ST_np Lxsd.NPT_NMTOKENS
  | Env.ST_np Lxsd.NPT_IDREFS
  | Env.ST_np Lxsd.NPT_ENTITIES ->
      begin
        match f_length with
        | None ->
            Some bounds
        | Some pre_bounds ->
            Some (combine_length_bounds pre_bounds bounds)
      end
  | Env.ST_p _
  | Env.ST_np
      (Lxsd.NPT_normalizedString | Lxsd.NPT_token | Lxsd.NPT_language |
      Lxsd.NPT_NMTOKEN | Lxsd.NPT_Name | Lxsd.NPT_NCName |
      Lxsd.NPT_ID | Lxsd.NPT_IDREF | Lxsd.NPT_ENTITY |
      Lxsd.NPT_integer | Lxsd.NPT_nonPositiveInteger |
      Lxsd.NPT_negativeInteger | Lxsd.NPT_long | Lxsd.NPT_int |
      Lxsd.NPT_short | Lxsd.NPT_byte | Lxsd.NPT_nonNegativeInteger |
      Lxsd.NPT_unsignedLong | Lxsd.NPT_unsignedInt |
      Lxsd.NPT_unsignedShort | Lxsd.NPT_unsignedByte |
      Lxsd.NPT_positiveInteger | Lxsd.NPT_yearMonthDuration |
      Lxsd.NPT_dayTimeDuration | Lxsd.NPT_dateTimeStamp )
  | Env.CT
  | Env.ST_no_primitive
  | Env.ST_union_with_list
  | Env.ST_union_without_list ->
      Message.error (Error.A_function "Xsd_linearise.combine_list_length")

(** Combine facets record and whitespace facet. *)
let combine_facets
    (combine_length_fun :
       'a option -> int option Lxsd.bounds ->
         Env.primitive_t ->
         'a option
 )
    primitive_type
    ({ Lxsd.f_length = f_length;
       Lxsd.f_pattern = f_pattern;
       Lxsd.f_enumeration = f_enumeration;
       Lxsd.f_value = f_value },
     whitespace) = function
       | Lxsd.CF_length (i,_) ->
           let bounds =
             Lxsd.Bounds ((Some i),(Some i)) in
           let f_length' =
             combine_length_fun f_length bounds primitive_type
           in
           { Lxsd.f_length = f_length';
             Lxsd.f_pattern = f_pattern;
             Lxsd.f_enumeration = f_enumeration;
             Lxsd.f_value = f_value },
           whitespace
       | Lxsd.CF_minLength (i,_) ->
           let bounds =
             Lxsd.Bounds ((Some i),None) in
           let f_length' =
             combine_length_fun f_length bounds primitive_type
           in
           { Lxsd.f_length = f_length';
             Lxsd.f_pattern = f_pattern;
             Lxsd.f_enumeration = f_enumeration;
             Lxsd.f_value = f_value },
           whitespace
       | Lxsd.CF_maxLength (i,_) ->
           let bounds =
             Lxsd.Bounds (None,(Some i)) in
           let f_length' =
             combine_length_fun f_length bounds primitive_type
           in
           { Lxsd.f_length = f_length';
             Lxsd.f_pattern = f_pattern;
             Lxsd.f_enumeration = f_enumeration;
             Lxsd.f_value = f_value },
           whitespace
       | Lxsd.CF_pattern (regexp,_) ->
           { Lxsd.f_length = f_length;
             Lxsd.f_pattern = (Lxsd.RE_regexp regexp)::f_pattern;
             Lxsd.f_enumeration = f_enumeration;
             Lxsd.f_value = f_value },
           whitespace
       | Lxsd.CF_enumeration sd_l ->
           let f_enumeration' =
             match f_enumeration with
             | None -> Some sd_l
             | Some pre_sd_l -> Some (sd_l @ pre_sd_l)
           in
           { Lxsd.f_length = f_length;
             Lxsd.f_pattern = f_pattern;
             Lxsd.f_enumeration = f_enumeration';
             Lxsd.f_value = f_value },
           whitespace
       | Lxsd.CF_maxInclusive (sd,_) ->
           let bounds =
             Lxsd.Bounds ([],[Lxsd.Inclusive sd]) in
           let f_value' =
             match f_value with
             | None ->
                 begin
                   match get_order primitive_type with
                   | None ->
                       None
                   | Some order ->
                       Some (order, bounds)
                 end
             | Some (f,pre_bounds) ->
                 Some (f,combine_value_bounds pre_bounds bounds)
           in
           { Lxsd.f_length = f_length;
             Lxsd.f_pattern = f_pattern;
             Lxsd.f_enumeration = f_enumeration;
             Lxsd.f_value = f_value' },
           whitespace
       | Lxsd.CF_maxExclusive (sd,_) ->
           let bounds =
             Lxsd.Bounds ([],[Lxsd.Exclusive sd]) in
           let f_value' =
             match f_value with
             | None ->
                 begin
                   match get_order primitive_type with
                   | None ->
                       None
                   | Some order ->
                       Some (order, bounds)
                 end
             | Some (f,pre_bounds) ->
                 Some (f,combine_value_bounds pre_bounds bounds)
           in
           { Lxsd.f_length = f_length;
             Lxsd.f_pattern = f_pattern;
             Lxsd.f_enumeration = f_enumeration;
             Lxsd.f_value = f_value' },
           whitespace
       | Lxsd.CF_minExclusive (sd,_) ->
           let bounds =
             Lxsd.Bounds ([Lxsd.Exclusive sd],[]) in
           let f_value' =
             match f_value with
             | None ->
                 begin
                   match get_order primitive_type with
                   | None ->
                       None
                   | Some order ->
                       Some (order, bounds)
                 end
             | Some (f,pre_bounds) ->
                 Some (f,combine_value_bounds pre_bounds bounds)
           in
           { Lxsd.f_length = f_length;
             Lxsd.f_pattern = f_pattern;
             Lxsd.f_enumeration = f_enumeration;
             Lxsd.f_value = f_value' },
           whitespace
       | Lxsd.CF_minInclusive (sd,_) ->
           let bounds =
             Lxsd.Bounds ([Lxsd.Inclusive sd],[]) in
           let f_value' =
             match f_value with
             | None ->
                 begin
                   match get_order primitive_type with
                   | None ->
                       None
                   | Some order ->
                       Some (order, bounds)
                 end
             | Some (f,pre_bounds) ->
                 Some (f,combine_value_bounds pre_bounds bounds)
           in
           { Lxsd.f_length = f_length;
             Lxsd.f_pattern = f_pattern;
             Lxsd.f_enumeration = f_enumeration;
             Lxsd.f_value = f_value' },
           whitespace
       | Lxsd.CF_whiteSpace (w,_) ->
           let whitespace' =
             combine_whitespace whitespace w in
           { Lxsd.f_length = f_length;
             Lxsd.f_pattern = f_pattern;
             Lxsd.f_enumeration = f_enumeration;
             Lxsd.f_value = f_value },
           whitespace'


(** Get the facet details of a base type definition from the
    linearised type table. *)
let get_pre_details std_ref type_table =
  match std_ref with
  | Lxsd.STD_built_in st ->
      Xsd_lib.get_built_in_st_details st
  | Lxsd.STD_td_ref td_id ->
      try
        begin
          match M_tds.find td_id type_table with
          | Lxsd.STD { Lxsd.std_details = details } ->
                         details
          | Lxsd.CTD _ ->
              Message.error
                (Error.A_function "Xsd_lineariser.get_pre_details")
        end
      with
      | Not_found ->
          Message.error (Error.A_function "Xsd_lineariser.get_pre_details")





let resolve_st type_table primitive_type st =
  match st with
  | { Lxsd.std_facets = cfacets;
      Lxsd.std_variety = Lxsd.STD_atomic std_ref } ->
        (* Atomic type (syntactically) *)
        begin
          match get_pre_details std_ref type_table with
          | { Lxsd.lstd_whitespace = _;
              Lxsd.lstd_members = [] } ->
                Message.error Error.A_empty_members_list
          | { Lxsd.lstd_whitespace = pre_whitespace;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic pre_facets ] } ->
                (* based on another atomic type: we combine the new
                   and previous facets *)
                let facets,whitespace =
                  List.fold_left
                    (combine_facets combine_item_length primitive_type)
                    (pre_facets,pre_whitespace)
                    cfacets
                in
                { Lxsd.lstd_whitespace = whitespace;
                  Lxsd.lstd_members =
                  [ Lxsd.LSTD_atomic facets ] }
          | { Lxsd.lstd_whitespace = _;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_list (list_facets,item_facets) ] } ->
                (* based on a list: we update the list's facets with
                   the current's facets *)
                let list_facets',_ =
                  List.fold_left
                    (combine_facets combine_list_length primitive_type)
                    (list_facets,Lxsd.W_collapse)
                    cfacets
                in
                { Lxsd.lstd_whitespace = Lxsd.W_collapse;
                  Lxsd.lstd_members =
                  [ Lxsd.LSTD_list (list_facets',item_facets) ] }
          | { Lxsd.lstd_whitespace = _;
              Lxsd.lstd_members = _::_::_ } ->
                (* based on a union: we keep only the new facets which are
                   restricting the others *)
                let member_facets,member_whitespace =
                  List.fold_left
                    (combine_facets combine_item_length primitive_type)
                    (Xsd_lib.empty_item_facets,Xsd_lib.empty_item_whitespace)
                    cfacets
                in
                let new_member =
                  Lxsd.LSTD_atomic member_facets in
                { Lxsd.lstd_whitespace = member_whitespace;
                  Lxsd.lstd_members = [new_member] }
        end
  | { Lxsd.std_facets = cfacets;
      Lxsd.std_variety = Lxsd.STD_list item_std_ref } ->
        (* List type (syntactically) *)
        begin
          match get_pre_details item_std_ref type_table with
          | { Lxsd.lstd_whitespace = _;
              Lxsd.lstd_members = [] } ->
                Message.error Error.A_empty_members_list
          | { Lxsd.lstd_whitespace = _;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_atomic item_facets ] } ->
                if primitive_type = Env.ST_union_with_list
                then
                  Message.error Error.XSDL_simple_type_derivation_invalid
                else
                  (* based on another atomic type: we create a list type
                     with the current facets as list's facets *)
                  let list_facets,_ =
                    List.fold_left
                      (combine_facets combine_list_length primitive_type)
                      (Xsd_lib.empty_list_facets,Xsd_lib.empty_list_whitespace)
                      cfacets
                  in
                  { Lxsd.lstd_whitespace = Lxsd.W_collapse;
                    Lxsd.lstd_members =
                    [ Lxsd.LSTD_list (list_facets,item_facets) ] }
          | { Lxsd.lstd_whitespace = _;
              Lxsd.lstd_members =
              [ Lxsd.LSTD_list _ ] } ->
                (* based on a list *)
                Message.error Error.XSDL_simple_type_derivation_invalid
          | { Lxsd.lstd_whitespace = _;
              Lxsd.lstd_members = _::_::_ } ->
                (* based on an union *)
                Message.error Error.XSDL_simple_type_derivation_invalid
        end
  | { Lxsd.std_facets = [];
      Lxsd.std_variety = Lxsd.STD_union std_ref_list } ->
        (* Union type (syntactically) *)
        begin
          let member_facets_list =
            List.fold_left
              (fun m_list std_ref ->
                  match get_pre_details std_ref type_table with
                  | { Lxsd.lstd_whitespace = _;
                      Lxsd.lstd_members = [] } ->
                        Message.error Error.A_empty_members_list
                  | { Lxsd.lstd_whitespace = _;
                      Lxsd.lstd_members = members_list } ->
                        m_list @ members_list )
              []
              std_ref_list
          in
          { Lxsd.lstd_whitespace = Xsd_lib.empty_item_whitespace;
            Lxsd.lstd_members = member_facets_list }
        end
  | { Lxsd.std_facets = _::_;
      Lxsd.std_variety = Lxsd.STD_union _ } ->
        Message.error (Error.A_function "Xsd_lineariser.resolve_st")


(** Get the complex type variety of a base type definition from the
    linearised type table. *)
let get_pre_ct td_id type_table =
  try
    begin
      match M_tds.find td_id type_table with
      | Lxsd.CTD ct ->
          ct.Lxsd.ctd_ct
      | Lxsd.STD _ ->
          Lxsd.CT_simple td_id
    end
  with
  | Not_found ->
      Message.error (Error.A_function "Xsd_lineariser.get_pre_ct")

let new_mgd mgd mgds : Lxsd.mgd_id * Lxsd.mgd Env.M_mgds.t =
  let id =
    try
      begin
        let Lxsd.Id_mgd max_id,_ =
          Env.M_mgds.max_binding mgds in
        if max_id + 1 = max_int
        then
          Message.error Error.ML_max_int_reached
        else
          Lxsd.Id_mgd (max_id + 1)
      end
    with
    | Not_found ->
        Lxsd.Id_mgd 1
  in
  id,(Env.M_mgds.add id mgd mgds)

let sequence_of_particles mgds p_base p =
  let mgd_id,mgds' =
    new_mgd
      (Lxsd.MG_sequence [p_base;p])
      mgds
  in
  { Lxsd.p_occurrence = { Lxsd.min_occurs = 1;
                          Lxsd.max_occurs = Lxsd.Occur 1 };
    Lxsd.p_term = (Lxsd.T_mgd mgd_id) },
  mgds'


(** Determine the explicit content of a particle.
    Case 2 in {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#dcl.ctd.ctcc.common}REF}. *)
let explicit_content mgds p =
  match p with
  | { Lxsd.p_occurrence = _;
      Lxsd.p_term = (Lxsd.T_ed _| Lxsd.T_w _) }
      (* case 2.1.1 *)
  | { Lxsd.p_occurrence = { Lxsd.min_occurs = _;
                            Lxsd.max_occurs = Lxsd.Occur 0 };
      Lxsd.p_term = Lxsd.T_mgd _ }
      (* case 2.1.4 *)
    -> None
  | { Lxsd.p_occurrence = { Lxsd.min_occurs = _;
                              Lxsd.max_occurs = (Lxsd.Occur _|Lxsd.Unbounded)};
      Lxsd.p_term = Lxsd.T_mgd mgd_id } ->
        let mgd =
          try
            Env.M_mgds.find mgd_id mgds
          with
          | Not_found -> Message.error (Error.A_function "Xsd_lineariser.explicit_content")
        in
        begin
          match mgd with
          | Lxsd.MG_all []
          | Lxsd.MG_sequence [] ->
              (* case 2.1.2 *)
              None
          | Lxsd.MG_choice [] ->
            if p.Lxsd.p_occurrence.Lxsd.min_occurs = 0
            then
              (* case 2.1.3 *)
              None
            else
              (* case 2.2 *)
              Some p
          | Lxsd.MG_all _ | Lxsd.MG_sequence _ | Lxsd.MG_choice _ ->
              (* case 2.2 *)
              Some p
        end

(** Determine the effective content of a particle.
    Case 3 in {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#dcl.ctd.ctcc.common}REF}. *)
let effective_content effective_mixed mgds p =
  match explicit_content mgds p with
  | None ->
      (* case 3.1 *)
      if effective_mixed
      then
        (* case 3.1.1 *)
        let mgd_id,mgds' =
          new_mgd
            (Lxsd.MG_sequence [])
            mgds
        in
        (Some {Lxsd.p_occurrence = { Lxsd.min_occurs = 1;
                                     Lxsd.max_occurs = Lxsd.Occur 1 };
               Lxsd.p_term = (Lxsd.T_mgd mgd_id) }),
        mgds'
      else
        (* case 3.1.2 *)
        None,mgds
  | Some p' ->
      (* case 3.2 *)
      (Some p'),mgds

let get_all_content mgds p =
  match p with
  | { Lxsd.p_occurrence = _;
      Lxsd.p_term = (Lxsd.T_ed _| Lxsd.T_w _) } ->
        None
  | { Lxsd.p_occurrence = _;
      Lxsd.p_term = Lxsd.T_mgd mgd_id } ->
        let mgd =
          try
            Env.M_mgds.find mgd_id mgds
          with
          | Not_found -> Message.error (Error.A_function "Xsd_lineariser.get_all_content")
        in
        begin
          match mgd with
          | Lxsd.MG_all l ->
              Some l
          | Lxsd.MG_sequence _ ->
              None
          | Lxsd.MG_choice _ ->
              None
        end


(** Extend a base particle with a new particle.
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#dcl.ctd.ctcc.common}REF} *)
let extend_particles effective_mixed mgds0 p_base p =
  match effective_content effective_mixed mgds0 p with
  | None,mgds1 ->
      (* case 4.2.2 *)
      p_base,mgds1
  | (Some effective_p),mgds1 ->
      (* case 4.2.3 *)
      match p_base with
      | { Lxsd.p_occurrence = _;
          Lxsd.p_term = Lxsd.T_mgd mgd_id } ->
            begin
              try
                begin
                  match Env.M_mgds.find mgd_id mgds1 with
                  | Lxsd.MG_all p_base_list ->
                      begin
                        match explicit_content mgds1 p with
                        | None ->
                            (* 4.2.3.1 *)
                            p_base,mgds1
                        | Some _ ->
                            begin
                              match get_all_content mgds1 effective_p with
                              | Some l ->
                                  (* case 4.2.3.2 *)
                                  let mgd_id,mgds2 =
                                    new_mgd
                                      (Lxsd.MG_all (p_base_list @ l))
                                      mgds1
                                  in
                                  { Lxsd.p_occurrence =
                                    { Lxsd.min_occurs =
                                      effective_p.Lxsd.p_occurrence.Lxsd.min_occurs;
                                      Lxsd.max_occurs = Lxsd.Occur 1 };
                                    Lxsd.p_term = Lxsd.T_mgd mgd_id },
                                  mgds2
                              | None ->
                                  (* case 4.2.3.3 *)
                                  sequence_of_particles mgds1 p_base effective_p
                            end
                      end
                  | Lxsd.MG_choice _
                  | Lxsd.MG_sequence _ ->
                      (* case 4.2.3.3 *)
                      sequence_of_particles mgds1 p_base effective_p
                end
              with
              | Not_found ->
                  Message.error (Error.A_function "Xsd_lineariser.extend_particles")
            end
      | { Lxsd.p_occurrence = _;
          Lxsd.p_term = (Lxsd.T_ed _|Lxsd.T_w _) } ->
            (* case 4.2.3.3 *)
            sequence_of_particles mgds1 p_base p

let resolve_ct
    (type_table : (Lxsd.st_resolved, Lxsd.ct_resolved) Lxsd.td M_tds.t)
    (mgds : Lxsd.mgd Env.M_mgds.t)
    (ctd : Lxsd.ct_unresolved Lxsd.ctd) :
    Lxsd.ct_resolved Lxsd.ctd * Lxsd.mgd Env.M_mgds.t =
  match ctd with
    { Lxsd.ctd_au = agd_id_l;
      Lxsd.ctd_aw = w_opt;
      Lxsd.ctd_ct = ct } ->
        let dm,base,variety =
          match ct with
          | { Lxsd.ctd_base_un = Lxsd.CTD_anyType_un;
              Lxsd.ctd_dm_un = dm;
              Lxsd.ctd_variety_un = v } ->
                dm,Lxsd.CT_anyType,v
          | { Lxsd.ctd_base_un = Lxsd.CTD_td_ref_un td_id;
              Lxsd.ctd_dm_un = dm;
              Lxsd.ctd_variety_un = v } ->
                dm, (get_pre_ct td_id type_table), v
          | { Lxsd.ctd_base_un = Lxsd.CTD_simpleContentType_un;
              Lxsd.ctd_dm_un = dm;
              Lxsd.ctd_variety_un = (Lxsd.CT_simple_un td_id) as v } ->
                let base =
                  match get_pre_ct td_id type_table with
                  | Lxsd.CT_simple _ as ct -> ct
                  | Lxsd.CT_anyType
                  | Lxsd.CT_empty
                  | Lxsd.CT_element_only _
                  | Lxsd.CT_mixed _ ->
                      Message.error
                        (Error.A_function "Xsd.lineariser.resolve_ct")
                in
                dm,base,v
          | { Lxsd.ctd_base_un = Lxsd.CTD_simpleContentType_un;
              Lxsd.ctd_dm_un = _;
              Lxsd.ctd_variety_un =
              (Lxsd.CT_empty_un
              |Lxsd.CT_element_only_un _
              |Lxsd.CT_mixed_un _) } ->
                Message.error
                    (Error.A_function "Xsd.lineariser.resolve_ct")
        in
        begin
          match dm with
          | Lxsd.Extension_un ->
              let ct',mgds' =
                match base,variety with
                | (* base *) Lxsd.CT_anyType,
                  _ ->
                    (* extending anyType seems permitted but looks
                       risky as the resulting type is anyType itself *)
                    Message.error Error.XSDL_extension_of_anytype_risky
                | (* base *) Lxsd.CT_empty,
                  Lxsd.CT_empty_un ->
                    Lxsd.CT_empty,mgds
                | (* base *) Lxsd.CT_empty,
                    Lxsd.CT_element_only_un p ->
                      (* TODO (XSD validation) we should check if the
                         particle is effectively composed of elements *)
                      (Lxsd.CT_element_only p),mgds
                | (* base *) Lxsd.CT_empty,
                      Lxsd.CT_mixed_un p ->
                        (* TODO (XSD validation) we should check if the
                           particle is effectively mixed *)
                        (Lxsd.CT_mixed p),mgds
                | (* base *) Lxsd.CT_simple (__td_id_1),
                      Lxsd.CT_simple_un td_id_2 ->
                        (* TODO (XSD validation) we should check if the
                           two simple types are equals *)
                        (Lxsd.CT_simple td_id_2),mgds
                | (* base *) Lxsd.CT_element_only p_base,
                      Lxsd.CT_element_only_un p ->
                        let p',mgds' = extend_particles false mgds p_base p in
                        (Lxsd.CT_element_only p'),mgds'
                | (* base *) Lxsd.CT_mixed p_base,
                      Lxsd.CT_mixed_un p ->
                        let p',mgds' = extend_particles true mgds p_base p in
                        (Lxsd.CT_mixed p'),mgds'
                | (* base *) Lxsd.CT_empty,
                  Lxsd.CT_simple_un _
                | (* base *) Lxsd.CT_simple _,
                  (Lxsd.CT_empty_un
                  |Lxsd.CT_mixed_un _
                  |Lxsd.CT_element_only_un _)
                | (* base *) Lxsd.CT_element_only _,
                  (Lxsd.CT_empty_un
                  |Lxsd.CT_simple_un _
                  |Lxsd.CT_mixed_un _)
                | (* base *) Lxsd.CT_mixed _,
                  (Lxsd.CT_empty_un
                  |Lxsd.CT_simple_un _
                  |Lxsd.CT_element_only_un _) ->
                      Message.error
                        Error.XSDL_complex_type_extension_invalid
              in
              { Lxsd.ctd_au = agd_id_l;
                Lxsd.ctd_aw = w_opt;
                Lxsd.ctd_ct = ct' },
              mgds'
          | Lxsd.Restriction_un ->
              let ct' =
                match base,variety with
                | (* base *) Lxsd.CT_anyType,
                  Lxsd.CT_empty_un ->
                    Lxsd.CT_empty
                | (* base *) Lxsd.CT_anyType,
                  Lxsd.CT_simple_un td_id ->
                    Lxsd.CT_simple td_id
                | (* base *) Lxsd.CT_anyType,
                  Lxsd.CT_element_only_un p ->
                    Lxsd.CT_element_only p
                | (* base *) Lxsd.CT_anyType,
                  Lxsd.CT_mixed_un p ->
                    Lxsd.CT_mixed p
                | (* base *) Lxsd.CT_empty,
                  Lxsd.CT_empty_un ->
                    Lxsd.CT_empty
                | (* base *) (Lxsd.CT_element_only _ | Lxsd.CT_mixed _),
                  Lxsd.CT_empty_un ->
                    (* TODO (XSD validation) we should check if the
                     base type is emptyable *)
                    Lxsd.CT_empty
                | (* base *) (Lxsd.CT_simple _),
                  Lxsd.CT_simple_un td_id ->
                    (* TODO (XSD validation) the dependency between
                       simple types should be included by {!get_tds} *)
                    Lxsd.CT_simple td_id
                | (* base *) (Lxsd.CT_mixed _),
                  Lxsd.CT_simple_un td_id ->
                    (* TODO (XSD validation) we should check if the
                       base type's elements are emptyable *)
                    Lxsd.CT_simple td_id
                | (* base *) (Lxsd.CT_element_only _|Lxsd.CT_mixed _),
                  Lxsd.CT_element_only_un p ->
                    (* TODO (XSD validation) we should check if the
                       current particle restricts the base one *)
                    Lxsd.CT_element_only p
                | (* base *) (Lxsd.CT_mixed _),
                  Lxsd.CT_mixed_un p ->
                    (* TODO (XSD validation) we should check if the
                       current particle restricts the base one *)
                    Lxsd.CT_mixed p
                | (* base *) (Lxsd.CT_simple _),
                    Lxsd.CT_empty_un
                | (* base *) (Lxsd.CT_empty
                             |Lxsd.CT_element_only _),
                    Lxsd.CT_simple_un _
                | (* base *) (Lxsd.CT_empty
                             |Lxsd.CT_simple _),
                    Lxsd.CT_element_only_un _
                | (* base *) (Lxsd.CT_empty
                             |Lxsd.CT_simple _
                             |Lxsd.CT_element_only _),
                    Lxsd.CT_mixed_un _ ->
                      Message.error
                        Error.XSDL_complex_type_restriction_invalid
              in
              { Lxsd.ctd_au = agd_id_l;
                Lxsd.ctd_aw = w_opt;
                Lxsd.ctd_ct = ct' },
              (* No extra model group added *)
              mgds
        end




(** Resolve type definition inheritance dependencies. *)
let resolve_td
    (_,td_id)
    (primitive_type,td)
    (type_table,mgds) =
  let (td' : (Lxsd.st_resolved,Lxsd.ct_resolved) Lxsd.td),
    mgds' =
    match td with
    | Lxsd.STD
        { Lxsd.std_details = details } ->
            let details' =
              resolve_st type_table primitive_type details in
            Lxsd.STD { Lxsd.std_details = details' },
            mgds
    | Lxsd.CTD ctd ->
        let ctd',mgds' =
          resolve_ct type_table mgds ctd in
        (Lxsd.CTD ctd'),mgds'
  in
  (M_tds.add td_id td' type_table),mgds'



let resolve_tds
    (type_table :
       ('a * (Lxsd.st_unresolved,Lxsd.ct_unresolved) Lxsd.td)
       Env.M_tds_order.t)
    (mgds : Lxsd.mgd Env.M_mgds.t) :
    (Lxsd.st_resolved,Lxsd.ct_resolved) Lxsd.td M_tds.t *
    Lxsd.mgd Env.M_mgds.t =
  Env.M_tds_order.fold
    resolve_td
    type_table
    (M_tds.empty,mgds)

(** {2 Attribute use inheritance} *)

module S_agd_id =
  Set.Make (struct type t = Lxsd.agd_id let compare = compare end)

let rec transitive_closure m1 =
  let m2 =
    Env.M_agds.fold
      (fun id s m ->
        let s' =
          S_agd_id.fold
            (fun s_id s_s ->
              S_agd_id.union
                s_s
                (try Env.M_agds.find s_id m with
                | Not_found -> S_agd_id.empty) )
            s
            s
        in
        Env.M_agds.add id (S_agd_id.remove id s') m)
      m1
      m1
  in
  if Env.M_agds.equal (fun s1 s2 -> S_agd_id.equal s1 s2) m1 m2
  then m2
  else transitive_closure m2

let resolve_au (agds : Lxsd.au_unresolved Lxsd.agd Env.M_agds.t) :
    (Lxsd.agd_id * Lxsd.au_resolved Lxsd.agd) list
 =
  let initial_deps =
    Env.M_agds.map
      (function
          { Lxsd.agd_uses = (Lxsd.AU_unresolved (_,agd_id_l));
            Lxsd.agd_w = _ } ->
        List.fold_left
          (fun s agd_id ->
            S_agd_id.add agd_id s)
          S_agd_id.empty
          agd_id_l)
      agds in
  let deps = transitive_closure initial_deps in
  let initial_au_l =
    Env.M_agds.map
      (function
          { Lxsd.agd_uses = (Lxsd.AU_unresolved (au_l,_));
            Lxsd.agd_w = _ } ->
              au_l)
      agds in
  let resolved_agds =
    Env.M_agds.mapi
      (fun
        agd_id
          { Lxsd.agd_uses = _;
            Lxsd.agd_w = w_opt } ->
              let s =
                S_agd_id.add
                  agd_id
                  (try Env.M_agds.find agd_id deps with
                  | Not_found -> S_agd_id.empty)
              in
              let au_l =
                S_agd_id.fold
                  (fun id l ->
                    let aus =
                      try Env.M_agds.find id initial_au_l with
                      | Not_found -> [] in
                    l @ aus
                  )
                  s
                  []
              in
              { Lxsd.agd_uses = Lxsd.AU_resolved au_l;
                Lxsd.agd_w = w_opt })
      agds in
  Env.M_agds.bindings resolved_agds



(** Return the list of particles composing an all-model group. *)
let rec flatten_all_mg_particle_list mgds acc = function
  | [] -> acc
  | h::t ->
      match h with
      | { Lxsd.p_occurrence = _;
          Lxsd.p_term = (Lxsd.T_ed _|Lxsd.T_w _) } ->
            flatten_all_mg_particle_list mgds (h::acc) t
      | { Lxsd.p_occurrence =
          { Lxsd.min_occurs = 1;
            Lxsd.max_occurs = Lxsd.Occur 1 };
          Lxsd.p_term = Lxsd.T_mgd mgd_id } ->
            begin
              try
                begin
                  match Env.M_mgds.find mgd_id mgds with
                  | Lxsd.MG_all p_l ->
                      flatten_all_mg_particle_list mgds acc (p_l @ t)
                  | Lxsd.MG_choice _
                  | Lxsd.MG_sequence _ ->
                      Message.error (* TODO add localisation *)
                        Error.XSDL_all_model_not_well_formed
                end
              with
              | Not_found ->
                  Message.error
                    (Error.A_function
                       "Xsd_lineariser.flatten_all_mg_particle_list")
            end
      | { Lxsd.p_occurrence =
          { Lxsd.min_occurs = _;
            Lxsd.max_occurs = (Lxsd.Occur _|Lxsd.Unbounded) };
          Lxsd.p_term = Lxsd.T_mgd _ } ->
            Message.error (* TODO add localisation *)
              Error.XSDL_all_model_not_well_formed


(** Flatten the all-model groups of a table of model group definitions
    and check whether they are well formed that is to say ``Group
    references are now allowed in <xs:all> model groups. Such
    references must have minOccurs=maxOccurs=1 and must refer to other
    <xs:all> groups.''
    {{:http://www.w3.org/TR/2011/CR-xmlschema11-1-20110721/#ch_models}REF}
 *)
let flatten_all_model_groups mgds : Lxsd.mgd Env.M_mgds.t =
  Env.M_mgds.map
    (fun v ->
      match v with
      | Lxsd.MG_choice _
      | Lxsd.MG_sequence _ -> v
      | Lxsd.MG_all p_l ->
          let p_l' =
            flatten_all_mg_particle_list mgds [] p_l in
          if List.length
              (List.filter
                 (fun { Lxsd.p_occurrence = _;
                        Lxsd.p_term = t } ->
                   (match t with
                   | Lxsd.T_w _ -> true
                   | Lxsd.T_ed _ | Lxsd.T_mgd _ -> false))
                 p_l') > 1
          then
            Message.error
              Error.XSDL_XSV_LIM_more_than_one_wildcard_in_all_model
          else
            Lxsd.MG_all p_l')
    mgds

(** {2 XSD linearisation main function} *)

(** Linearise XSD and determinism test. *)
let linearise schema =
  let env1 =
    Env.empty in
  let lxsd =
    match schema with
    | { Xsd.schema_form = Xsd.Inlined;
        Xsd.target = target;
        Xsd.definitions = definitions } ->
          let env2,uri_id = Env.new_uri target env1 in
          let env_final =
            List.fold_left
              linearise_definition
              env2
              definitions
          in
          let uris = Env.get_uris env_final in
          let ad_missing,ads = Env.get_ads env_final in
          let ed_missing,eds = Env.get_eds env_final in
          let agd_missing,agds = Env.get_agds env_final in
          let mgd_missing,explicit_mgds = Env.get_mgds env_final in
          if
            ad_missing || ed_missing ||
            agd_missing || mgd_missing
          then
            Message.error Error.XSDL_missing_definition_s;
          (* [get_tds] emits its own [Error.XSDL_missing_definition_s] *)
          let type_table = Env.get_tds env_final in
          let roots = Env.get_roots env_final in
          (* We propagate the inheritance of facets *)
          let resolved_tds,extended_mgds =
            resolve_tds type_table explicit_mgds in
          let extended_allflattened_mgds =
            flatten_all_model_groups extended_mgds in
          let tds =
            M_tds.bindings resolved_tds in
          let mgds =
            Env.M_mgds.bindings extended_allflattened_mgds in
          (* We propagate the inheritance of attribute use *)
          let resolved_agds =
              (resolve_au agds) in
          { Lxsd.target = uri_id;
            Lxsd.uris = uris;
            Lxsd.tds = tds;
            Lxsd.ads = ads;
            Lxsd.eds = eds;
            Lxsd.agds = resolved_agds;
            Lxsd.mgds = mgds;
            Lxsd.roots = roots }
  in
  if test_determinism lxsd
  then
    lxsd
  else
    Message.error Error.XSDL_non_deterministic_grammar
