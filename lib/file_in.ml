(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: file_in.ml 1499 2012-05-02 09:20:22Z maarek $ *)

(** {1 File handling library} *)

(** File extensions. *)
type extension =
  | ML
  | Bin
  | Xsd
  | Xml

(** Input file type. *)
type t =
  | In of string * extension

(** Give the suffix corresponding to the extension. *)
let suffix = function
  | ML -> ".ml"
  | Bin -> ".bin"
  | Xsd -> ".xsd"
  | Xml -> ".xml"

(** Test if each extension of a given list of input extensions is either
    XML or XSD. *)
let test_in_extension_l l =
  List.for_all
    (function
      | Xml | Xsd -> true
      | ML | Bin -> false)
    l


(** Get the extension corresponding to a given file name from a list
    of possible extensions.
    @raise Error.E [Error.F_wrong_input_extension] *)
let get_extension s extension_l =
  try
    List.find
      (fun extension ->
        Filename.check_suffix s (suffix extension))
      extension_l
  with
    Not_found ->
      raise (Error.E Error.F_wrong_input_extension)


(** Return an input file value out a given string and a set of
    acceptable extensions.
   @raise Error.E [Error.F_wrong_input_extension]
   @raise Error.E [Error.F_wrong_input_file]
*)
let of_string s extension_l =
  if not (test_in_extension_l extension_l)
  then
    raise (Error.E Error.F_wrong_input_extension)
  else
    if not (Filename.is_implicit s)
    then
      raise (Error.E Error.F_wrong_input_file)
    else
      let extension = get_extension  s extension_l in
      In (s, extension)


(** Return the file name corresponding to an input file value.
   @raise Error.E [Error.F_wrong_printing_file_extension] *)
let to_string (In (s,_)) =
  Filename.quote s


(** Record the opened input channels and close the recoreded openend
    input channels.
   close_opened_in_files:
   @raise Error.E [Error.IO_open_in_file_fail]
*)
let add_opened_file,close_opened_files =
  let ic_opened_in_files = ref [] in
  begin
    fun (ic,n) ->
      ic_opened_in_files := (ic,n) :: !ic_opened_in_files
  end,
  begin
    fun () ->
      List.iter
        (fun (ic,n) ->
          try
            close_in ic
          with
          | Sys_error _ ->
              raise (Error.E (Error.IO_open_in_file_fail n)))
        !ic_opened_in_files
  end


(** Return an input channel from an input file value.
   @raise Error.E [Error.F_wrong_input_extension]
   @raise Error.E [Error.IO_open_in_file_fail]
*)
let to_in_channel (In (s,in_ext)) extension_l =
  if not (List.mem in_ext extension_l)
  then
    raise (Error.E Error.F_wrong_input_extension)
  else
    let in_channel =
      try open_in_bin s with
      | Sys_error _ ->
          raise (Error.E (Error.IO_open_in_file_fail s))
    in
    add_opened_file (in_channel,s);
    in_channel











