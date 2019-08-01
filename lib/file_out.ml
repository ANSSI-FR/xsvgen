(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: file_out.ml 1686 2012-05-30 09:00:22Z maarek $ *)

(** {1 File handling library} *)

(** File extensions. *)
type extension =
  | ML
  | Sh
  | Log

(** Output name. *)
type t =
  | Out of string * bool (** name and debug mode *)

(** Output name for which the preparation has been performed. *)
type prepared =
  | Out_prepared of string

(** Give the suffix corresponding to the extension. *)
let suffix = function
  | ML -> ".ml"
  | Sh -> ".sh"
  | Log -> ".log"

(** Name of the generator library directory. *)
let generator_lib_dir = "xsvgen_lib"


(** Return an output name value from a given debug mode and a given
    name.
  *)
let of_string debug s =
  let out_s =
    if debug
    then s ^ "-debug"
    else s
  in
  if
    not (Filename.is_implicit out_s) ||
    out_s = generator_lib_dir ||
    out_s = Sys.executable_name
  then
    raise (Error.E Error.F_wrong_output_basename)
  else
    Out (out_s,debug)

(** Perform a preparation of the output directory, for a given
    name "xsval":
    - removes any existing directory named "xsval",
    - create a fresh "xsval" directory,
    - copy the validator common sources in "xsval/lib_src",
    - copy the version of "message.ml" accordingly to the mode
      (default or silent),
    - create a directory "xsval/val_src".
  *)
let prepare (Out (s,debug)) =
  if
    try Sys.is_directory generator_lib_dir with
    | Sys_error _ -> false
  then
    begin
      if
        ( Sys.command ("rm -fr " ^ s) <> 0
        || Sys.command ("mkdir " ^ s) <> 0
        || Sys.command ("cp -r " ^ generator_lib_dir
                        ^ " " ^ s ^ "/lib_src") <> 0
        || Sys.command ("cp " ^ generator_lib_dir ^ "/" ^
                        (if debug
                        then "message_debug.ml"
                        else "message_silent.ml") ^ " " ^ s ^
                        "/lib_src/message.ml") <> 0
        || Sys.command ("rm " ^ s ^ "/lib_src/message_debug.ml" ^
                        " " ^ s ^ "/lib_src/message_silent.ml") <> 0
        || Sys.command ("mkdir " ^ s ^ "/val_src") <> 0
        || Sys.command ("mv " ^ s ^ "/lib_src/xsval.mli " ^
                        s ^ "/val_src") <> 0)
      then raise (Error.E Error.IO_output_dir)
      else (Out_prepared s)
    end
  else raise (Error.E Error.IO_lib_dir_missing)

(** Record the opened output channels and close the recoreded openend
    output channels.
    close_opened_out_files:
    @raise Error.E [Error.IO_open_out_file_fail]
*)
let add_opened_out_file,close_opened_files =
  let oc_opened_out_files = ref [] in
  begin
    fun (oc,n) ->
      oc_opened_out_files := (oc,n) :: !oc_opened_out_files
  end,
  begin
    fun () ->
      List.iter
        (fun (oc,n) ->
          try close_out oc with
          | Sys_error _ ->
              raise (Error.E (Error.IO_open_out_file_fail n)))
        !oc_opened_out_files
  end

(** Return a formatter for the output file name corresponding to the
    given output name and the given extension.

   Normally Format.formatter_of_out_channel should not cause any
   exception, as we create a supposedly correct out_channel (else it
   raises Sys_error) in [out_to_formatter]
   @raise Error.E [Error.F_wrong_output_file_extension]
   @raise Invalid_argument [(string)]
   @raise Failure [(string)]
   @raise Error.E [Error.IO_open_out_file_fail]
*)
let to_formatter (Out_prepared out_basename) extension =
  let file_name =
    match extension with
    | ML ->
        Format.sprintf "%s%s%s"
          out_basename
          "/val_src/xsval"
          (suffix extension)
    | Sh | Log ->
        Format.sprintf "%s%s%s"
          out_basename
          "/xsval"
          (suffix extension)
  in
  let out_channel =
    try open_out file_name with
    | Sys_error _ ->
        raise (Error.E (Error.IO_open_out_file_fail file_name))
  in
  add_opened_out_file (out_channel,file_name);
  Format.formatter_of_out_channel out_channel


(** Print to a given formatter the output executable file name
    corresponding to the given output name.  *)
let fprintf f (Out_prepared basename) =
  Format.fprintf f "%s"
    (Filename.quote (basename ^ ".bin"))
