(*
 * Copyright (c) 2021 Patrick Ferris <pf341@patricoferris.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t = { projects : string list; locations : string list }

let default = { projects = []; locations = [] }
let conf_err s = Error (`Msg (Fmt.str "Okra Conf Error: %s" s))

let of_yaml yaml =
  let open Rresult in
  let open Yaml.Util in
  let map_option f = function
    | None -> Ok [ "TODO ADD KR (ID)" ]
    | Some (`A v) -> (
        try Ok (List.map f v) with Yaml.Util.Value_error s -> conf_err s)
    | Some _ -> conf_err "Expected a list"
  in
  find "projects" yaml >>= map_option to_string_exn >>= fun projects ->
  find "locations" yaml >>= map_option to_string_exn >>= fun locations ->
  Ok { projects; locations }

let projects { projects; _ } = projects
let locations { locations; _ } = locations

let load file =
  let open Rresult in
  Bos.OS.File.read (Fpath.v file) >>= fun contents ->
  Yaml.of_string contents >>= fun yaml -> of_yaml yaml

let home =
  match Sys.getenv_opt "HOME" with
  | None -> Fmt.failwith "$HOME is not set!"
  | Some dir -> dir

let default_okra_file =
  let ( / ) = Filename.concat in
  home / ".okra" / "conf.yaml"

open Cmdliner

let cmdliner =
  Arg.value
  @@ Arg.opt Arg.file default_okra_file
  @@ Arg.info ~doc:"Okra configuration file" ~docv:"CONF" [ "conf" ]
