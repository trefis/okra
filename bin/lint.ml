(*
 * Copyright (c) 2021 Magnus Skjegstad <magnus@skjegstad.com>
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

type t = {
  include_sections : string list;
  ignore_sections : string list;
  files : string list;
}

open Cmdliner

let files_term =
  Arg.(value & (pos_all non_dir_file []) & info [] ~docv:"FILE")

let include_sections_term =
  let info =
    Arg.info [ "include-sections" ]
      ~doc:
        "If non-empty, only lint entries under these sections - everything else is ignored."
  in
  Arg.value (Arg.opt (Arg.list Arg.string) [] info)

let ignore_sections_term =
  let info =
    Arg.info [ "ignore-sections" ]
      ~doc:
        "If non-empty, don't lint entries under the specified sections."
  in
  Arg.value (Arg.opt (Arg.list Arg.string) [] info)

let engineer_term =
  let info =
    Arg.info [ "engineer"; "e" ]
      ~doc:
        "Lint an engineer report. This is an alias for --include-sections=\"last week\", --ignore-sections=\"\""
  in
  Arg.value (Arg.flag info)

let team_term =
  let info =
    Arg.info [ "team"; "t" ]
      ~doc:
        "Lint a team report. This is an alias for --include-sections=\"\", --ignore-sections=\"OKR updates\""
  in
  Arg.value (Arg.flag info)

let run conf =
  let success = ref true in
  if List.length conf.files > 0 then (
    List.iter
      (fun f ->  
        let ic = open_in f in
        try
          let res = Okra.Lint.lint ~include_sections:conf.include_sections ~ignore_sections:conf.ignore_sections ic in
          if (not res) then success := res else ();
          close_in ic;
        with e->
          close_in_noerr ic;
          Printf.fprintf stderr "Caught error while linting:\n\n";
          raise e
      ) conf.files)
  else (
    try
      let res = Okra.Lint.lint ~include_sections:conf.include_sections ~ignore_sections:conf.ignore_sections stdin in
      if (not res) then success := res else ()
    with e->
      Printf.fprintf stderr "Caught error while linting:\n\n";
      raise e
  );
  if not !success then (
    exit 1)
  else ()

let term =
  let lint include_sections ignore_sections engineer team files =
    let conf =
      if engineer then (
        {
          include_sections = ["Last week"];
          ignore_sections = [];
          files;
        })
      else
      if team then (
        {
          include_sections;
          ignore_sections = ["OKR updates"];
          files;
        })
      else (
          {
            include_sections;
            ignore_sections;
            files;
          })
    in
    run conf
  in
  Term.(
    const lint
    $ include_sections_term
    $ ignore_sections_term
    $ engineer_term
    $ team_term
    $ files_term)

let cmd =
  let info =
    Term.info "lint" ~doc:"Check for formatting errors and missing information in the report"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Check for general formatting errors, then attempt to parse the report and look for inconsistencies.";
          `P
            "Reads from stdin if no files are specified.";
        ]
  in
  (term, info)
