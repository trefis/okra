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

let run conf =
  let success = ref true in
  if List.length conf.files > 0 then (
    List.iter
      (fun f ->  
        let ic = open_in f in
        try
          let res = Okra.Lint.lint ~include_sections:conf.include_sections ic in
          if (not res) then success := res else ();
          close_in ic;
        with e->
          close_in_noerr ic;
          Printf.fprintf stderr "Caught error while linting:\n\n";
          raise e
      ) conf.files)
  else (
    try
      let res = Okra.Lint.lint ~include_sections:conf.include_sections stdin in
      if (not res) then success := res else ()
    with e->
      Printf.fprintf stderr "Caught error while linting:\n\n";
      raise e
  );
  if not !success then (
    exit 1)
  else ()

let term =
  let lint include_sections files =
    let conf =
      {
        include_sections;
        files;
      }
    in
    run conf
  in
  Term.(
    const lint
    $ include_sections_term
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
