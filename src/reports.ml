(*
 * Copyright (c) 2021 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Aggregate

let pp_days ppf d =
  if d = 1. then Fmt.string ppf "1 day" else Fmt.pf ppf "%.0f days" d

let pp ?(include_krs = []) ?(show_time = true) ?(show_time_calc = true)
    ?(show_engineers = true) ppf okrs =
  let v =
    List.map Aggregate.of_weekly (List.of_seq (Hashtbl.to_seq_values okrs))
  in
  let uppercase_include_krs = List.map String.uppercase_ascii include_krs in
  let c_project = ref "" in
  let c_objective = ref "" in
  let c_kr_id = ref "" in
  let c_kr_title = ref "" in
  List.iter
    (fun e ->
      (* only proceed if include_krs is empty or has a match *)
      if List.length include_krs = 0 || List.mem e.kr_id uppercase_include_krs
      then (
        if e.project <> !c_project then (
          Fmt.pf ppf "\n# %s\n" e.project;
          c_project := e.project)
        else ();
        if e.objective <> !c_objective then (
          Fmt.pf ppf "\n## %s\n" e.objective;
          c_objective := e.objective)
        else ();
        if e.kr_id <> !c_kr_id || e.kr_title <> !c_kr_title then (
          Fmt.pf ppf "\n- %s (%s)\n" e.kr_title e.kr_id;
          c_kr_title := e.kr_title;
          c_kr_id := e.kr_id)
        else ();
        if show_engineers then
          if show_time then
            if show_time_calc then (
              (* show time calc + engineers *)
              List.iter (Fmt.pf ppf "  - + %s") e.time_entries;
              Fmt.pf ppf "  - = ";
              Hashtbl.iter
                (fun e d -> Fmt.pf ppf "@%s (%a) " e pp_days d)
                e.time_per_engineer;
              Fmt.pf ppf "\n")
            else (
              (* show total time for each engineer *)
              Fmt.pf ppf "  - ";
              let first = ref true in
              Hashtbl.iter
                (fun s v ->
                  if not !first then Fmt.pf ppf ", " else first := false;
                  Fmt.pf ppf "@%s (%a)" s pp_days v)
                e.time_per_engineer;
              Fmt.pf ppf "\n")
          else (
            (* only show engineers, no time *)
            Hashtbl.iter
              (fun s _ -> Fmt.pf ppf "  - @%s " s)
              e.time_per_engineer;
            Fmt.pf ppf "\n")
        else ();
        (* don't show time or engineers *)
        List.iter
          (fun lines ->
            let work = String.concat "\n    " lines in
            Fmt.pf ppf "  - %s\n" work)
          e.work)
      else () (* skip this KR *))
    (List.sort Aggregate.compare v)
