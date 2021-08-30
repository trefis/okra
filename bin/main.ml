(*
 * Copyright (c) 2021 Magnus Skjegstad
 * Copyright (c) 2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Okra

let () =
  let md = Omd.of_channel stdin in
  let okrs = Okra.process md in

  (*Hashtbl.iter (fun _ v ->
    print_okrs v) store.okr_ht*)
  let v = List.map Okra.of_weekly (List.of_seq (Hashtbl.to_seq_values okrs)) in
  let c_project = ref "" in
  let c_objective = ref "" in
  let c_kr_id = ref "" in
  let c_kr_title = ref "" in
  List.iter
    (fun e ->
      if e.project <> !c_project then (
        Printf.printf "\n# %s\n" e.project;
        c_project := e.project)
      else ();
      if e.objective <> !c_objective then (
        Printf.printf "\n## %s\n" e.objective;
        c_objective := e.objective)
      else ();
      if e.kr_id <> !c_kr_id || e.kr_title <> !c_kr_title then (
        Printf.printf "\n- %s (%s)\n" e.kr_title e.kr_id;
        c_kr_title := e.kr_title;
        c_kr_id := e.kr_id)
      else ();
      List.iter (fun s -> Printf.printf "    - + %s" s) e.time_entries;
      Printf.printf "    - = ";
      Hashtbl.iter
        (fun s v -> Printf.printf "@%s (%.2f days) " s v)
        e.time_per_engineer;
      Printf.printf "\n";
      List.iter (fun s -> Printf.printf "    - %s" s) e.work)
    (List.sort Okra.compare v)
