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

(** [report_team_md okrs] outputs a team report to stdout. 

[ignore_sections] ignores a list of subsections in the report. By default this
is set to "OKR updates".

[include_krs] only includes this list of KR IDs. Note that this will ignore
empty KR IDs or KRs marked as "NEW KR" unless specified in the list. If the
list is empty, all KRs are returned.

When [show_time_calc] is set, an extra line will be added to the output each
time the same entry is included in the report with a sum at the end. This is
useful for showing the intermediate steps when aggreating multiple reports that
contain the same KR.

Example output with [show_time_calc] enabled:

- This is a KR (KR ID)
  + @engineer1 (1 day), @engineer2 (1 day)
  + @engineer1 (1 day)
  = @engineer1 (2 days), @engineer2 (1 day)
  - Work item
  ...

[show_time] shows the time entries
[show_engineers] shows the list of engineers

*)
let report_team_md ?ignore_sections:(ignore_sections=["OKR updates"])
		   ?include_krs:(include_krs=[])
		   ?show_time:(show_time=true)
  		   ?show_time_calc:(show_time_calc=true)
                   ?show_engineers:(show_engineers=true)
		   okrs =
  let v = List.map Okra.of_weekly (List.of_seq (Hashtbl.to_seq_values okrs)) in
  let uppercase_include_krs = (List.map String.uppercase_ascii include_krs) in
  let uppercase_ignore_sections = (List.map String.uppercase_ascii ignore_sections) in
  let c_project = ref "" in
  let c_objective = ref "" in
  let c_kr_id = ref "" in
  let c_kr_title = ref "" in
  List.iter
    (fun e ->
      (* only proceed if include_krs is empty or has a match, and ignore_sections is empty or without match *)
      if ((List.length include_krs == 0 || List.mem e.kr_id uppercase_include_krs)) &&
	 ((List.length ignore_sections == 0 || not (List.mem (String.uppercase_ascii e.project) uppercase_ignore_sections))) then (
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
	      if show_engineers then (
		      if show_time then (
			      if show_time_calc then ( (* show time calc + engineers *)
				      List.iter (fun s -> Printf.printf "    - + %s" s) e.time_entries;
				      Printf.printf "    - = ";
				      Hashtbl.iter
					(fun s v -> Printf.printf "@%s (%.2f days) " s v)
					e.time_per_engineer;
				      Printf.printf "\n")
			      else ( (* show total time for each engineer *)
				      Printf.printf "    - ";
				      Hashtbl.iter
					(fun s v -> Printf.printf "@%s (%.2f days) " s v)
					e.time_per_engineer;
				      Printf.printf "\n"))
		      else ( (* only show engineers, no time *)
			      Printf.printf "    - ";
			      Hashtbl.iter
				(fun s _ -> Printf.printf "@%s " s)
				e.time_per_engineer;
			      Printf.printf "\n"))
	      else (); (* don't show time or engineers *)
	      List.iter (fun s -> Printf.printf "    - %s" s) e.work)
      else () (* skip this KR *)
    )
    (List.sort Okra.compare v)

let () =
  let md = Omd.of_channel stdin in
  let okrs = Okra.process md in
  report_team_md okrs
