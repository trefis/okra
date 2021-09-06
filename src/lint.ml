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

let fail_fmt_patterns =
  [ 
    (Str.regexp ".*\t"), "Tab found. Use spaces for indentation (2 preferred).";
    (Str.regexp ".*-  "), "Double space before text after bullet point ('-  text'), this can confuse the parser. Use '- text'";
    (Str.regexp "^ -"), "Single space used for indentation (' - text'). Remove or replace by 2 or more spaces.";
    (Str.regexp "^[ ]*\\*"), "* used as bullet point, this can confuse the parser. Only use - as bullet marker.";
    (Str.regexp "^[ ]*\\+"), "+ used as bullet point, this can confuse the parser. Only use - as bullet marker.";
    (Str.regexp "^[ ]+#"), "Space found before title marker #. Start titles in first column.";
  ]

let lint ?(include_sections=[]) ?(ignore_sections=[]) ic =
  let failures = ref 0 in 
  let rec check_and_read buf ic pos =
    (try
      let line = input_line ic in
      List.iter
        (fun (regexp, msg) ->
          if Str.string_match regexp line 0 then (
            Printf.printf "Line %n: %s\n" pos msg;
            failures := !failures + 1)
          else ()
        ) fail_fmt_patterns;
      Buffer.add_string buf line;
      Buffer.add_string buf "\n";
      check_and_read buf ic (pos + 1)
    with
    | End_of_file -> Buffer.contents buf
    | e -> raise e)
  in 
  let s = check_and_read (Buffer.create 1024) ic 1 in
  if (!failures > 0) then
    false
  else (
    (* parse and without output to sanity check *)
    (try
      let md = Omd.of_string s in
      let okrs = Aggregate.process ~include_sections:include_sections ~ignore_sections:ignore_sections md in
      let _ = List.map Aggregate.of_weekly (List.of_seq (Hashtbl.to_seq_values okrs)) in
      true
    with
    | Aggregate.No_time_found s -> Printf.printf "No time entry found. Each KR must be followed by '- @... (x days)\nError: %s\n" s; false
    | Aggregate.Invalid_time s -> Printf.printf "Invalid time entry found. Format is '- @eng1 (x days), @eng2 (x days)'\nError: %s\n" s; false
    | Aggregate.Multiple_time_entries s -> Printf.printf "KR with multiple time entries found. Only one time entry should follow immediately after the KR.\nError: %s\n" s; false
    | Aggregate.No_work_found s -> Printf.printf "No work items found. This may indicate an unreported parsing error. Remove the KR if it is without work.\nError: %s\n" s; false)
  )
  (* read into buffer while checking formatting, then send final string to omd for parsing and sanity checks *)
