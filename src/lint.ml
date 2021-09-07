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

type lint_result =
  | No_error
  | Format_error of (int * string) list
  | No_time_found of string
  | Invalid_time of string
  | Multiple_time_entries of string
  | No_work_found of string

let fail_fmt_patterns =
  [
    (Str.regexp ".*\t", "Tab found. Use spaces for indentation (2 preferred).");
    ( Str.regexp ".*-  ",
      "Double space before text after bullet point ('-  text'), this can \
       confuse the parser. Use '- text'" );
    ( Str.regexp "^ -",
      "Single space used for indentation (' - text'). Remove or replace by 2 \
       or more spaces." );
    ( Str.regexp "^[ ]*\\*",
      "* used as bullet point, this can confuse the parser. Only use - as \
       bullet marker." );
    ( Str.regexp "^[ ]*\\+",
      "+ used as bullet point, this can confuse the parser. Only use - as \
       bullet marker." );
    ( Str.regexp "^[ ]+#",
      "Space found before title marker #. Start titles in first column." );
  ]

let string_of_result res =
  let buf = Buffer.create 16 in
  (match res with
  | No_error -> Buffer.add_string buf "No error\n"
  | Format_error x ->
      List.iter
        (fun (pos, msg) ->
          Buffer.add_string buf (Fmt.str "Line %d: %s\n" pos msg))
        x;
      Buffer.add_string buf
        (Fmt.str "%d formatting errors found. Parsing aborted.\n"
           (List.length x))
  | No_time_found s ->
      Buffer.add_string buf
        (Fmt.str
           "No time entry found. Each KR must be followed by '- @... (x days)\n\
            Error: %s\n"
           s)
  | Invalid_time s ->
      Buffer.add_string buf
        (Fmt.str
           "Invalid time entry found. Format is '- @eng1 (x days), @eng2 (x \
            days)'\n\
            Error: %s\n"
           s)
  | Multiple_time_entries s ->
      Buffer.add_string buf
        (Fmt.str
           "KR with multiple time entries found. Only one time entry should \
            follow immediately after the KR.\n\
            Error: %s\n"
           s)
  | No_work_found s ->
      Buffer.add_string buf
        (Fmt.str
           "No work items found. This may indicate an unreported parsing \
            error. Remove the KR if it is without work.\n\
            Error: %s\n"
           s));
  Buffer.contents buf

let lint ?(include_sections = []) ?(ignore_sections = []) ic =
  let format_errors = ref [] in
  let rec check_and_read buf ic pos =
    try
      let line = input_line ic in
      List.iter
        (fun (regexp, msg) ->
          if Str.string_match regexp line 0 then
            format_errors := (pos, msg) :: !format_errors
          else ())
        fail_fmt_patterns;
      Buffer.add_string buf line;
      Buffer.add_string buf "\n";
      check_and_read buf ic (pos + 1)
    with
    | End_of_file -> Buffer.contents buf
    | e -> raise e
  in
  let s = check_and_read (Buffer.create 1024) ic 1 in
  if List.length !format_errors > 0 then
    Format_error (List.sort (fun (x, _) (y, _) -> compare x y) !format_errors)
  else
    (* parse and without output to sanity check *)
    try
      let md = Omd.of_string s in
      let okrs = Aggregate.process ~include_sections ~ignore_sections md in
      let _ =
        List.map Aggregate.of_weekly (List.of_seq (Hashtbl.to_seq_values okrs))
      in
      No_error
    with
    | Aggregate.No_time_found s -> No_time_found s
    | Aggregate.Invalid_time s -> Invalid_time s
    | Aggregate.Multiple_time_entries s -> Multiple_time_entries s
    | Aggregate.No_work_found s -> No_work_found s
