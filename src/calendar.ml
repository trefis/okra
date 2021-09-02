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

module Cal = CalendarLib.Calendar

type t = { week : int; year : int }

let week { week; _ } = week
let year { year; _ } = year
let day = 60. *. 60. *. 24.
let make ~week ~year = { week; year }

let this_week () =
  let now = Cal.now () in
  { week = Cal.week now; year = Cal.year now }

let github_week t =
  let monday, sunday = Cal.Date.week_first_last t.week t.year in
  let sunday =
    (* Some people might work on the sunday... *)
    Cal.Date.to_unixfloat sunday +. (day -. 1.) |> Cal.from_unixfloat
  in
  ( Cal.Date.to_unixfloat monday |> Get_activity.Period.to_8601,
    Cal.to_unixfloat sunday |> Get_activity.Period.to_8601 )
