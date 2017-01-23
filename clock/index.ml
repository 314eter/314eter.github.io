open React

module T = Tyxml_js.Html
module R = Tyxml_js.R.Html

let time, set_time = S.create (new%js Js.date_now)
let hours = S.map (fun date -> date##getHours) time
let minutes = S.map (fun date -> date##getMinutes) time
let finish_hours, set_fhours = S.create (S.value hours)
let finish_minutes, set_fminutes = S.create (S.value minutes)

let overtime =
  let open S.Compare in
  let open S.Bool in
  hours >= finish_hours && minutes >= finish_minutes

let flash, set_flash = S.create false

let time_hours = S.map string_of_int hours
let time_minutes = S.map (Printf.sprintf "%02d") minutes
let%html.T div_time = "<div class='time'>" [R.pcdata time_hours; T.pcdata ":"; R.pcdata time_minutes] "</div>"

let%html.T input_hours = "<span contenteditable=true>" [T.pcdata (S.value time_hours)] "</span>"
let%html.T input_minutes = "<span contenteditable=true>" [T.pcdata (S.value time_minutes)] "</span>"
let%html.T div_finish = "<div class='finish'>" [input_hours; T.pcdata ":"; input_minutes] "</div>"

let div_clock =
  let clock_class = S.map (function true -> ["flash"] | false -> []) S.Bool.(overtime && flash) in
  T.div ~a:[T.a_id "clock"; R.a_class clock_class] [div_time; div_finish]

let register elt set () =
  Lwt_js_events.inputs (Tyxml_js.To_dom.of_span elt) @@ fun event _ ->
  Lwt.return @@ Js.Opt.iter event##.target @@ fun target ->
  Js.Opt.iter target##.textContent @@ fun text ->
  Js.to_string text |> int_of_string |> set

let rec loop () =
  let%lwt () = Lwt_js.sleep 1.0 in
  set_time (new%js Js.date_now);
  set_flash (not (S.value flash));
  loop ()

let () =
  Tyxml_js.Register.id "content" [div_clock];
  Lwt_js_events.async (register input_hours set_fhours);
  Lwt_js_events.async (register input_minutes set_fminutes);
  Lwt_js_events.async loop
